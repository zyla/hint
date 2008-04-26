{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Interpreter.GHC
-- License     :  BSD-style
--
-- Maintainer  :  jcpetruzza@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (GHC API)
--
-- A Haskell interpreter built on top of the GHC API
-----------------------------------------------------------------------------
module Language.Haskell.Interpreter.GHC(
    -- * Session handling
     InterpreterSession, newSession, newSessionUsing,
    -- * Error handling
     InterpreterError(..), GhcError(..),
    -- * The interpreter type
     Interpreter,
    -- ** Running the interpreter
     withSession,
    -- ** Interpreter options
     setUseLanguageExtensions,
    -- ** Context handling
     ModuleName,
     loadModules, getLoadedModules, setTopLevelModules,
     setImports,
     reset,
    -- ** Module querying
     ModuleElem(..), Id, name, children,
     getModuleExports,
    -- ** Type inference
     typeOf, typeChecks, kindOf,
    -- ** Evaluation
     interpret, as, infer,
     eval)

where

import Prelude hiding ( span )

import qualified GHC
import qualified Outputable as GHC.O
import qualified SrcLoc     as GHC.S
import qualified ErrUtils   as GHC.E
import qualified Name       as GHC.N

import qualified GHC.Exts ( unsafeCoerce# )

import Control.Monad        ( liftM, filterM, guard, when )
import Control.Monad.Trans  ( liftIO )
import Control.Monad.Error  ( MonadError(throwError, catchError) )


import Control.Exception ( Exception(DynException), tryJust, throwDyn )

import Data.Typeable           ( Typeable, TypeRep, mkTyCon,
                                 mkTyConApp, splitTyConApp )
import qualified Data.Typeable ( typeOf )
import Data.Dynamic            ( fromDynamic )

import Data.List  ( (\\) )
import Data.Maybe ( catMaybes )

import Language.Haskell.Interpreter.GHC.Base

import qualified Language.Haskell.Interpreter.GHC.Compat as Compat

import Language.Haskell.Interpreter.GHC.Parsers     ( ParseResult(..),
                                                      parseExpr, parseType )
import Language.Haskell.Interpreter.GHC.Conversions ( FromGhcRep(..) )


-- | Set to true to allow GHC's extensions to Haskell 98.
setUseLanguageExtensions :: Bool -> Interpreter ()
setUseLanguageExtensions val =
    do
        ghc_session <- fromSessionState ghcSession
        --
        let negate_or_not = if val then "" else "no-"
        let flag = concat ["-f", negate_or_not, "glasgow-exts"]
        --
        old_flags               <- liftIO $ GHC.getSessionDynFlags ghc_session
        (new_flags, not_parsed) <- liftIO $ GHC.parseDynamicFlags old_flags
                                                                  [flag]
        --
        when (not . null $ not_parsed) $
            throwError $ UnknownError (concat ["flag: '", flag,
                                                           "' not recognized"])
        --
        liftIO $ GHC.setSessionDynFlags ghc_session new_flags
        --
        return ()

-- | Module names are _not_ filepaths.
type ModuleName = String

-- | An Id for a class, a type constructor, a data constructor, a binding, etc
type Id = String

data ModuleElem = Fun Id | Class Id [Id] | Data Id [Id]
  deriving (Read, Show, Eq)

name :: ModuleElem -> Id
name (Fun f)     = f
name (Class c _) = c
name (Data d _)  = d

children :: ModuleElem -> [Id]
children (Fun   _)     = []
children (Class _ ms)  = ms
children (Data  _ dcs) = dcs

-- | Tries to load all the requested modules from their source file.
--   Modules my be indicated by their ModuleName (e.g. \"My.Module\") or
--   by the full path to its source file.
--
-- The interpreter is 'reset' both before loading the modules and in the event
-- of an error.
loadModules :: [String] -> Interpreter ()
loadModules fs =
    do
        ghc_session <- fromSessionState ghcSession
        --
        -- first, unload everything
        reset
        --
        let doLoad = mayFail $ do
            targets <- mapM (\f -> GHC.guessTarget f Nothing) fs
            --
            GHC.setTargets ghc_session targets
            res <- GHC.load ghc_session GHC.LoadAllTargets
            return $ guard (isSucceeded res) >> Just ()
        --
        doLoad `catchError` (\e -> reset >> throwError e)
        --
        return ()

-- | Returns the list of modules loaded with 'loadModules'.
getLoadedModules :: Interpreter [ModuleName]
getLoadedModules = liftM (map modNameFromSummary) getLoadedModSummaries

modNameFromSummary :: GHC.ModSummary -> ModuleName
modNameFromSummary =  modNameFromModule . GHC.ms_mod

modNameFromModule :: GHC.Module -> ModuleName
modNameFromModule = GHC.moduleNameString . GHC.moduleName

getLoadedModSummaries :: Interpreter [GHC.ModSummary]
getLoadedModSummaries =
  do ghc_session  <- fromSessionState ghcSession
     --
     all_mod_summ <- liftIO $ GHC.getModuleGraph ghc_session
     filterM (liftIO . GHC.isLoaded ghc_session . GHC.ms_mod_name) all_mod_summ

-- | Sets the modules whose context is used during evaluation. All bindings
--   of these modules are in scope, not only those exported.
--
--   Modules must be interpreted to use this function.
setTopLevelModules :: [ModuleName] -> Interpreter ()
setTopLevelModules ms =
    do
        ghc_session <- fromSessionState ghcSession
        --
        loaded_mods_ghc <- getLoadedModSummaries
        --
        let not_loaded = ms \\ map modNameFromSummary loaded_mods_ghc
        when (not . null $ not_loaded) $
            throwError $ NotAllowed ("These modules have not been loaded:\n" ++
                                     unlines not_loaded)
        --
        ms_mods <- mapM findModule ms
        --
        let mod_is_interpr = GHC.moduleIsInterpreted ghc_session
        not_interpreted <- liftIO $ filterM (liftM not . mod_is_interpr) ms_mods
        when (not . null $ not_interpreted) $
            throwError $ NotAllowed ("These modules are not interpreted:\n" ++
                                     unlines (map modNameFromModule
                                                  not_interpreted))
        --
        liftIO $ do
            (_, old_imports) <- GHC.getContext ghc_session
            GHC.setContext ghc_session ms_mods old_imports

-- | Gets an abstract representation of all the entities exported by the module.
--   It is similar to the @:browse@ command in GHCi. 
getModuleExports :: ModuleName -> Interpreter [ModuleElem]
getModuleExports mn =
    do
        ghc_session <- fromSessionState ghcSession
        --
        module_  <- findModule mn
        mod_info <- mayFail $ GHC.getModuleInfo ghc_session module_
        exports  <- liftIO $ mapM (GHC.lookupName ghc_session)
                                  (GHC.modInfoExports mod_info)
        --
        return (asModElemList $ catMaybes exports)

asModElemList :: [GHC.TyThing] -> [ModuleElem]
asModElemList xs = concat [cs',
                           ts',
                           ds \\ (concatMap (map Fun . children) ts'),
                           fs \\ (concatMap (map Fun . children) cs')]
    where (cs,ts,ds,fs) = ([asModElem c | c@GHC.AClass{}   <- xs],
                           [asModElem t | t@GHC.ATyCon{}   <- xs],
                           [asModElem d | d@GHC.ADataCon{} <- xs],
                           [asModElem f | f@GHC.AnId{}     <- xs])
          cs' = [Class n $ filter (alsoIn fs) ms  | Class n ms  <- cs]
          ts' = [Data  t $ filter (alsoIn ds) dcs | Data  t dcs <- ts]
          alsoIn es = (`elem` (map name es))


asModElem :: GHC.TyThing -> ModuleElem
asModElem (GHC.AnId f)      = Fun $ getUnqualName f
asModElem (GHC.ADataCon dc) = Fun $ getUnqualName dc
asModElem (GHC.ATyCon tc)   = Data  (getUnqualName tc)
                                    (map getUnqualName $ GHC.tyConDataCons tc)
asModElem (GHC.AClass c)    = Class (getUnqualName c)
                                    (map getUnqualName $ GHC.classMethods c)

getUnqualName :: GHC.NamedThing a => a -> String
getUnqualName = GHC.O.showSDocUnqual . GHC.pprParenSymName

findModule :: ModuleName -> Interpreter GHC.Module
findModule mn =
    do
        ghc_session <- fromSessionState ghcSession
        --
        let mod_name = GHC.mkModuleName mn
        mapGhcExceptions NotAllowed $ GHC.findModule ghc_session
                                                     mod_name
                                                     Nothing

mapGhcExceptions :: (String -> InterpreterError) -> IO a -> Interpreter a
mapGhcExceptions buildEx action =
    do
        r <- liftIO $ tryJust ghcExceptions action
        either (throwError . buildEx . flip GHC.showGhcException []) return r

ghcExceptions :: Exception -> Maybe GHC.GhcException
ghcExceptions (DynException a) = fromDynamic a
ghcExceptions  _               = Nothing

-- | Sets the modules whose exports must be in context.
setImports :: [ModuleName] -> Interpreter ()
setImports ms =
    do
        ghc_session <- fromSessionState ghcSession
        --
        ms_mods <- mapM findModule ms
        --
        liftIO $ do
            (old_top_level, _) <- GHC.getContext ghc_session
            GHC.setContext ghc_session old_top_level ms_mods

-- | All imported modules are cleared from the context, and
--   loaded modules are unloaded. It is similar to a @:load@ in
--   GHCi, but observe that not even the Prelude will be in
--   context after a reset.
reset :: Interpreter ()
reset =
    do
        ghc_session <- fromSessionState ghcSession
        --
        -- Remove all modules from context
        liftIO $ GHC.setContext ghc_session [] []
        --
        -- Unload all previously loaded modules
        liftIO $ GHC.setTargets ghc_session []
        liftIO $ GHC.load ghc_session GHC.LoadAllTargets
        --
        -- At this point, GHCi would call rts_revertCAFs and
        -- reset the buffering of stdin, stdout and stderr.
        -- Should we do any of these?
        --
        -- liftIO $ rts_revertCAFs
        --
        return ()


-- | Returns a string representation of the type of the expression.
typeOf :: String -> Interpreter String
typeOf expr =
    do
        ghc_session <- fromSessionState ghcSession
        --
        -- First, make sure the expression has no syntax errors,
        -- for this is the only way we have to "intercept" this
        -- kind of errors
        failOnParseError parseExpr expr
        --
        ty <- mayFail $ GHC.exprType ghc_session expr
        --
        -- Unqualify necessary types (i.e., do not expose internals)
        unqual <- liftIO $ GHC.getPrintUnqual ghc_session
        return $ fromGhcRep (GHC.dropForAlls ty, unqual)

-- | Tests if the expression type checks.
typeChecks :: String -> Interpreter Bool
typeChecks expr = (typeOf expr >> return True)
                  `catchError`
                  onCompilationError (\_ -> return False)

-- | Returns a string representation of the kind of the type expression.
kindOf :: String -> Interpreter String
kindOf type_expr =
    do
        ghc_session <- fromSessionState ghcSession
        --
        -- First, make sure the expression has no syntax errors,
        -- for this is the only way we have to "intercept" this
        -- kind of errors
        failOnParseError parseType type_expr

        kind <- mayFail $ GHC.typeKind ghc_session type_expr
        --
        return $ fromGhcRep kind


-- | Convenience functions to be used with typeCheck to provide witnesses.
--   Example:
--
--   * @interpret \"head [True,False]\" (as :: Bool)@
--
--   * @interpret \"head $ map show [True,False]\" infer >>= flip interpret (as :: Bool)@
as, infer :: Typeable a => a
as    = undefined
infer = undefined

-- | Evaluates an expression, given a witness for its monomorphic type.
interpret :: Typeable a => String -> a -> Interpreter a
interpret expr witness =
    do
        ghc_session <- fromSessionState ghcSession
        --
        -- First, make sure the expression has no syntax errors,
        -- for this is the only way we have to "intercept" this
        -- kind of errors
        failOnParseError parseExpr expr
        --
        let expr_typesig = concat ["(", expr, ") :: ", show $ myTypeOf witness]
        expr_val <- mayFail $ GHC.compileExpr ghc_session expr_typesig
        --
        return (GHC.Exts.unsafeCoerce# expr_val :: a)

-- HACK! Allows evaluations even when the Prelude is not in scope
myTypeOf :: Typeable a => a -> TypeRep
myTypeOf a
    | type_of_a == type_of_string = qual_type_of_string
    | otherwise                   = type_of_a
    where type_of_a           = Data.Typeable.typeOf a
          type_of_string      = Data.Typeable.typeOf (undefined :: [Char])
          (list_ty_con, _)    = splitTyConApp type_of_string
          qual_type_of_string = mkTyConApp list_ty_con
                                        [mkTyConApp (mkTyCon "Prelude.Char") []]

-- | @eval expr@ will evaluate @show expr@.
--  It will succeed only if @expr@ has type t and there is a 'Show'
--  instance for t.
eval :: String -> Interpreter String
eval expr = interpret show_expr (as :: String)
    where show_expr = unwords ["Prelude.show", "(", expr, ") "]

mayFail :: IO (Maybe a) -> Interpreter a
mayFail ghc_action =
    do
        maybe_res <- liftIO ghc_action
        --
        es <- modifySessionState ghcErrListRef (const [])
        --
        case maybe_res of
            Nothing -> if null es
                         then throwError $ UnknownError "Got no error message"
                         else throwError $ WontCompile (reverse es)
            Just a  -> if null es
                         then return a
                         else fail "GHC reported errors and also gave a result!"

failOnParseError :: (GHC.Session -> String -> IO ParseResult)
                 -> String
                 -> Interpreter ()
failOnParseError parser expr =
    do
        ghc_session <- fromSessionState ghcSession
        --
        parsed <- liftIO $ parser ghc_session expr
        --
        -- If there was a parsing error, do the "standard" error reporting
        res <- case parsed of
                   ParseOk             -> return (Just ())
                   --
                   ParseError span err ->
                       do
                           -- parsing failed, so we report it just as all
                           -- other errors get reported....
                           logger <- fromSessionState ghcErrLogger
                           liftIO $ logger GHC.SevError
                                           span
                                           GHC.O.defaultErrStyle
                                           err
                           --
                           -- behave like the rest of the GHC API functions
                           -- do on error...
                           return Nothing
        --
        -- "may Have Already Failed", actually :)
        mayFail (return res)

isSucceeded :: GHC.SuccessFlag -> Bool
isSucceeded GHC.Succeeded = True
isSucceeded GHC.Failed    = False

onCompilationError :: ([GhcError] -> Interpreter a)
                   -> (InterpreterError -> Interpreter a)
onCompilationError recover =
    \interp_error -> case interp_error of
                       WontCompile errs -> recover errs
                       otherErr         -> throwError otherErr

-- SHOULD WE CALL THIS WHEN MODULES ARE LOADED / UNLOADED?
-- foreign import ccall "revertCAFs" rts_revertCAFs  :: IO ()
