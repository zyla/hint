module Control.Monad.Ghc (
    Ghc, runGhc,
    GhcT, runGhcT,
    GHC.GhcMonad(..),
    module Control.Monad.Trans
) where

import qualified Control.Exception.Extensible as E

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import qualified Control.Monad.Trans as MTL

import Control.Monad.Catch

import qualified GHC ( runGhc, runGhcT )
import qualified MonadUtils as GHC
import qualified Exception  as GHC
import qualified GhcMonad   as GHC

import qualified DynFlags as GHC

newtype Ghc a = Ghc{ unGhc :: GHC.Ghc a }
    deriving (Functor
             ,Monad
             ,GHC.HasDynFlags
             ,GHC.ExceptionMonad
#if __GLASGOW_HASKELL__ < 708
             ,GHC.MonadIO
#else
             ,MTL.MonadIO
             ,Applicative
#endif
             ,GHC.GhcMonad)

#if __GLASGOW_HASKELL__ < 708
instance Applicative Ghc where
  pure  = return
  (<*>) = ap

instance MTL.MonadIO Ghc where
    liftIO = GHC.liftIO
#endif

instance MonadThrow Ghc where
    throwM  = liftIO . E.throwIO

instance MonadCatch Ghc where
    catch   = GHC.gcatch

instance MonadMask Ghc where
    -- @gmask@ is available...
    -- ...but it doesn't have a rank-n type like @mask@, so we need
    -- to use @Control.Exception.mask@ directly... (sigh)
    -- (this is type-directed, write only code)
    mask f = wrap $ \s ->
               mask $ \io_restore ->
                 unwrap (f $ \m -> (wrap $ \s' -> io_restore (unwrap m s'))) s
     where
        wrap   = Ghc . GHC.Ghc
        unwrap = GHC.unGhc . unGhc

    uninterruptibleMask = mask

runGhc :: Maybe FilePath -> Ghc a -> IO a
runGhc f (Ghc m) = GHC.runGhc f m

newtype GhcT m a = GhcT { unGhcT :: GHC.GhcT (MTLAdapter m) a }
                 deriving (Functor
                          ,Monad
                          ,GHC.HasDynFlags
                          )

instance (Functor m, Monad m) => Applicative (GhcT m) where
  pure  = return
  (<*>) = ap

runGhcT :: (Functor m, MonadIO m, MonadCatch m, MonadMask m) => Maybe FilePath -> GhcT m a -> m a
runGhcT f = unMTLA . GHC.runGhcT f . unGhcT

instance MTL.MonadTrans GhcT where
    lift = GhcT . GHC.liftGhcT . MTLAdapter

instance MTL.MonadIO m => MTL.MonadIO (GhcT m) where
    liftIO = GhcT . GHC.liftIO

#if __GLASGOW_HASKELL__ < 708
  -- ghc started using transformers at some point
instance MTL.MonadIO m => GHC.MonadIO (GhcT m) where
    liftIO = MTL.liftIO
#endif

instance MonadCatch m => MonadThrow (GhcT m) where
    throwM = lift . throwM

instance (MonadIO m,MonadCatch m, MonadMask m) => MonadCatch (GhcT m) where
    m `catch` f = GhcT ((unGhcT m) `GHC.gcatch` (unGhcT . f))

instance (MonadIO m, MonadMask m) => MonadMask (GhcT m) where
    mask f = wrap $ \s ->
               mask $ \io_restore ->
                 unwrap (f $ \m -> (wrap $ \s' -> io_restore (unwrap m s'))) s
      where
        wrap g   = GhcT $ GHC.GhcT $ \s -> MTLAdapter (g s)
        unwrap m = \s -> unMTLA ((GHC.unGhcT $ unGhcT $  m) s)

    uninterruptibleMask = mask

instance (MonadIO m, MonadCatch m, MonadMask m) => GHC.ExceptionMonad (GhcT m) where
    gcatch  = catch
    gmask f = mask (\x -> f x)

instance (Functor m, MonadIO m, MonadCatch m, MonadMask m) => GHC.GhcMonad (GhcT m) where
    getSession = GhcT GHC.getSession
    setSession = GhcT . GHC.setSession

-- | We use the 'MTLAdapter' to convert between similar classes
--   like 'MTL'''s 'MonadIO' and 'GHC'''s 'MonadIO'.
newtype MTLAdapter m a = MTLAdapter {unMTLA :: m a} deriving (Functor, Applicative, Monad)

instance MTL.MonadIO m => GHC.MonadIO (MTLAdapter m) where
    liftIO = MTLAdapter . MTL.liftIO

instance (MonadIO m, MonadCatch m, MonadMask m) => GHC.ExceptionMonad (MTLAdapter m) where
  m `gcatch` f = MTLAdapter $ (unMTLA m) `catch` (unMTLA . f)
  gmask io = MTLAdapter $ mask (\f -> unMTLA $ io (MTLAdapter . f . unMTLA))
