module Language.Haskell.Interpreter.GHC.Parsers

where

import qualified GHC(Session, getSessionDynFlags)

import qualified Lexer        as GHC.L (P(..), ParseResult(..), mkPState)
import qualified Parser       as GHC.P (parseStmt)
import qualified StringBuffer as GHC.SB(stringToStringBuffer)
import qualified SrcLoc       as GHC.S (SrcSpan, noSrcLoc)
import qualified ErrUtils     as GHC.E (Message)

data ParseResult = ParseOk | ParseError GHC.S.SrcSpan GHC.E.Message

parseExpr :: GHC.Session -> String -> IO ParseResult
parseExpr ghc_session expr =
    do
        dyn_fl <- GHC.getSessionDynFlags ghc_session
        --
        buf <- GHC.SB.stringToStringBuffer expr
        --
        let parse_res = GHC.L.unP GHC.P.parseStmt (GHC.L.mkPState buf GHC.S.noSrcLoc dyn_fl)
        --
        case parse_res of
            GHC.L.POk{}            -> return ParseOk
            --
            GHC.L.PFailed span err -> return (ParseError span err)