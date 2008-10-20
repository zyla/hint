module Hint.Parsers

where

import Prelude hiding(span)

import qualified Hint.GHC as GHC

import qualified Lexer        as GHC.L (P(..), ParseResult(..), mkPState)
import qualified Parser       as GHC.P (parseStmt, parseType)

data ParseResult = ParseOk | ParseError GHC.SrcSpan GHC.Message

parseExpr :: GHC.Session -> String -> IO ParseResult
parseExpr = runParser GHC.P.parseStmt

parseType :: GHC.Session -> String -> IO ParseResult
parseType = runParser GHC.P.parseType

runParser :: GHC.L.P a -> GHC.Session -> String -> IO ParseResult
runParser parser ghc_session expr =
    do
        dyn_fl <- GHC.getSessionDynFlags ghc_session
        --
        buf <- GHC.stringToStringBuffer expr
        --
        let parse_res = GHC.L.unP parser (GHC.L.mkPState buf GHC.noSrcLoc dyn_fl)
        --
        case parse_res of
            GHC.L.POk{}            -> return ParseOk
            --
            GHC.L.PFailed span err -> return (ParseError span err)
