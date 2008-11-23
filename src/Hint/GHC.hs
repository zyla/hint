module Hint.GHC (
    module GHC,
    module Outputable,
    module ErrUtils,
    module Pretty,
    module DriverPhases,
    module StringBuffer,
    module Lexer,
    module Parser,
#if __GLASGOW_HASKELL__ >= 610
    module Control.Monad.Ghc,
#endif
#if __GLASGOW_HASKELL__ >= 608
    module PprTyThing,
#elif __GLASGOW_HASKELL__ < 608
    module SrcLoc,
#endif
)

where
#if __GLASGOW_HASKELL__ >= 610
import GHC hiding ( Phase, GhcT, runGhcT )
import Control.Monad.Ghc ( GhcT, runGhcT )
#else
import GHC hiding ( Phase )
#endif

import Outputable   ( PprStyle, ppr,
                      showSDoc, showSDocForUser, showSDocUnqual,
                      withPprStyle, defaultErrStyle )
import ErrUtils     ( Message, mkLocMessage  )
import Pretty       ( Doc )
import DriverPhases ( Phase(Cpp), HscSource(HsSrcFile) )
import StringBuffer ( stringToStringBuffer )
import Lexer        ( P(..), ParseResult(..), mkPState )
import Parser       ( parseStmt, parseType )

#if __GLASGOW_HASKELL__ >= 608
import PprTyThing   ( pprTypeForUser )
#elif __GLASGOW_HASKELL__ < 608
import SrcLoc       ( SrcSpan, noSrcLoc )
#endif
