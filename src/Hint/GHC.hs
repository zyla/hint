module Hint.GHC (
    Message, module X
) where

import GHC as X hiding (Phase, GhcT, runGhcT)
import Control.Monad.Ghc as X (GhcT, runGhcT)

import HscTypes as X (SourceError, srcErrorMessages, GhcApiError)

import Outputable as X (PprStyle, SDoc, Outputable(ppr),
                        showSDoc, showSDocForUser, showSDocUnqual,
                        withPprStyle, defaultErrStyle)

import ErrUtils as X (mkLocMessage, pprErrMsgBagWithLoc, MsgDoc) -- we alias MsgDoc as Message below

import DriverPhases as X (Phase(Cpp), HscSource(HsSrcFile))
import StringBuffer as X (stringToStringBuffer)
import Lexer as X (P(..), ParseResult(..), mkPState)
import Parser as X (parseStmt, parseType)
import FastString as X (fsLit)

#if __GLASGOW_HASKELL__ >= 710
import DynFlags as X (xFlags, xopt, LogAction, FlagSpec(..))
#else
import DynFlags as X (xFlags, xopt, LogAction)
#endif

import PprTyThing as X (pprTypeForUser)
import SrcLoc as X (mkRealSrcLoc)

#if __GLASGOW_HASKELL__ >= 708
import ConLike as X (ConLike(RealDataCon))
#endif

type Message = MsgDoc
