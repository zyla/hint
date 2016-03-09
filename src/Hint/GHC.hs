module Hint.GHC (
    module GHC,
    module Outputable,
    module ErrUtils, Message,
    module Pretty,
    module DriverPhases,
    module StringBuffer,
    module Lexer,
    module Parser,
    module DynFlags,
    module FastString,
    module Control.Monad.Ghc,
    module HscTypes,
    module Bag,
    module PprTyThing,
    module SrcLoc,
#if __GLASGOW_HASKELL__ >= 708
    module ConLike,
#endif
)

where
import GHC hiding ( Phase, GhcT, runGhcT )
import Control.Monad.Ghc ( GhcT, runGhcT )

import HscTypes ( SourceError, srcErrorMessages, GhcApiError )
import Bag ( bagToList )

import Outputable   ( PprStyle, SDoc, Outputable(ppr),
                      showSDoc, showSDocForUser, showSDocUnqual,
                      withPprStyle, defaultErrStyle )

import ErrUtils     ( mkLocMessage, pprErrMsgBagWithLoc, MsgDoc) -- we alias MsgDoc as Message below

import Pretty       ( Doc )
import DriverPhases ( Phase(Cpp), HscSource(HsSrcFile) )
import StringBuffer ( stringToStringBuffer )
import Lexer        ( P(..), ParseResult(..), mkPState )
import Parser       ( parseStmt, parseType )
import FastString   ( fsLit )

#if   __GLASGOW_HASKELL__ >= 710
import DynFlags     ( supportedLanguagesAndExtensions, xFlags, xopt, LogAction, FlagSpec(..) )
#else
import DynFlags     ( supportedLanguagesAndExtensions, xFlags, xopt, LogAction)
#endif

import PprTyThing   ( pprTypeForUser )
import SrcLoc       ( mkRealSrcLoc )

#if __GLASGOW_HASKELL__ >= 708
import ConLike      ( ConLike(RealDataCon) )
#endif

type Message = MsgDoc
