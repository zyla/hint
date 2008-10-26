module Hint.GHC (
    module GHC,
    module Outputable,
    module ErrUtils,
    module Pretty,
    module DriverPhases,
    module StringBuffer,

#if   __GLASGOW_HASKELL__ >= 610
    module HscTypes
#elif __GLASGOW_HASKELL__ >= 608
    module PprTyThing,
#elif __GLASGOW_HASKELL__ < 608
    module SrcLoc,
#endif
)

where

import GHC hiding ( Phase )
import Outputable ( PprStyle, ppr,
                    showSDoc, showSDocForUser, showSDocUnqual,
                    withPprStyle, defaultErrStyle )
import ErrUtils ( Message, mkLocMessage  )
import Pretty ( Doc )
import DriverPhases ( Phase(Cpp), HscSource(HsSrcFile) )
import StringBuffer (stringToStringBuffer)

#if   __GLASGOW_HASKELL__ >= 610
import HscTypes ( Session )
#elif __GLASGOW_HASKELL__ >= 608
import PprTyThing( pprTypeForUser )
#elif __GLASGOW_HASKELL__ < 608
import SrcLoc ( SrcSpan, noSrcLoc )
#endif
