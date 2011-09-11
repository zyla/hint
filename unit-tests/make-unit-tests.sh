if [ $# -gt 0 ]; then
  SUFFIX="-$1"
  shift
fi

GHC=ghc$SUFFIX
GHC_PKG=ghc-pkg$SUFFIX

echo "Using $($GHC --version)"

HINT_INSTALLED=$($GHC_PKG describe hint > /dev/null 2>&1; echo $?)
if [ "$HINT_INSTALLED" == "0" ]; then
  DISABLE_HINT="-hide-package hint"
fi

$GHC --make run-unit-tests.hs \
  -fforce-recomp \
  -Wall \
  -i../src \
  -odir . -hidir . \
  -package ghc \
  -cpp \
  -XGeneralizedNewtypeDeriving \
  -XMultiParamTypeClasses \
  -XDeriveDataTypeable \
  -XMagicHash \
  -XTypeSynonymInstances \
  -XFlexibleInstances \
  -XFlexibleContexts \
  -XFunctionalDependencies \
  -XKindSignatures \
  -XRank2Types \
  -XScopedTypeVariables \
  -XExistentialQuantification \
  -XPatternGuards \
  $DISABLE_HINT
