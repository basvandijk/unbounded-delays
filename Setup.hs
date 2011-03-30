#! /usr/bin/env runhaskell

import Distribution.Simple                ( defaultMainWithHooks
                                          , simpleUserHooks
                                          , UserHooks(haddockHook)
                                          )
import Distribution.Simple.Setup          ( HaddockFlags )
import Distribution.Simple.Program        ( userSpecifyArgs )
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(withPrograms) )
import Distribution.PackageDescription    ( PackageDescription )

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks { haddockHook = haddockHook' }

-- Define __HADDOCK__ for CPP when running haddock.
haddockHook' :: PackageDescription -> LocalBuildInfo -> UserHooks -> HaddockFlags -> IO ()
haddockHook' pkg lbi = haddockHook simpleUserHooks pkg $ lbi { withPrograms = p }
    where
      p = userSpecifyArgs "haddock" ["--optghc=-D__HADDOCK__"] (withPrograms lbi)
