{-# LANGUAGE OverloadedStrings #-}
module Main where

import Development.Shake ( shakeArgs
                         , shakeOptions
                         , shakeFiles
                         , shakeProgress
                         , progressSimple)

import Project.Libraries 
import Project.Types
import Project.Defaults
import Project.Internal

main :: IO ()
main = do
  let
    (Right target) = targetFile "hello-world"
    cfg            = defaultATSConfig target
  shakeArgs shakeOptions { shakeFiles    = (fixedTextToString . atsBuildDir $ cfg)
                                , shakeProgress = progressSimple}
                   (atsProjectBuilder cfg)


