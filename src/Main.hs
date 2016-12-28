{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Development.Shake ( shakeOptions
                         , shake
                         , ShakeOptions(..)
--                         , Verbosity(..)
                         , shakeFiles
                         , shakeProgress
                         , progressSimple)

import Options.Generic

import Project.Libraries 
import Project.Types
import Project.Internal
import Project.Initialize
import qualified Data.Yaml as Yaml

data Commands = Init {target :: Maybe Text}
              | Add  Text
              | InitWithQuery
              | Build

  deriving (Show,Eq,Ord,Generic,ParseRecord)




main :: IO ()
main = do
  rslt <- getRecord "Bookie Options"
  runBookie rslt

runBookie :: Commands -> IO ()
runBookie (Init Nothing)       = writeAtsSimple 
runBookie (Init (Just target')) = (writeAtsNoQuery `traverse` (targetFile target')) >>= print
runBookie InitWithQuery        = writeAts
runBookie Build                = main'
runBookie (Add txt)            = addSourceFile `traverse` (txtToSourceFile txt) >>= print


main' :: IO ()
main' = do
  putStrLn "build started Whoo HOO"
  (Right cfg) <- Yaml.decodeFileEither "bookie.yaml"
  shake shakeOptions { shakeFiles     = (fixedTextToString . atsBuildDir $ cfg)
--                     , shakeVerbosity = Diagnostic
                     , shakeProgress  = progressSimple}
                     (atsProjectBuilder cfg)


