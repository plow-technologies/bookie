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
import Project.Execute
import qualified Data.Yaml as Yaml





data Commands = Init {target :: Maybe Text}
              | Exec {execTarget :: Text}
              | Add  Text
              | InitWithQuery
              | Build


  deriving (Show,Eq,Ord,Generic,ParseRecord)







main :: IO ()
main = do
  rslt <- getRecord "Bookie Options"
  runBookie rslt






runBookie :: Commands -> IO ()
runBookie (Exec execTarget')     = runTarget execTarget'
runBookie (Init Nothing)        = writeAtsSimple 
runBookie (Init (Just target')) = (writeAtsNoQuery `traverse` (targetFile target')) >>= print
runBookie InitWithQuery         = writeAts
runBookie Build                 = main'
runBookie (Add txt)             = addSourceFile `traverse` (txtToSourceFile txt) >>= print





main' :: IO ()
main' = do
  putStrLn "build started Whoo HOO"  
  (Right cfgStorable) <- Yaml.decodeFileEither "bookie.yaml"
  cfg         <- getWorkingDirAndConstructConfig cfgStorable
  shake shakeOptions { shakeFiles     = (fixedTextToString . atsBuildDir $ cfg)
                     , shakeProgress  = progressSimple}
                     (atsProjectBuilder cfg)




runTarget :: Text -> IO ()
runTarget execTarget' = do
  putStrLn "build started Whoo HOO"
  (Right cfgStorable) <- Yaml.decodeFileEither "bookie.yaml"
  cfg                 <- getWorkingDirAndConstructConfig cfgStorable
  executeCmd (atsBuildDir cfg) execTarget'
  return ()  
