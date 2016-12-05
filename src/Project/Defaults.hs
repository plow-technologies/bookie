{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Project.Defaults
Description : Default Settings and files for ats build
Copyright   : Plow Technologies LLC
License     : MIT License

Maintainer  : Scott Murphy

Currently, the only defaults are defined in the Types directory because they ahve to be
used in the instances for To and From JSON..
| -}

module Project.Defaults ( defaultATSConfig
                        , defaultTargetDestination
                        , defaultWorkingDirectory) where

import Data.Either      (rights)

import Project.Internal (unsafeFixedTextFromText)

import Development.Shake.FilePath ( (</>)
                                  , FilePath)

import Project.Types ( defaultATSConfig
                     , defaultTargetDestination
                     , defaultWorkingDirectory)









