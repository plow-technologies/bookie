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

module Project.Defaults ( defaultATSConfigStorable
                        , defaultTargetDestination
                        , defaultBuildDirectory) where



-- import Project.Internal (unsafeFixedTextFromText)



import Project.Types ( defaultATSConfigStorable
                     , defaultTargetDestination
                     , defaultBuildDirectory)









