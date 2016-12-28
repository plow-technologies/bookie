{-# LANGUAGE TemplateHaskell #-}
{- |
Module      : Project.Lens
Description : Lenses for this build system
Copyright   : Plow Technologies LLC
License     : MIT License

Maintainer  : Scott Murphy


Lenses for build system.

| -}



module Project.Lens where


import Project.Types
import Control.Lens


makeLensesFor [("atsSourceFiles","sourceFiles")] ''AtsBuildConfig

