{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeInType #-}

{- |
Module      : Project.Types
Description : Text and Intermediate types for project
Copyright   : Plow Technologies LLC
License     : MIT License

Maintainer  : Scott Murphy

There are several file types here.
They are all instances of semigroup.
The instances check to make sure the constraints are still valid.
If not, they will only return the first of the two objects
| -}


module Project.Types  ( TargetFile
                        ,  SourceFiles
                         , DatsFile
                         , SatsFile
                         , HatsFile
                         , CFile
                         , Flag
                         , Version
                         , BuildDir
                         , ObjFile
                         , SourceFile     (..)
                         , CommandRec     (..)
                         , AtsBuildConfig (..)

                         , datsFile
                         , satsFile
                         , hatsFile
                         , objFile
                         , targetFile
                         , buildDir
                         , workingDir
                         , cfile
                         , flag
                         , versionText


                         , defaultATSConfig
                         , defaultTargetDestination
                         , defaultBuildDirectory
                         , emptyWorkingDirectory) where

import Data.Text         ( Text)
import Data.Monoid       ( (<>))
import Development.Shake ( CmdOption)
import Control.Applicative
import Project.Internal  ( FixedTextErrors

                         , FixedText
                         , fixedTextFromText)

import Project.Internal (unsafeFixedTextFromText)
import Development.Shake.FilePath ( (</>)
                                  , FilePath)
import Data.Aeson
import Data.Either (rights)

--------------------------------------------------
-- File Extensions
--------------------------------------------------

-- | File that ends in .dats
type DatsFile = FixedText 100 6 ".+?\\.dats$"

datsFile :: Text -> Either FixedTextErrors DatsFile
datsFile = fixedTextFromText


-- | File that ends in .sats
type SatsFile = FixedText 100 6 ".+?\\.sats$"

satsFile :: Text -> Either FixedTextErrors SatsFile
satsFile = fixedTextFromText

-- | File that ends in .hats
type HatsFile = FixedText 100 6 ".+?\\.hats$"

hatsFile :: Text -> Either FixedTextErrors HatsFile
hatsFile = fixedTextFromText

-- | C Files
type CFile = FixedText 100 3 ".+?\\.c$"

cfile :: Text -> Either FixedTextErrors CFile
cfile = fixedTextFromText

-- | Object File
type ObjFile = FixedText 100 3 ".+?\\.o$"

objFile :: Text -> Either FixedTextErrors ObjFile
objFile = fixedTextFromText

-- | Target file
type TargetFile = FixedText 100 1 "[[:alnum:]_.-]"

targetFile :: Text -> Either FixedTextErrors TargetFile
targetFile = fixedTextFromText

-- | Version 
type Version = FixedText 100 1 "[[:digit:]\\.-vV\\*_()]"
versionText :: Text -> Either FixedTextErrors Version
versionText = fixedTextFromText

-- | length limited flags
type Flag = FixedText 100 0 "."

flag :: Text -> Either FixedTextErrors Flag
flag = fixedTextFromText


-- | Usually ats-work, the directory files are built in 
type BuildDir = FixedText 100 2 "[[:alnum:]_.-]"


buildDir :: Text -> Either FixedTextErrors BuildDir
buildDir = fixedTextFromText



-- | The root directory of the project
--   If you are using relative directories, this is
--   where your relative point should be
--
--   Notice: This directory is always discovered and
--   not supplied by the yaml.  
type WorkingDir = FixedText 100 0 "[[:alnum:]_.-]"

workingDir :: Text -> Either FixedTextErrors WorkingDir
workingDir = fixedTextFromText


-- | All Source Files
-- Notice, the lack of Obj file here
data SourceFile = SourceFileDats DatsFile
                | SourceFileSats SatsFile
                | SourceFileHats HatsFile
                | SourceFileC    CFile
  deriving (Show,Eq,Ord)



type SourceFiles = [SourceFile]





-- | Command Record
-- data structure to output
-- the arguments of a command execution in a neat way


data CommandRec = CommandRec {
  recCmd         :: String,
  recCmdOptions  :: [CmdOption],
  recCmdArgs     :: [String]
             }
 deriving (Eq,Show,Ord)


--------------------------------------------------
-- Config Type
--------------------------------------------------

data AtsBuildConfig = AtsBuildConfig {
     atsHome        :: FilePath   ,     
     atsSourceDir   :: FilePath   ,
     atsCC          :: FilePath   ,
     atsOpt         :: FilePath   ,
     atsFlags       :: [Flag]     ,
     atsBuildDir    :: BuildDir   ,
     atsSourceFiles :: SourceFiles ,
     atsProjectVersion :: Version ,
     atsWorkingDir  :: WorkingDir ,
     atsTarget      :: TargetFile 


     }
 deriving (Show,Eq,Ord)



--------------------------------------------------
-- Aeson instances
--------------------------------------------------

-- | the fromjson instance uses a defaultATS to catch any
--   fields not supplied by the yaml.
--
--   Except for the target field which must be supplied
instance FromJSON AtsBuildConfig where
  parseJSON (Object o) = do
       target <- o .: "ats-target"
       
       let AtsBuildConfig {..} = defaultATSConfig target
       AtsBuildConfig <$>
             (o .: "ats-home"         <|> pure atsHome)         
         <*> (o .: "ats-source-dir"   <|> pure atsSourceDir)   
         <*> (o .: "ats-cc"           <|> pure atsCC)           
         <*> (o .: "ats-opt"          <|> pure atsOpt)
         <*> (o .: "ats-flags"        <|> pure atsFlags)        
         <*> (o .: "ats-build-dir"    <|> pure atsBuildDir)    
         <*> (o .: "ats-source-files" <|> pure atsSourceFiles) 
         <*> (o .: "ats-project-version" <|> pure atsProjectVersion)
         <*> (pure emptyWorkingDirectory)
         <*> pure target


  parseJSON _ = fail "AtsBuildConfig expecting Object"

instance ToJSON AtsBuildConfig where
  toJSON (AtsBuildConfig {..}) = object [
       "ats-home"         .= atsHome
     , "ats-source-dir"   .= atsSourceDir
     , "ats-cc"           .= atsCC
     , "ats-opt"          .= atsOpt
     , "ats-flags"        .= atsFlags
     , "ats-build-dir"    .= atsBuildDir
     , "ats-source-files" .= atsSourceFiles
     , "ats-project-version" .= atsProjectVersion]



-- | "ex: <filename>.dats"
instance FromJSON SourceFile where
  parseJSON (String txt) =
    let oneOfTheFiles =  (SourceFileDats <$> (datsFile txt))  `alt`
                         (SourceFileSats <$> (satsFile txt))  `alt`
                         (SourceFileHats <$> (hatsFile txt))  `alt`
                         (SourceFileC    <$> (cfile txt)   )  

    in case oneOfTheFiles of
         (Right val) -> return val
         (Left  err)  -> fail $ show err

  parseJSON _         = fail "Rule: text expected for SourceFile"

instance ToJSON SourceFile where
  toJSON (SourceFileDats f) = toJSON f 
  toJSON (SourceFileSats f) = toJSON f
  toJSON (SourceFileHats f) = toJSON f
  toJSON (SourceFileC    f) = toJSON f
  
--------------------------------------------------
-- Helper Functions for this module
--------------------------------------------------

-- | The typeclass instance for Either wasn't quite right
--   for the alternative I wanted so here we are...
alt :: Monoid a => Either a b -> Either a b -> Either a b
alt ea eb = either (nextEither eb) Right ea
  where
   nextEither (Left errB) errA = Left (errA <> errB)
   nextEither eB         _    = eB

--------------------------------------------------
-- Defaults 
--------------------------------------------------

-- | This is the default set of options to build an 
--   ats compiled binary.
defaultATSConfig :: TargetFile -> AtsBuildConfig
defaultATSConfig target = AtsBuildConfig { 
     atsHome           = "/usr/local"
   , atsBuildDir       = defaultBuildDirectory
   , atsSourceDir      = "src"
   , atsCC             = "/usr/local/bin/patscc"
   , atsOpt            = "/usr/local/bin/patsopt"
   , atsFlags          = rights $ flag <$> ["-O2", "-DATS_MEMALLOC_LIBC"]     
   , atsSourceFiles    = mempty
   , atsProjectVersion = ver
   , atsWorkingDir     = emptyWorkingDirectory
   , atsTarget         = target  }
  where
    (Right ver) = versionText "0.0.1"


-- | final target is built in target
defaultTargetDestination :: FilePath
defaultTargetDestination = "dist" </> "build"


-- | Providing good defaults
defaultBuildDirectory :: BuildDir
defaultBuildDirectory = unsafeFixedTextFromText "defaultBuildDirectory: " "ats-work"


emptyWorkingDirectory :: WorkingDir
emptyWorkingDirectory = unsafeFixedTextFromText "Working Directory Starts Empty: " ""
