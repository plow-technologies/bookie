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
                         , SourceDir
                         , BuildDir
                         , ExecDir
                         , WorkingDir
                         , ObjFile
                         , SourceFile     (..)
                         , CommandRec     (..)
                         , AtsBuildConfig (..)
                         , AtsBuildConfigStorable
                         , toAtsBuildConfigStorable
                         , fromAtsBuildConfigStorable
                         
                         , datsFile
                         , satsFile
                         , hatsFile
                         , objFile
                         , targetFile
                         , sourceDir
                         , buildDir
                         , workingDir
                         , cfile
                         , flag
                         , versionText


                         , defaultATSConfigStorable
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
type TargetFile = FixedText 100 1 "[[:alnum:]_.\\-]"

targetFile :: Text -> Either FixedTextErrors TargetFile
targetFile = fixedTextFromText

-- | Version 
type Version = FixedText 100 1 "[[:digit:]\\.\\-vV\\*_()]"
versionText :: Text -> Either FixedTextErrors Version
versionText = fixedTextFromText

-- | length limited flags
type Flag = FixedText 100 0 "."

flag :: Text -> Either FixedTextErrors Flag
flag = fixedTextFromText


-- | Usually src, the directory your sources live in
type SourceDir = FixedText 100 2 "[[:alnum:]_.\\-]"

sourceDir :: Text -> Either FixedTextErrors SourceDir
sourceDir = fixedTextFromText

unsafeSourceDir :: Text -> SourceDir 
unsafeSourceDir = unsafeFixedTextFromText "Source Dir: "


-- | Usually ats-work, the directory files are built in 
type BuildDir = FixedText 100 2 "[[:alnum:]_.\\-]"


-- | The full path of the exec, the directory files are built in 
type ExecDir = FixedText 100 2 "[[:alnum:]_.\\-]"


buildDir :: Text -> Either FixedTextErrors BuildDir
buildDir = fixedTextFromText



-- | The root directory of the project
--   If you are using relative directories, this is
--   where your relative point should be
--
--   Notice: This directory is always discovered and
--   not supplied by the yaml.  
type WorkingDir = FixedText 100 0 "[[:alnum:]_./\\-]"

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
-- | not all proper config fields are storable.
--   the working directory for instance is a field
--   that must be gathered at run time.
--   However, it is meant to be that everywhere in the project
--   that a build config is needed, only the AtsBuildConfig will
--   be used.
--   So the constructor for storable is not provided.
--   Instead use to and from Storable to take
--   an atsBuildConfig and convert it to a storable one.
data AtsBuildConfigStorable = AtsBuildConfigStorable {
     atsStoreHome           :: FilePath   ,
     atsStoreLibDir         :: FilePath   ,
     atsStoreSourceDir      :: SourceDir  ,
     atsStoreCC             :: FilePath   ,
     atsStoreOpt            :: FilePath   ,
     atsStoreFlags          :: [Flag]     ,
     atsStoreBuildDir       :: BuildDir   ,
     atsStoreSourceFiles    :: SourceFiles ,
     atsStoreProjectVersion :: Version ,
     atsStoreTarget         :: TargetFile 


     }
 deriving (Show,Eq,Ord)


data AtsBuildConfig = AtsBuildConfig {
     atsHome        :: FilePath    ,
     atsLibDir      :: FilePath    ,
     atsSourceDir   :: SourceDir   ,
     atsCC          :: FilePath    ,
     atsOpt         :: FilePath    ,
     atsFlags       :: [Flag]      ,
     atsBuildDir    :: BuildDir    ,
     atsSourceFiles :: SourceFiles ,
     atsProjectVersion :: Version  ,
     atsWorkingDir  :: WorkingDir  ,
     atsTarget      :: TargetFile 
     }
 deriving (Show,Eq,Ord)


-- | Convert a Build Config Storable to a build config
--   non-storable
--   every field in storable is in AtsbuildConfig
toAtsBuildConfigStorable  :: AtsBuildConfig -> AtsBuildConfigStorable
toAtsBuildConfigStorable (AtsBuildConfig {..}) = (AtsBuildConfigStorable {
      atsStoreHome           = atsHome
    , atsStoreLibDir         = atsLibDir
    , atsStoreSourceDir      = atsSourceDir      
    , atsStoreCC             = atsCC             
    , atsStoreOpt            = atsOpt            
    , atsStoreFlags          = atsFlags          
    , atsStoreBuildDir       = atsBuildDir       
    , atsStoreSourceFiles    = atsSourceFiles    
    , atsStoreProjectVersion = atsProjectVersion 
    , atsStoreTarget         = atsTarget                                                          
   })

fromAtsBuildConfigStorable  :: AtsBuildConfigStorable -> WorkingDir -> AtsBuildConfig
fromAtsBuildConfigStorable (AtsBuildConfigStorable {..}) atsWorkingDirIncoming = (AtsBuildConfig {
      atsHome           = atsStoreHome           
    , atsLibDir         = atsStoreLibDir      
    , atsSourceDir      = atsStoreSourceDir      
    , atsCC             = atsStoreCC             
    , atsOpt            = atsStoreOpt            
    , atsFlags          = atsStoreFlags          
    , atsBuildDir       = atsStoreBuildDir       
    , atsSourceFiles    = atsStoreSourceFiles    
    , atsProjectVersion = atsStoreProjectVersion 
    , atsWorkingDir     = atsWorkingDirIncoming
    , atsTarget         = atsStoreTarget

   })


--------------------------------------------------
-- Aeson instances
--------------------------------------------------

-- | the fromjson instance uses a defaultATS to catch any
--   fields not supplied by the yaml.
--
--   Except for the target field which must be supplied
instance FromJSON AtsBuildConfigStorable where
  parseJSON (Object o) = do
       target <- o .: "ats-target"
       
       let AtsBuildConfigStorable {..} = defaultATSConfigStorable target
       AtsBuildConfigStorable <$>
             (o .: "ats-home"            <|> pure atsStoreHome)
         <*> (o .: "ats-lib-dir"         <|> pure atsStoreLibDir)
         <*> (o .: "ats-source-dir"      <|> pure atsStoreSourceDir)   
         <*> (o .: "ats-cc"              <|> pure atsStoreCC)           
         <*> (o .: "ats-opt"             <|> pure atsStoreOpt)
         <*> (o .: "ats-flags"           <|> pure atsStoreFlags)        
         <*> (o .: "ats-build-dir"       <|> pure atsStoreBuildDir)    
         <*> (o .: "ats-source-files"    <|> pure atsStoreSourceFiles) 
         <*> (o .: "ats-project-version" <|> pure atsStoreProjectVersion)
         <*> pure target


  parseJSON _ = fail "AtsBuildConfigStorable expecting Object"

instance ToJSON AtsBuildConfigStorable where
  toJSON (AtsBuildConfigStorable {..}) = object [
      "ats-target"          .= atsStoreTarget
    , "ats-lib-dir"         .= atsStoreLibDir
    , "ats-home"            .= atsStoreHome
    , "ats-source-dir"      .= atsStoreSourceDir
    , "ats-cc"              .= atsStoreCC
    , "ats-opt"             .= atsStoreOpt
    , "ats-flags"           .= atsStoreFlags
    , "ats-build-dir"       .= atsStoreBuildDir
    , "ats-source-files"    .= atsStoreSourceFiles
    , "ats-project-version" .= atsStoreProjectVersion]



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
defaultATSConfigStorable :: TargetFile -> AtsBuildConfigStorable
defaultATSConfigStorable target = AtsBuildConfigStorable { 
     atsStoreHome           = "/usr/local"
   , atsStoreLibDir         = "/usr/local/lib/ats2-postiats-0.3.2/ccomp/atslib/lib"
   , atsStoreBuildDir       = defaultBuildDirectory
   , atsStoreSourceDir      = unsafeSourceDir "src"
   , atsStoreCC             = "/usr/local/bin/patscc"
   , atsStoreOpt            = "/usr/local/bin/patsopt"
   , atsStoreFlags          = rights $ flag <$> ["-O2", "-DATS_MEMALLOC_LIBC", "-latslib"]     
   , atsStoreSourceFiles    = mempty
   , atsStoreProjectVersion = ver
   , atsStoreTarget         = target  }
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
