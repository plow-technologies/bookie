{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Module      : Project.Initialize
Description : Generate a bookie.yaml file
Copyright   : Plow Technologies LLC
License     : MIT License

Maintainer  : Scott Murphy

bookie.yaml file for ats projects is generated after asking you a set of questions
The following defaults are used:
```

ats-home:                   
ats-source-dir:             
ats-cc:           
ats-opt:          
ats-flags:        
ats-build-dir:    
ats-source-files: 
ats-project-version: 
ats-target: <undefined>
```

| -}


module Project.Initialize where

import Project.Types
import Project.Internal
import Control.Lens
import Project.Lens
import GHC.TypeLits (KnownNat,KnownSymbol)
import Data.Aeson (eitherDecode')

import qualified Data.ByteString      as ByteString
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.Yaml as Yaml
import Data.Monoid ((<>))
import Data.String (IsString)
import Development.Shake.Command
import System.Directory (getCurrentDirectory)
import Data.Text (pack,Text)

-- | We want to do different things in the case of an input error and a null string
--   null string means use default
data Input a = UseDefault 
             | InputError String
             | Success    a
  deriving (Show,Functor)






withExtraMessageInputFixedText :: (KnownNat max, KnownNat min, KnownSymbol reg) => String  ->IO ( Input (FixedText max min reg))
withExtraMessageInputFixedText str = do text <- ByteString.getLine
                                        bytestringToFixedText text
  where
    bytestringToFixedText txt
          | ByteString.null txt = return $ UseDefault
          | otherwise           = case eitherDecode'. ByteStringLazy.fromStrict . quoteText $ txt of
                                    (Left err)     -> return $ InputError  (str <> " " <> show  err)
                                    (Right txtVal) -> return $ Success txtVal




quoteText :: (Monoid m, Data.String.IsString m) => m -> m
quoteText text = "\""<> text <> "\""

--------------------------------------------------
-- File Types
--------------------------------------------------

-- | Input the name of the folder and built executable
--   Right now, the assumption is made that these are the same.
inputTargetFile :: IO (Input TargetFile)
inputTargetFile = withExtraMessageInputFixedText errMsg
  where
    errMsg = "Failure to read TargetFile: "


-- | input the starting version of the project you are building
inputVersion :: IO (Input Version)
inputVersion = withExtraMessageInputFixedText errMsg
  where
    errMsg = "Failure to read Version: "


-- | File where the work will be done
inputBuildDir :: IO (Input BuildDir)
inputBuildDir = withExtraMessageInputFixedText errMsg
  where
    errMsg = "Failure to read build dir"


inputSourceDir :: IO (Input FilePath)
inputSourceDir = (fmap show)   <$> toFilePath
  where
    toFilePath :: IO (Input (FixedText 100 3  "[[:alphanum:]]"))
    toFilePath = withExtraMessageInputFixedText errMsg
    errMsg = "Failure to get source dir"





-- | We want the behavior to be a bit specific
--   this is like fromMaybeM but with a fail condition
useDefault :: a -> IO ( Input a) -> IO a
useDefault a mia = do ia <- mia
                      case ia of
                         UseDefault       -> return a
                         Success a'       -> return a'
                         InputError err   -> fail $ show err




-- | here we just want to fail in both cases
-- in otherwords there can be no default
noDefault :: IO ( Input a) -> IO a
noDefault  mia = do ia <- mia
                    case ia of
                      UseDefault       -> fail "No default possible for this field."
                      Success a'       -> return a'
                      InputError err   -> fail $ show err


askForInputWithDefault :: (Show a) => String -> a -> IO (Input a) -> IO a
askForInputWithDefault fieldName dflt action = do
  putStrLn ("Enter " <> fieldName <> " (" <> show dflt <> "): ")
  useDefault dflt action

askForInput :: String ->  IO (Input a) -> IO a
askForInput fieldName action = do
  putStrLn ("Enter " <> fieldName <> " (required): ")
  noDefault action


-- | Just ask for the required field of target
--   use default values for everything else
getAtsConfigSimple :: IO AtsBuildConfig
getAtsConfigSimple = do   
    target <- askForInput "target" inputTargetFile
    let (AtsBuildConfig {..}) = defaultATSConfig target
    
    return (defaultATSConfig target)





-- | Query several of the values instead of just target
getAtsConfig :: IO AtsBuildConfig
getAtsConfig = do
  (Right wd)                     <- (workingDir.pack) <$> getCurrentDirectory
  (AtsBuildConfig {..})  <- getAtsConfigSimple
  version                <- askForInputWithDefault "Version"   atsProjectVersion inputVersion
  buildDir'              <- askForInputWithDefault "build dir" atsBuildDir       inputBuildDir
  _sourceDir'            <- askForInputWithDefault "source dir" atsSourceDir     inputSourceDir
  return (AtsBuildConfig atsHome
                         atsSourceDir
                         atsCC
                         atsOpt
                         atsFlags
                         buildDir'
                         atsSourceFiles
                         version
                         wd
                         atsTarget) 


-- | Pull the target out and build with it.
writeBookieFile :: AtsBuildConfig  -> IO ()
writeBookieFile cfg = Yaml.encodeFile filePath cfg
  where
    filePath = (show . atsTarget $ cfg) <> "/" <> "bookie.yaml"


-- | Write teh bookie file with a supplied target
writeAtsNoQuery :: TargetFile -> IO ()
writeAtsNoQuery target = do
  let cfg = defaultATSConfig target
  generateProjectTree cfg

-- | Write the bookie file after query simple
writeAtsSimple :: IO ()
writeAtsSimple = do
  cfg <- getAtsConfigSimple
  generateProjectTree cfg


-- | Write the bookie file after query
writeAts :: IO ()
writeAts = do
  cfg <- getAtsConfig
  generateProjectTree cfg

generateProjectTree :: AtsBuildConfig -> IO ()
generateProjectTree cfg@(AtsBuildConfig{..}) = do
  cmd Shell ("mkdir " ++ show atsTarget :: String) :: IO ()
  cmd (Cwd (show atsTarget)) Shell ("mkdir " ++ (atsSourceDir)      :: String) :: IO ()
  cmd (Cwd (show atsTarget)) Shell ("mkdir " ++ (show atsBuildDir)  :: String) :: IO ()
  writeBookieFile cfg




addSourceFile :: SourceFile -> IO ()
addSourceFile src = do
  val <- Yaml.decodeFileEither "bookie.yaml" :: IO (Either Yaml.ParseException AtsBuildConfig)
  either (\ex -> print ex) addInTheSource val
    where
      addInTheSource val = do let newVal =  val & sourceFiles %~ (src :)
                              Yaml.encodeFile "bookie.yaml" newVal




-- | Transform a source file into a fixed text
txtToSourceFile :: Text -> Either FixedTextErrors SourceFile
txtToSourceFile txt = (SourceFileDats <$> datsFile  txt) `opt`
                       (SourceFileSats <$> satsFile txt) `opt`
                       (SourceFileHats <$> hatsFile txt) `opt`                      
                       (SourceFileC    <$> cfile    txt) 

  where
    opt :: Either a b -> Either a b -> Either a b
    opt (Left _)  b = b
    opt (Right r) _ = Right r
