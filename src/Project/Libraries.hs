{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeInType #-}

module Project.Libraries  where

import Development.Shake ( Rules
                          , command_
                          , Action
                          , CmdOption(..)
                          , want
                          , need
                          , liftIO
                          , (%>))
import           Development.Shake.FilePath ((</>),FilePath)



import Data.Either (rights,lefts)
import Project.Internal
import Data.Semigroup ((<>))
import Data.Monoid (mempty)
import Control.Monad (when)
import Project.Defaults ( defaultTargetDestination )
import Debug.Trace

import Project.Types ( DatsFile
                     , ObjFile
                     , TargetFile
                     , Flag
                     , BuildDir
                     , WorkingDir
                     , SourceFiles
                     , SourceFile     (..)
                     , CommandRec     (..)
                     , AtsBuildConfig (..)
                     , datsFile)


--------------------------------------------------
-- End Imports
--------------------------------------------------


-- | Given a Config, this function assembles the build process
atsProjectBuilder :: AtsBuildConfig -> Rules ()
atsProjectBuilder cfg' = final
  where
    cfg             = cfg' {atsSourceFiles = addDefaultSourceFiles (atsSourceFiles cfg') (atsTarget cfg')}
    
    sourceFiles     = atsSourceFiles   cfg
    buildDir        = atsBuildDir      cfg
    workingDir      = atsWorkingDir    cfg
    targetFile      = atsTarget        cfg
    targetRule      = targetFileToRule cfg

    objectWants     = makeAtsObjectFilePaths sourceFiles workingDir  buildDir
    targetWant      = makeTargetLocation     workingDir  buildDir    targetFile

    possibleRules   = fmap (sourceFileToRule cfg ) . atsSourceFiles $ cfg

    malformedRules  = lefts  possibleRules
    sourceFileRules = rights possibleRules
    
    allRules        =  sourceFileRules <> [targetRule] :: [Rules ()]
    final       = do
      liftIO $ print objectWants
      liftIO $ print targetWant
      liftIO $ putStrLn "all rules: count" *> (print . length $ allRules)
      when (length malformedRules > 0 ) (liftIO $ putStrLn "These rules were malformed" >> print malformedRules)
      want (objectWants <> [targetWant])
      _ <- sequence allRules
      return ()






-- | Build each of the source files as an object
--   in its build directory
makeAtsObjectFilePaths :: SourceFiles -> WorkingDir -> BuildDir  -> [FilePath]
makeAtsObjectFilePaths atsSourceFiles atsWorkingDir atsBuildDir  = atsBuildObjects
  where
    atsBuildDirFP    = show atsBuildDir
    fullBuildDirPath = show atsWorkingDir </> atsBuildDirFP
    addBuildDir      = (fullBuildDirPath </>)
    atsBuildObjects  = rights $ (fmap (addBuildDir. fixedTextToString) . sourceFileToObject) <$> atsSourceFiles





--------------------------------------------------
-- Data Transformers
-- these functions perform pure transformatons
-- on data.
--------------------------------------------------  


-- | Declare the final binary target and put it in the dist/build folder
makeTargetLocation :: WorkingDir -> BuildDir -> TargetFile -> FilePath
makeTargetLocation atsWorkingDir atsBuildDir atsTarget = targetLocation
  where
    atsWorkingDirFP  = show atsWorkingDir
    atsBuildDirFP    = show atsBuildDir
    targetLocation   = atsWorkingDirFP             </>
                       atsBuildDirFP               </>
                       defaultTargetDestination    </>
                       fixedTextToString atsTarget




-- | If no source files are specified, use the target as the source file
-- with an assumed dats extension and sourceDir location
addDefaultSourceFiles :: [SourceFile] -> TargetFile -> [SourceFile]
addDefaultSourceFiles atsSourceFiles atsTarget
   | null atsSourceFiles = either mempty (: []) $ SourceFileDats <$> defaultDatsFile
   | otherwise           = atsSourceFiles
       where
         defaultDatsFile = targetFileToDats atsTarget



-- | Convert Flag to string for use in command
flagToString :: Flag -> String
flagToString flag' = fixedTextToString flag'



-- | Turn any Source File into a FilePath that is usable with shake
sourceFileToFilePath :: SourceFile -> FilePath
sourceFileToFilePath bf = case bf of
    (SourceFileC    c) -> fixedTextToString c
    (SourceFileDats d) -> fixedTextToString d
    (SourceFileHats h) -> fixedTextToString h
    (SourceFileSats s) -> fixedTextToString s



-- | Convert a Source.dats into Source.dats.o
sourceFileToObject :: SourceFile -> Either FixedTextErrors ObjFile
sourceFileToObject src = atsBuildObjects
  where    
    addAnO            = (<> ".o")
    atsBuildObjects   = (fixedTextFromString .
                         addAnO .  sourceFileToFilePath) src
                        




-- | Convert a target into a dats file
-- useful in default config
targetFileToDats :: TargetFile -> Either FixedTextErrors DatsFile
targetFileToDats = datsFile . (<> ".dats") . fixedTextToText


-- | Rule to generate the final target file for the build
targetFileToRule  :: AtsBuildConfig ->  Rules ()
targetFileToRule cfg  =  fileTarget  %> buildTarget
  where
    fileTarget    = makeTargetLocation workingDir buildDir targetFile
    needs         = makeAtsObjectFilePaths sourceFiles workingDir buildDir
    sourceFiles   = atsSourceFiles cfg 
    buildDir      = atsBuildDir    cfg
    workingDir    = atsWorkingDir  cfg
    targetFile    = atsTarget      cfg
    buildTarget _ = do
      need needs
      let targetRec = atsBuildTarget cfg 
      executeCommandRec targetRec


-- | Add all the directories to the names of the files 
sourceFileToRule :: AtsBuildConfig -> SourceFile -> Either FixedTextErrors (Rules ())
sourceFileToRule cfg@AtsBuildConfig { atsSourceDir
                                    , atsWorkingDir
                                    , atsBuildDir  } src = ( %> buildSource) <$> (traceShow buildFilePath buildFilePath)
  where
    workingDirFP      = show atsWorkingDir
    atsBuildDirFP     = show atsBuildDir
    atsFullBuildDirFP = workingDirFP    </> atsBuildDirFP
    atsFullSourceFP   = workingDirFP    </> show atsSourceDir
    sourceFilePath    = atsFullSourceFP </> fileName

    buildFilePath     = (atsFullBuildDirFP </> ) <$>  
                            (fixedTextToString <$> objectFile)

    fileName          = sourceFileToFilePath src    
    objectFile        = sourceFileToObject   src

    buildSource _     =
      need [sourceFilePath]            *>
      executeCommandRec (atsBuildObjectCommand cfg src)







--------------------------------------------------
-- Command Builders
--------------------------------------------------

-- | Instead of executing comands directly
--   this library builds intermediate structures
--   called command records.  This allows us to more easily
--   test the build script with multiple interpreters.
atsBuildObjectCommand :: AtsBuildConfig -> SourceFile -> CommandRec
atsBuildObjectCommand cfg src = CommandRec cmd opts args 
  where
    opts          = [Cwd atsBuildDirFP]    
    cmd           = atsCC         cfg    
    atsFlags'     = atsFlags      cfg
    buildDir      = atsBuildDir   cfg
    workingDir    = atsWorkingDir cfg
    sourceDir     = atsSourceDir  cfg
    workingDirFP  = show workingDir
    atsBuildDirFP = show buildDir    
    objectFile    = either (error . ("ats-build-problem" ++) .show) id (sourceFileToObject src)
    args          = [ workingDirFP </> show sourceDir </> sourceFileToFilePath src
                    , compileSourceFilesNoLinkFlag
                    , compileOutputFlag
                    , workingDirFP </> atsBuildDirFP </> fixedTextToString objectFile ] <> (flagToString <$> atsFlags')

-- | Compile output
compileOutputFlag :: FilePath
compileOutputFlag = "-o"

-- | Compile or assemble the source files, but do not link. The linking stage simply is not done. The ultimate output is in
--   the form of an object file for each source file. 
compileSourceFilesNoLinkFlag :: FilePath
compileSourceFilesNoLinkFlag = "-c"


-- | Run Command Specified in the command Record
executeCommandRec :: CommandRec -> Action ()
executeCommandRec (CommandRec cmd opts args) = command_ opts cmd args


-- | Build the target
atsBuildTarget :: AtsBuildConfig -> CommandRec
atsBuildTarget AtsBuildConfig {  atsCC
                               , atsFlags
                               , atsSourceFiles
                               , atsBuildDir
                               , atsWorkingDir
                               , atsTarget} = CommandRec atsCC [Cwd (show atsWorkingDir </> show atsBuildDir)] allOptions
  where    
    objects    = makeAtsObjectFilePaths atsSourceFiles atsWorkingDir atsBuildDir    
    allOptions = objects                                                         <>
                 [ compileOutputFlag , makeTargetLocation atsWorkingDir atsBuildDir atsTarget] <>
                 (fixedTextToString <$> atsFlags)

