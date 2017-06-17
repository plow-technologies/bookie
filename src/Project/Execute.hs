
{- |
Module      : Project.Execute
Description : Execute the built target by name
Copyright   : Plow Technologies LLC
License     : MIT License

Maintainer  : Scott Murphy


Execute a given target by running
$> bookie exec <target-name> 

-}



module Project.Execute where



import Project.Types
import Data.Text
import Data.Semigroup
import Project.Internal
import Development.Shake.Command


-- | Fill out the whole path to the target by  adding the
--   defaultTargetDestination to the BuildDir
buildTargetFilePath :: BuildDir -> Either FixedTextErrors ExecDir
buildTargetFilePath bd = (bd <>) <$> (fixedTextFromString defaultTargetDestination)


-- | Execute the target command.
executeCmd :: BuildDir -> Text -> IO ()
executeCmd bd target = evaluateCases
  where
    evaluateCases = case buildTargetFilePath bd of
                      (Left    e) -> putStrLn $ "Failure to read: " <> show e
                      (Right  fp) -> cmd Shell (fixedTextToString fp <> unpack target )   
