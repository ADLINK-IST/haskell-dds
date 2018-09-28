{-# LANGUAGE RecordWildCards #-}
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.Types.LocalBuildInfo
import Distribution.Types.TargetInfo
import Distribution.Types.UnqualComponentName
import Distribution.Simple.GHC
import Distribution.System
import Distribution.PackageDescription
import System.Environment
import System.Exit
import System.Process
import Control.Monad

main = defaultMainWithHooks $ autoconfUserHooks { postCopy = addRpath }

addRpath args bf pkg lbi 
  = case buildOS of
      OSX -> addRPathOSX bf pkg lbi
      _   -> return ()

addRPathOSX bf pkg lbi = do
  --let targets = allTargetsInBuildOrder' pkg lbi
  --    tlbis = map targetCLBI targets
  --    names = map componentLocalName tlbis
  --    file = buildDir lbi ++ "/" ++ unComponentId unitId ++ "-" ++ ghcVer ++ ".dylib"
  let names = map (unUnqualComponentName . exeName) $ executables pkg
      files = map (\n -> (fromPathTemplate (bindir $ installDirTemplates lbi)) ++ "/" ++ n) names
  osplHome <- getEnv "OSPL_HOME"
  spliceTarget <- liftM (maybe "" ('/':)) $ lookupEnv "SPLICE_TARGET"
  let osplLibs = osplHome ++ "/lib" ++ spliceTarget
  putStrLn $ "Post build: name = " ++ show files
  putStrLn $ "Post build: dir = " ++ osplLibs
  mapM_ (\f -> executeShellCommand $ "install_name_tool -add_rpath " ++ osplLibs ++ " " ++ f) files

executeShellCommand cmd   = putStrLn ("EXEC: " ++ cmd) >> system cmd >>= check
  where 
    check (ExitSuccess)   = return ()
    check (ExitFailure n) = error $ "cmd: " ++ cmd ++ " failure code " ++ show n
