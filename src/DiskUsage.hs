{-# LANGUAGE RecordWildCards #-}
module DiskUsage where

import App
import Utils


data DUEntryAction =
      TraverseDir    {dirPath :: FilePath, requireReporting :: Bool}
    | RecordFileSize {fsize :: FileOffset}
    | None


diskUsage :: MyApp (FilePath, FileOffset) FileOffset ()
diskUsage = liftM2 decide ask currentPathStatus >>= processEntry
    where
        decide AppEnv {..} fs
          | isDirectory fs =
              TraverseDir path (depth <= maxDepth cfg)
          | isRegularFile fs && checkExtension cfg path =
              RecordFileSize (fileSize fs)
          | otherwise = 
              None
        
        processEntry TraverseDir {..} = do
            usageOnEntry <- get
            traverseDirectoryWith diskUsage
            when requireReporting $ do
                usageOnExit <- get
                tell [(dirPath, usageOnExit - usageOnEntry)]
        processEntry RecordFileSize {..} = modify (+fsize)
        processEntry None                = pure ()

