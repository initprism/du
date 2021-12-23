{-# LANGUAGE RecordWildCards #-}

module FileCounter where

import App
import Utils
import System.Directory.Extra

fileCount :: MyApp (FilePath, Int) s ()
fileCount = do
    AppEnv {..} <- ask
    fs <- currentPathStatus 
    when (isDirectory fs && depth <= maxDepth cfg) $ do
        traverseDirectoryWith fileCount
        files <- liftIO $ listFiles path
        tell [(path, length $ filter (checkExtension cfg) files )]
