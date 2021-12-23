{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.IO as TIO
import TextShow
import Options.Applicative as Opt

import App
import DirTree
import DiskUsage
import FileCounter


work :: AppConfig -> IO ()
work config = do
    (_, dirs) <- runMyApp dirTree config ()
    (_, counters) <- runMyApp fileCount config ()
    (_, usages) <- runMyApp diskUsage config (0 :: FileOffset)
    let report = toText $ 
                 buildEntries "Directory tree:" treeEntryBuilder dirs
                 <> buildEntries "File counter:" tabEntryBuilder counters
                 <> buildEntries "File space usage:" tabEntryBuilder usages
    TIO.putStr report

buildEntries :: Builder -> (e -> Builder) -> [e] -> Builder
buildEntries title entryBuilder entries =
    unlinesB $ title : map entryBuilder entries

tabEntryBuilder :: TextShow s => (FilePath, s) -> Builder
tabEntryBuilder (fp, s) = showb s <> "\t" <> fromString fp

treeEntryBuilder :: (FilePath, Int) -> Builder
treeEntryBuilder (fp, n) = fromString indent <> fromString fp
    where 
        indent = replicate (2 * n) ' '


mkConfig :: Opt.Parser AppConfig
mkConfig =
    AppConfig
    <$> strArgument (metavar "DIRECTORY" <> value "." <> showDefault )
    <*> option auto
        (metavar "DEPTH" <> short 'd' <> long "depth" <> value maxBound <> 
         help "Display an entry for all directories DEPTH directories deep")
    <*> optional
        (strOption (metavar "EXT" <> long "extension" <> short 'e' <>
         help "Filter files by extestion"))
    <*> switch
        (short 'L' <> help "Follow symlinks (Off by default)")


main :: IO ()
main = execParser opts >>= work 
    where
        opts = info (mkConfig <**> helper)
                    (fullDesc <> progDesc "Directory usage info")
