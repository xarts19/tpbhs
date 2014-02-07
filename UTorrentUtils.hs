module UTorrentUtils
( startMagnetDonwload
) where

import System.Process
import System.Exit
import Torrent (Magnet(..))

import Debug.Trace

utorrentCommand :: String
utorrentCommand = "C:\\Program Files (x86)\\uTorrent\\uTorrent.exe"

utorrentArg :: String
utorrentArg = "/DIRECTORY"

startMagnetDonwload :: FilePath -> Magnet -> IO (Maybe Int)
startMagnetDonwload destination_folder (Magnet magnet) = do
    traceIO destination_folder
    (_, _, _, pHandle) <- createProcess $ proc utorrentCommand [utorrentArg, "\"" ++ destination_folder ++ "\"", magnet]
    ret <- waitForProcess pHandle
    case ret of
        ExitSuccess -> return Nothing
        ExitFailure code -> return $ Just code
