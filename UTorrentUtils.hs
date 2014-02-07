module UTorrentUtils
( startUTorrent
, startMagnetDonwload
) where

import System.Process
import System.Exit
import Control.Concurrent
import Torrent (Magnet(..))

utorrentCommand :: String
utorrentCommand = "C:\\Program Files (x86)\\uTorrent\\uTorrent.exe"

utorrentArg :: String
utorrentArg = "/DIRECTORY"

startUTorrent :: IO ()
startUTorrent = do
    putStrLn "Starting uTorrent..."
    _ <- createProcess $ proc utorrentCommand []
    threadDelay 1000000
    return ()

startMagnetDonwload :: FilePath -> Magnet -> IO (Maybe Int)
startMagnetDonwload destination_folder (Magnet magnet) = do
    (_, _, _, pHandle) <- createProcess $ proc utorrentCommand [utorrentArg, destination_folder, magnet]
    ret <- waitForProcess pHandle
    case ret of
        ExitSuccess -> return Nothing
        ExitFailure code -> return $ Just code
