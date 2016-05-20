module Main where

import System.IO
import System.Linux.Input.Event
import Control.Monad
import Control.Monad.Loops
import Control.Monad.IfElse
import System.Process
import System.Exit
import Control.Concurrent
import Data.Time.Clock
import Data.List
import System.Directory

data Action = POWER | VOLUMEUP | VOLUMEDOWN | VIDEO4 | VIDEO5 deriving Show

type ActionChan = Chan (Action, UTCTime)

keyPower, keyVolDown, keyVolUp, keySuper, keyAppleTV, keyPC :: Key
keyPower = Key 38
keyVolDown = Key 114
keyVolUp = Key 115
keySuper = Key 125
keyPC = Key 35
keyAppleTV = Key 37

released :: Key -> Event -> Bool
released requestedKey (KeyEvent _ key Released) = key == requestedKey
released _ _ = False

sendAction :: ActionChan -> Maybe Action -> IO ()
sendAction _ Nothing = return ()
sendAction events (Just action) = do
    now <- getCurrentTime
    writeChan events (action, now)

runAction :: (Action, UTCTime) -> IO ()
runAction (a, t) = do
    now <- getCurrentTime

    if (diffUTCTime now t < 1) then do
        (_exitCode , stdout, stderr) <-
            readProcessWithExitCode "irsend" ["send_once", "sony-kdl40w2000", "KEY_" ++ show a] ""

        when (stdout /= "") $ putStrLn stdout
        when (stderr /= "") $ putStrLn stderr
    else
        putStrLn "Discarding event older than 1 second"

handleSuperModifiedKey :: Key -> Maybe Action
handleSuperModifiedKey key
    | key == keyPower = Just POWER
    | key == keyPC = Just VIDEO4
    | key == keyAppleTV = Just VIDEO5
    | otherwise = Nothing

handleSuper :: ActionChan -> Handle -> IO Event
handleSuper events f = do
    e <- readEvent f
    sendAction events $ case e of
        KeyEvent _ key Depressed -> handleSuperModifiedKey key
        KeyEvent _ key Repeated -> handleSuperModifiedKey key
        _ -> Nothing

    return e

superPressed :: ActionChan -> Handle -> IO ()
superPressed events f = void $ iterateUntil (released keySuper) $ handleSuper events f

translateKey :: Key -> Maybe Action
translateKey key
    | key == keyVolDown = Just VOLUMEDOWN
    | key == keyVolUp = Just VOLUMEUP
    | otherwise = Nothing

translateDepressedKey :: ActionChan -> Handle -> Key -> IO ()
translateDepressedKey events f key
    | key == keySuper = superPressed events f
    | otherwise = sendAction events $ translateKey key

translateEvent :: ActionChan -> Handle -> Event -> IO ()
translateEvent events f (KeyEvent _ key Depressed) = translateDepressedKey events f key
translateEvent events _ (KeyEvent _ key Repeated) = sendAction events $ translateKey key
translateEvent _ _ _ = return ()

readEvent :: Handle -> IO Event
readEvent f = untilJust $ hReadEvent f

readEvents :: ActionChan -> Handle -> IO ()
readEvents events f = forever (readEvent f >>= translateEvent events f)

exitedSuccessfully :: ExitCode -> Bool
exitedSuccessfully ExitSuccess = True
exitedSuccessfully _ = False

seconds :: Int -> Int
seconds n = n * (10 ^ 6)

untilSuccessful :: String -> [String] -> IO ()
untilSuccessful command args = void $ iterateUntil exitedSuccessfully $ do
    putStr "Waiting for "
    putStrLn command
    (exitCode, _, _) <- readProcessWithExitCode command args ""
    threadDelay $ seconds 1
    return exitCode

stopService :: String -> IO ()
stopService name = void $ readProcessWithExitCode "service" [name, "stop"] ""

startService :: String -> IO ()
startService name = callProcess "service" [name, "start"]

translateEvents :: ActionChan -> IO ()
translateEvents events =
    withFile "/dev/input/by-id/usb-Logitech_USB_Receiver-if02-event-mouse" ReadMode $ readEvents events

-- Disable iguanaIR and lirc services with:
--  sudo update-rc.d iguanaIR disable
--  sudo update-rc.d lirc disable

iguanaVendorId, iguanaProductId :: String
iguanaVendorId = "1781\n"
iguanaProductId = "0938\n"

iguanaService, lircService :: String
iguanaService = "iguanaIR"
lircService = "lirc"

main :: IO ()
main = do
    stopService iguanaService
    stopService lircService

    let deviceDir = "/sys/bus/usb/devices"

    usbDevices <- (\\ [".", ".."]) <$> getDirectoryContents deviceDir

    forM usbDevices $ \device -> do
        let devicePath = deviceDir ++ "/" ++ device
        let vendorIdFile = devicePath ++ "/idVendor"
        let productIdFile = devicePath ++ "/idProduct"

        whenM (andM (doesFileExist <$> [vendorIdFile, productIdFile])) $ do
            vendorId <- readFile vendorIdFile
            productId <- readFile productIdFile

            when (vendorId == iguanaVendorId && productId == iguanaProductId) $ do
                let authorizedPath = devicePath ++ "/authorized"

                putStrLn "Resetting IR transceiver"
                writeFile authorizedPath "0\n"
                threadDelay $ seconds 1
                writeFile authorizedPath "1\n"
                threadDelay $ seconds 1

    startService iguanaService
    untilSuccessful "igclient" ["--get-version"]

    startService lircService
    untilSuccessful "irsend" ["set_transmitters", "1"]

    threadDelay $ seconds 1

    putStrLn "Running"

    events <- newChan
    void $ forkIO $ translateEvents events

    forever (readChan events >>= runAction)