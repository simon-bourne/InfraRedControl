{-# LANGUAGE DefaultSignatures #-}

module Main (main) where

import TypedIO
import System.IO (Handle)
import System.Linux.Input.Event
import Control.Monad
import Control.Monad.Loops
import Control.Monad.IfElse
import System.Exit
import Control.Concurrent (Chan)
import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.List

data ActionKey = POWER | VOLUMEUP | VOLUMEDOWN | VIDEO4 | VIDEO5 deriving (Show, Eq)
data Action = Start ActionKey UTCTime | Stop

type ActionChan = Chan Action

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

data IrSendAction = Send_Once | Send_Start | Send_Stop deriving Show

class Monad m => IrSender m where
    irsend :: IrSendAction -> ActionKey -> m ()
    default irsend :: (RunProcess m, StdoutWriter m) => IrSendAction -> ActionKey -> m ()
    irsend action key = do
        (_exitCode , output, errorOutput) <-
            readProcessWithExitCode "irsend" [show action, "sony-kdl40w2000", "KEY_" ++ show key] ""
        logOutput output
        logOutput errorOutput
          where
            logOutput o = when (o /= "") $ writeStdoutLn o

instance IrSender IO

class Monad m => ActionSender m where
    sendAction :: ActionChan -> Maybe ActionKey -> m ()
    default sendAction :: (CurrentTime m, Producer m) => ActionChan -> Maybe ActionKey -> m ()
    sendAction _ Nothing = return ()
    sendAction events (Just action) = do
        now <- getCurrentTime
        writeChan events $ Start action now

    sendStop :: ActionChan -> m ()
    default sendStop :: Producer m => ActionChan -> m ()
    sendStop events = writeChan events Stop

instance ActionSender IO

irstart :: IrSender m => ActionKey -> m (Maybe ActionKey)
irstart key
    | key `elem` [VOLUMEUP, VOLUMEDOWN] = do
        irsend Send_Start key
        return $ Just key
    | otherwise = do
        irsend Send_Once key
        return Nothing

startAction :: (CurrentTime m, IrSender m, StdoutWriter m) => ActionKey -> UTCTime -> m (Maybe ActionKey)
startAction key t = do
    now <- getCurrentTime

    if (diffUTCTime now t < 1) then
        irstart key
    else do
        writeStdoutLn "Discarding event older than 1 second"
        return Nothing

runAction :: (CurrentTime m, IrSender m, StdoutWriter m) => Maybe ActionKey -> Action -> m (Maybe ActionKey)
runAction (Just currentKey) Stop = do
    irsend Send_Stop currentKey
    return Nothing
runAction (Just currentKey) (Start key t)
    | currentKey == key = return $ Just key
    | otherwise = do
        irsend Send_Stop currentKey
        startAction key t
runAction Nothing (Start key t) = startAction key t
runAction Nothing Stop = return Nothing

handleSuperModifiedKey :: Key -> Maybe ActionKey
handleSuperModifiedKey key
    | key == keyPower = Just POWER
    | key == keyPC = Just VIDEO4
    | key == keyAppleTV = Just VIDEO5
    | otherwise = Nothing

handleSuper :: (EvdevReader m, ActionSender m) => ActionChan -> Handle -> m Event
handleSuper events f = do
    e <- readEvent f
    sendAction events $ case e of
        KeyEvent _ key Depressed -> handleSuperModifiedKey key
        KeyEvent _ key Repeated -> handleSuperModifiedKey key
        _ -> Nothing

    return e

superPressed :: (EvdevReader m, ActionSender m) => ActionChan -> Handle -> m ()
superPressed events f = void $ iterateUntil (released keySuper) $ handleSuper events f

translateKey :: Key -> Maybe ActionKey
translateKey key
    | key == keyVolDown = Just VOLUMEDOWN
    | key == keyVolUp = Just VOLUMEUP
    | otherwise = Nothing

translateDepressedKey :: (EvdevReader m, ActionSender m) => ActionChan -> Handle -> Key -> m ()
translateDepressedKey events f key
    | key == keySuper = superPressed events f
    | otherwise = sendAction events $ translateKey key

translateEvent :: (EvdevReader m, ActionSender m) => ActionChan -> Handle -> Event -> m ()
translateEvent events f (KeyEvent _ key Depressed) = translateDepressedKey events f key
translateEvent events _ (KeyEvent _ key Repeated) = sendAction events $ translateKey key
translateEvent events _ (KeyEvent _ _ Released) = sendStop events
translateEvent _ _ _ = return ()

readEvents :: (EvdevReader m, ActionSender m) => ActionChan -> Handle -> m ()
readEvents events f = forever (readEvent f >>= translateEvent events f)

exitedSuccessfully :: ExitCode -> Bool
exitedSuccessfully ExitSuccess = True
exitedSuccessfully _ = False

seconds :: Int -> Int
seconds n = n * (10 ^ (6 :: Int))

untilSuccessful :: (Sleeper m, StdoutWriter m, RunProcess m) => String -> [String] -> m ()
untilSuccessful command args = void $ iterateUntil exitedSuccessfully $ do
    writeStdout "Waiting for "
    writeStdoutLn command
    exitCode <- runProcessWithExitCode command args
    sleep $ seconds 1
    return exitCode

stopService :: RunProcess m => String -> m ()
stopService name = void $ runProcessWithExitCode "service" [name, "stop"]

startService :: RunProcess m => String -> m ()
startService name = runProcessOrFail "service" [name, "start"]

translateEvents :: (EvdevReader m, ActionSender m, FileReader m) => ActionChan -> m ()
translateEvents events =
    withReadOnlyFile "/dev/input/by-id/usb-Logitech_USB_Receiver-if02-event-mouse" $ readEvents events

-- Disable iguanaIR and lirc services with:
--  sudo update-rc.d iguanaIR disable
--  sudo update-rc.d lirc disable

iguanaVendorId, iguanaProductId :: String
iguanaVendorId = "1781\n"
iguanaProductId = "0938\n"

iguanaService, lircService :: String
iguanaService = "iguanaIR"
lircService = "lirc"

filesAllExist :: DirectoryReader m => [String] -> m Bool
filesAllExist files = andM (doesFileExist <$> files)

isIguanaDevice :: String -> String -> Bool
isIguanaDevice vendorId productId =
    vendorId == iguanaVendorId && productId == iguanaProductId

resetUSBDevice :: (StdoutWriter m, FileWriter m, Sleeper m) => String -> m ()
resetUSBDevice devicePath = do
    let authorizedPath = devicePath ++ "/authorized"

    writeStdoutLn "Resetting IR transceiver"
    writeToFile authorizedPath "0\n"
    sleep $ seconds 1
    writeToFile authorizedPath "1\n"
    sleep $ seconds 1

resetIfIguanaDevice :: (StdoutWriter m, FileReader m, FileWriter m, Sleeper m, DirectoryReader m) => String -> m ()
resetIfIguanaDevice device = do
    let devicePath = deviceDir ++ "/" ++ device
    let vendorIdFile = devicePath ++ "/idVendor"
    let productIdFile = devicePath ++ "/idProduct"

    whenM (filesAllExist [vendorIdFile, productIdFile]) $ do
        vendorId <- readFromFile vendorIdFile
        productId <- readFromFile productIdFile

        when (isIguanaDevice vendorId productId) $ resetUSBDevice devicePath

deviceDir :: String
deviceDir = "/sys/bus/usb/devices"

processEvent :: (Consumer m, CurrentTime m, IrSender m, StdoutWriter m) => ActionChan -> Maybe ActionKey -> m (Maybe ActionKey)
processEvent events current = readChan events >>= runAction current

stopServices :: RunProcess m => m ()
stopServices = stopService iguanaService >> stopService lircService

resetIguanaDevices :: (StdoutWriter m, Sleeper m, DirectoryReader m, FileWriter m, FileReader m) => m ()
resetIguanaDevices = do
    usbDevices <- (\\ [".", ".."]) <$> getDirectoryContents deviceDir

    mapM_ resetIfIguanaDevice usbDevices

startServices :: (StdoutWriter m, RunProcess m, Sleeper m) => m ()
startServices = do
    startService iguanaService
    untilSuccessful "igclient" ["--get-version"]

    startService lircService
    untilSuccessful "irsend" ["set_transmitters", "1"]

    sleep $ seconds 1

    writeStdoutLn "Running"

runThreads :: (Controller m,
               FileReader m,
               EvdevReader m,
               ActionSender m,
               StdoutWriter m,
               IrSender m,
               CurrentTime m) => m ()
runThreads = do
    events <- newChan
    void $ fork $ translateEvents events

    iterateM_ (processEvent events) Nothing

main :: IO ()
main = do
    stopServices
    resetIguanaDevices
    startServices
    runThreads
