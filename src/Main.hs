{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE PatternSynonyms   #-}

module Main (main) where

import           Control.Concurrent       (Chan)
import           Control.Monad
import           Control.Monad.IfElse
import           Control.Monad.Loops
import           Data.List
import           System.Exit
import           System.IO                (Handle)
import           System.Linux.Input.Event
import           Data.Time.Clock (UTCTime, diffUTCTime)
import TypedIO

class Show a => NamedKey a where
    keyName :: a -> String
    keyName = show

data ActionKey = SingleKey SingleKey | RepeatingKey RepeatingKey
data SingleKey = POWERON | POWER | VIDEO4 | VIDEO5 deriving (NamedKey, Show)
data RepeatingKey = VOLUMEUP | VOLUMEDOWN deriving (NamedKey, Show, Eq)

data CurrentChannel = PC | AppleTV

data Action = Start ActionKey UTCTime | Stop

newtype ActionChan = ActionChan (Chan Action)

pattern KeyPower = Key 38
pattern KeyVolDown = Key 114
pattern KeyVolUp = Key 115
pattern KeySuper = Key 125
pattern KeySwitchChannel = Key 35

data IrSendAction = Send_Once | Send_Start | Send_Stop deriving Show

runIRSend :: (NamedKey k, RunProcess m, StdoutWriter m) => IrSendAction -> k -> m ()
runIRSend action key = do
    (_exitCode , output, errorOutput) <-
        readProcessWithExitCode "irsend" [show action, "sony-kdl40w2000", "KEY_" ++ keyName key] ""
    logOutput output
    logOutput errorOutput
      where
        logOutput o = when (o /= "") $ writeStdoutLn o

newtype CurrentKey = CurrentKey RepeatingKey

newtype EvdevHandle = EvdevHandle Handle

class Monad m => EvdevReader m where
    withEvdev :: FilePath -> (EvdevHandle -> m a) -> m a
    readEvent :: EvdevHandle -> m Event

instance EvdevReader IO where
    withEvdev name f = withReadOnlyFile ("/dev/input/by-id/" ++ name) (f . EvdevHandle)
    readEvent (EvdevHandle h) = untilJust $ hReadEvent h

class Monad m => IrSender m where
    irstart :: ActionKey -> m (Maybe CurrentKey)
    default irstart :: (RunProcess m, StdoutWriter m) => ActionKey -> m (Maybe CurrentKey)
    irstart (SingleKey key) = do
        runIRSend Send_Once key
        return Nothing
    irstart (RepeatingKey key) = do
        runIRSend Send_Start key
        return $ Just $ CurrentKey key

    irstop :: CurrentKey -> m ()
    default irstop :: (RunProcess m, StdoutWriter m) => CurrentKey -> m ()
    irstop (CurrentKey key) = runIRSend Send_Stop key

instance IrSender IO

class Monad m => ActionSender m where
    sendAction :: ActionChan -> Maybe ActionKey -> m ()
    default sendAction :: (CurrentTime m, Producer m) => ActionChan -> Maybe ActionKey -> m ()
    sendAction _ Nothing = return ()
    sendAction (ActionChan events) (Just action) = do
        now <- getCurrentTime
        writeChan events $ Start action now

    sendStop :: ActionChan -> m ()
    default sendStop :: Producer m => ActionChan -> m ()
    sendStop (ActionChan events) = writeChan events Stop

instance ActionSender IO

startAction :: (CurrentTime m, IrSender m, StdoutWriter m) => ActionKey -> UTCTime -> m (Maybe CurrentKey)
startAction key t = do
    now <- getCurrentTime

    if diffUTCTime now t < 1 then
        irstart key
    else do
        writeStdoutLn "Discarding event older than 1 second"
        return Nothing

runAction :: (CurrentTime m, IrSender m, StdoutWriter m) => Maybe CurrentKey -> Action -> m (Maybe CurrentKey)
runAction (Just currentKey) Stop = do
    irstop currentKey
    return Nothing
runAction key@(Just (CurrentKey currentKey)) (Start (RepeatingKey repeatingKey) _)
    | currentKey == repeatingKey = return key
runAction (Just currentKey) (Start key t) = do
    irstop currentKey
    startAction key t
runAction Nothing (Start key t) = startAction key t
runAction Nothing Stop = return Nothing

handleSuperModifiedKey :: CurrentChannel -> Key -> (CurrentChannel, Maybe ActionKey)
handleSuperModifiedKey currentChannel KeyPower = (currentChannel, Just $ SingleKey POWER)
handleSuperModifiedKey PC KeySwitchChannel = (AppleTV, Just $ SingleKey VIDEO5)
handleSuperModifiedKey AppleTV KeySwitchChannel = (PC, Just $ SingleKey VIDEO4)
handleSuperModifiedKey currentChannel _ = (currentChannel, Nothing)

data SuperKeyState = SuperHeld | SuperReleased deriving Eq

handleSuper :: (EvdevReader m, ActionSender m) => ActionChan -> EvdevHandle -> CurrentChannel -> m (CurrentChannel, SuperKeyState)
handleSuper events f currentChannel = do
    e <- readEvent f
    let (newChannel, actionKey) = case e of
          KeyEvent _ key Depressed -> handleSuperModifiedKey currentChannel key
          KeyEvent _ key Repeated -> handleSuperModifiedKey currentChannel key
          _ -> (currentChannel, Nothing)

    sendAction events actionKey

    let superKeyState = case e of
          KeyEvent _ KeySuper Released -> SuperReleased
          _ -> SuperHeld

    return (newChannel, superKeyState)

superPressed :: (EvdevReader m, ActionSender m) => CurrentChannel -> ActionChan -> EvdevHandle -> m CurrentChannel
superPressed currentChannel events f = fst <$> iterateUntilM ((== SuperReleased) . snd) (handleSuper events f . fst) (currentChannel, SuperHeld)

translateKey :: Key -> Maybe ActionKey
translateKey KeyVolDown = Just $ RepeatingKey VOLUMEDOWN
translateKey KeyVolUp = Just $ RepeatingKey VOLUMEUP
translateKey _ = Nothing

translateDepressedKey :: (EvdevReader m, ActionSender m) => CurrentChannel -> ActionChan -> EvdevHandle -> Key -> m CurrentChannel
translateDepressedKey currentChannel events f KeySuper = superPressed currentChannel events f
translateDepressedKey currentChannel events _ key = do
  sendAction events $ translateKey key
  return currentChannel

translateEvent :: (EvdevReader m, ActionSender m) => CurrentChannel -> ActionChan -> EvdevHandle -> Event -> m CurrentChannel
translateEvent currentChannel events f (KeyEvent _ key Depressed) = translateDepressedKey currentChannel events f key
translateEvent currentChannel events _ (KeyEvent _ key Repeated) = do
  sendAction events $ translateKey key
  return currentChannel
translateEvent currentChannel events _ (KeyEvent _ _ Released) = do
  sendStop events
  return currentChannel
translateEvent currentChannel _ _ _ = return currentChannel

readSingleEvent :: (EvdevReader m, ActionSender m) => ActionChan -> EvdevHandle -> CurrentChannel -> m CurrentChannel
readSingleEvent events f currentChannel = readEvent f >>= translateEvent currentChannel events f

readEvents :: (EvdevReader m, ActionSender m) => ActionChan -> EvdevHandle -> m ()
readEvents events f = iterateM_ (readSingleEvent events f) PC

seconds :: Int -> Int
seconds n = n * (10 ^ (6 :: Int))

untilSuccessful :: (Sleeper m, StdoutWriter m, RunProcess m) => String -> [String] -> m ()
untilSuccessful command args = void $ iterateUntil (== ExitSuccess) $ do
    writeStdout "Waiting for "
    writeStdoutLn command
    exitCode <- runProcessWithExitCode command args
    sleep $ seconds 1
    return exitCode

stopService :: RunProcess m => ServiceName -> m ()
stopService (ServiceName name) = void $ runProcessWithExitCode "service" [name, "stop"]

startService :: RunProcess m => ServiceName -> m ()
startService (ServiceName name) = runProcessOrFail "service" [name, "start"]

translateEvents :: (EvdevReader m, ActionSender m, FileReader m) => ActionChan -> m ()
translateEvents events = withEvdev "usb-Logitech_USB_Receiver-if02-event-mouse" $ readEvents events

-- Disable iguanaIR and lirc services with:
--  sudo update-rc.d iguanaIR disable
--  sudo update-rc.d lirc disable

newtype ServiceName = ServiceName String

iguanaService, lircService :: ServiceName
iguanaService = ServiceName "iguanaIR"
lircService = ServiceName "lirc"

filesAllExist :: DirectoryReader m => [String] -> m Bool
filesAllExist files = andM (doesFileExist <$> files)

newtype VendorId = VendorId String
newtype ProductId = ProductId String

pattern IguanaVendorId = VendorId "1781\n"
pattern IguanaProductId = ProductId "0938\n"

isIguanaDevice :: VendorId -> ProductId -> Bool
isIguanaDevice IguanaVendorId IguanaProductId = True
isIguanaDevice _ _ = False

resetUSBDevice :: (StdoutWriter m, FileWriter m, Sleeper m) => String -> m ()
resetUSBDevice devicePath = do
    let authorizedPath = devicePath ++ "/authorized"

    writeStdoutLn "Resetting IR transceiver"
    writeToFile authorizedPath "0\n"
    sleep $ seconds 1
    writeToFile authorizedPath "1\n"
    sleep $ seconds 1

newtype Device = Device String

resetIfIguanaDevice :: (StdoutWriter m, FileReader m, FileWriter m, Sleeper m, DirectoryReader m) =>
                       Device -> m ()
resetIfIguanaDevice (Device device) = do
    let devicePath = deviceDir ++ "/" ++ device
    let vendorIdFile = devicePath ++ "/idVendor"
    let productIdFile = devicePath ++ "/idProduct"

    whenM (filesAllExist [vendorIdFile, productIdFile]) $ do
        vendorId <- VendorId <$> readFromFile vendorIdFile
        productId <- ProductId <$> readFromFile productIdFile

        when (isIguanaDevice vendorId productId) $ resetUSBDevice devicePath

deviceDir :: String
deviceDir = "/sys/bus/usb/devices"

processEvent :: (Consumer m, CurrentTime m, IrSender m, StdoutWriter m) => ActionChan -> Maybe CurrentKey -> m (Maybe CurrentKey)
processEvent (ActionChan events) current = readChan events >>= runAction current

stopServices :: RunProcess m => m ()
stopServices = stopService iguanaService >> stopService lircService

resetIguanaDevices :: (StdoutWriter m, Sleeper m, DirectoryReader m, FileWriter m, FileReader m) => m ()
resetIguanaDevices = do
    usbDevices <- (\\ [".", ".."]) <$> getDirectoryContents deviceDir

    mapM_ resetIfIguanaDevice (Device <$> usbDevices)

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
    events <- ActionChan <$> newChan
    void $ fork $ translateEvents events

    iterateM_ (processEvent events) Nothing

main :: IO ()
main = do
    stopServices
    resetIguanaDevices
    startServices
    void $ irstart $ SingleKey POWERON
    runThreads
