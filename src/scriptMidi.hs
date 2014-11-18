import System.MIDI
import System.IO
import System.Process
import Control.Monad
import Control.Concurrent
import Safe

comment :: String -> IO ()
comment s = hPutStrLn stderr ( "# " ++ s )

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  sources <- enumerateSources
  names   <- mapM getName sources
  comment "What source do you want?"
  mapM_ (comment . \(a,b) -> show a ++ " - " ++ b) (zip [1 :: Int ..] names)
  choiceID  <- readMay `fmap` getLine :: IO (Maybe Int)
  run choiceID $ do cid <- choiceID
                    lookup cid (zip [1..] sources)

run :: Maybe Int -> Maybe Source -> IO ()
run (Just choiceID) (Just choice) = print choiceID           >> setup choice
run _                _            = comment "Invalid Option" >> main

setup :: Source -> IO ()
setup choice = do
  buttonIDs     <- newChan
  buttonStrings <- mapMChan createButton buttonIDs
  keys          <- newChan
  events        <- mergeChans buttonStrings keys
  conn          <- openSource choice (Just (maybeWriteChan buttonIDs . getID))
  start conn
  comment "Processing Events"
  void $ forkIO $ do getContents >>= writeList2Chan keys . map Keyboard . lines
                     writeChan keys (Keyboard "exit")
  updateCommands events
  stop conn

getID :: MidiEvent -> Maybe Int
getID (MidiEvent _ (MidiMessage _ (NoteOn x _))) = Just x
getID _ = Nothing

maybeWriteChan :: Chan a -> Maybe a -> IO ()
maybeWriteChan c (Just e) = writeChan c e
maybeWriteChan _ Nothing  = return ()

createButton :: Show a => a -> IO Event
createButton b = do
  comment $ show b
  return (Button (show b))

mapMChan :: (x -> IO y) -> Chan x -> IO (Chan y)
mapMChan f c1 = do
  c2 <- dupChan c1
  c3 <- newChan
  void $ forkIO $ getChanContents c2 >>= mapM_ (writeChan c3 <=< f)
  return c3

mergeChans :: Chan a -> Chan a -> IO (Chan a)
mergeChans a b = do
  a' <- dupChan a
  b' <- dupChan b
  c  <- newChan
  void $ forkIO $ getChanContents a' >>= writeList2Chan c
  void $ forkIO $ getChanContents b' >>= writeList2Chan c
  return c

respondCommand :: Command -> IO ()
-- respondCommand (Run s  ) = void $ forkIO $ callCommand s
respondCommand (Run s  ) = void $ forkIO $ void $ createProcess
                                                  ((shell s)
                                                   { delegate_ctlc = True
                                                   , std_out       = UseHandle stderr })
respondCommand (Log a b) = putStrLn a >> putStrLn b

updateCommands :: Chan Event -> IO ()
updateCommands events = getChanContents events
                    >>= mapM_           respondCommand
                      . behave          []
                      . filter          (not . isComment)
                      . filter          (/= Keyboard "")
                      . takeWhile       (/= Keyboard "exit")

isComment :: Event -> Bool
isComment (Keyboard x) = and $ zipWith (==) x "#"
isComment _            = False

-- Event definition and processing

data Event   = Keyboard String | Button     String deriving (Eq, Show)
data Command = Run      String | Log String String deriving (Eq, Show)

behave :: [(String,String)] -> [Event] -> [Command]
behave l (Button   b1 : xs) | Just c <- lookup b1 l = Run c     : behave l           (Keyboard b1 : xs)
behave l (Button   b1 : Keyboard k1     : xs)       = Log b1 k1 : behave ((b1,k1):l) xs
behave l (Keyboard __ : Button   b1     : xs)       =             behave l           (Button b1 : xs)
behave l (Button   __ : Button   b2     : xs)       =             behave l           (Button b2 : xs)
behave l (Keyboard k1 : Keyboard k2     : xs)       = Log k1 k2 : behave ((k1,k2):l) xs
behave _ _                                          = []
