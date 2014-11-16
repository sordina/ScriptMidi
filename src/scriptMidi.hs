import System.MIDI
import System.IO
import System.Process
import Control.Monad
import Control.Concurrent
import Safe

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  sources <- enumerateSources
  names   <- mapM getName sources
  mapM_ (putStrLn . \(a,b) -> show a ++ " - " ++ b) (zip [1 :: Int ..] names)
  putStrLn "What source do you want?"
  choiceID  <- readMay `fmap` getLine :: IO (Maybe Int)
  run $ do cid <- choiceID
           lookup cid (zip [1..] sources)

run :: Maybe Source -> IO ()
run Nothing       = print "Invalid Option" >> main
run (Just choice) = setup choice

setup :: Source -> IO ()
setup choice = do
  buttonIDs     <- newChan
  buttonStrings <- mapMChan createButton buttonIDs
  keys          <- newChan
  events        <- mergeChans buttonStrings keys
  conn          <- openSource choice (Just (maybeWriteChan buttonIDs . getID))
  start conn
  print "Processing Events"
  void $ forkIO $ do getContents >>= writeList2Chan keys . map Keyboard . lines
                     writeChan keys (Keyboard "exit")
  updateCommands events
  stop conn

runCommandFromLookup :: Show a => MVar [(String, String)] -> a -> IO ()
runCommandFromLookup commands note = do
  print note
  cs <- readMVar commands
  let nid = show note
  case lookup nid cs of Nothing -> return  ()
                        Just c  -> void $ forkIO $ callCommand c

getID :: MidiEvent -> Maybe Int
getID (MidiEvent _ (MidiMessage _ (NoteOn x _))) = Just x
getID _ = Nothing

maybeWriteChan :: Chan a -> Maybe a -> IO ()
maybeWriteChan c (Just e) = writeChan c e
maybeWriteChan _ Nothing  = return ()

createButton :: Show a => a -> IO Event
createButton b = do
  print b
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

updateCommands :: Chan Event -> IO ()
updateCommands events = getChanContents events
                    >>= mapM_ (forkIO . callCommand)
                      . behave []
                      . takeWhile (/= (Keyboard "exit"))

-- Event definition and processing

data Event = Keyboard String | Button String deriving (Eq, Show)

behave :: [(String,String)] -> [Event] -> [String]
behave l (Button   b1 : xs) | Just c <- lookup b1 l = c : behave l (Keyboard b1 : xs)
behave l (Button   b1 : Keyboard k1     : xs)       = behave ((b1,k1):l) xs
behave l (Keyboard __ : Button   b1     : xs)       = behave l             (Button b1 : xs)
behave l (Button   __ : Button   b2     : xs)       = behave l             (Button b2 : xs)
behave l (Keyboard k1 : Keyboard k2     : xs)       = behave ((k1,k2):l) xs
behave _ _                                          = []
