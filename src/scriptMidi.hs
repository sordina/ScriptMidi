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
  dests   <- enumerateDestinations
  names   <- mapM getName sources
  dNames  <- mapM getName dests

  comment "What source do you want?"
  mapM_ (comment . \(a,b) -> show a ++ " - " ++ b) (zip [1 :: Int ..] names)

  choiceID  <- readMay `fmap` getLine :: IO (Maybe Int)

  run $ do cid    <- choiceID
           name   <- lookup cid  (zip [1..]  names)
           source <- lookup name (zip names  sources)
           return (cid, source, lookup name (zip dNames dests))

run :: Maybe (Int, Source, Maybe Destination) -> IO ()
run (Just (choiceID, source, dest)) = print choiceID           >> setup source dest
run _                               = comment "Invalid Option" >> main

maybeOpenDestination :: Maybe Destination -> IO (Maybe Connection)
maybeOpenDestination Nothing     = return Nothing
maybeOpenDestination (Just dest) = do conn <- openDestination dest
                                      start  conn
                                      return (Just conn)

setup :: Source -> Maybe Destination -> IO ()
setup source dest = do
  buttonIDs     <- newChan
  buttonStrings <- mapMChan createButton buttonIDs
  keys          <- newChan
  events        <- mergeChans buttonStrings keys
  sConn         <- openSource source (Just (maybeWriteChan buttonIDs . getID))
  dConn         <- maybeOpenDestination dest
  start sConn
  comment "Processing Events"
  void $ forkIO $ do getContents >>= writeList2Chan keys . map Keyboard . lines
                     writeChan keys (Keyboard "exit")
  respondToEvents events dConn
  stop sConn

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

respondCommand :: Maybe Connection -> Command -> IO ()
respondCommand _    (Run s  ) = void $ forkIO $ void $ createProcess
                                                       ((shell s)
                                                        { delegate_ctlc = True
                                                        , std_out       = UseHandle stderr })
respondCommand dest (Log a b) = do putStrLn a
                                   putStrLn b
                                   case dest of Nothing -> return ()
                                                -- When a binding is created, turn on the light for that binding
                                                -- Just d  -> do send d $ MidiMessage 1 $ NoteOn (read a - 67) 60
                                                Just d  -> lightUp d (read a)

lightUp :: Connection -> Int -> IO ()
lightUp conn n | n >= 68 || n <= 71   = send conn $ MidiMessage 1 $ NoteOn (n - 67) 60
               | n >= 72 || n <= 75   = send conn $ MidiMessage 1 $ NoteOn (n - 63) 60
               | n >= 76 || n <= 79   = send conn $ MidiMessage 1 $ NoteOn (n - 59) 60
               | n >= 80 || n <= 83   = send conn $ MidiMessage 1 $ NoteOn (n - 55) 60
               | otherwise            = return ()

respondToEvents :: Chan Event -> Maybe Connection -> IO ()
respondToEvents events dest = getChanContents events
                          >>= mapM_           (respondCommand dest)
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
