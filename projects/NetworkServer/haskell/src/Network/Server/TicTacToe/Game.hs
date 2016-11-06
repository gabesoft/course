module Network.Server.TicTacToe.Game where

import Control.Applicative ((<$), (<$>))
import Data.Char (isSpace, toLower, toUpper)
import Data.Foldable (find, msum)
import Data.Function (on)
import Data.IORef (atomicModifyIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.TicTacToe
import Network.Server.Common.Env
import Network.Server.Common.HandleLens
import Network.Server.Common.Lens
import Network.Server.Common.Line
import Network.Server.Common.Ref
import Network.Server.TicTacToe.Loop
import System.IO (hGetLine, hPutStrLn)

type FinishedGames = [FinishedBoard]

type Game a = IORefLoop Board (Board, FinishedGames) a

data Command
  = Move Position
  | Current
  | Finished
  | Chat String
  | Turn
  | At Position
  | Unknown String
  deriving (Eq, Show)

-- |
--
-- >>> command "MOVE ne"
-- Move NE
--
-- >>> command "MOVE 2"
-- Move N
--
-- >>> command "GAME"
-- Current
--
-- >>> command "FiniSHED"
-- Finished
--
-- >>> command "CHAT hi"
-- Chat "hi"
--
-- >>> command "Turn"
-- Turn
--
-- >>> command "At 4"
-- At W
--
-- >>> command "At C"
-- At C
--
-- >>> command "At X"
-- Unknown "At X"
--
-- >>> command "Move i"
-- Unknown "Move i"
command :: String -> Command
command z =
  let p l =
        reverse . dropWhile isSpace . reverse . dropWhile isSpace <$>
        prefixThen ((==) `on` toLower) l z
  in Unknown z `fromMaybe`
     msum
       [ do m <- p "MOVE "
            q <- sPosition m
            return (Move q)
       , Current <$ p "GAME"
       , Finished <$ p "FINISHED"
       , Chat <$> p "CHAT"
       , Turn <$ p "TURN"
       , do a <- p "AT"
            q <- sPosition a
            return (At q)
       ]

-- |
--
-- >>> sPosition "1"
-- Just NW
--
-- > sPosition "E"
-- Just E
--
-- > sPosition "sw"
-- Just SW
--
-- > sPosition "x"
-- Nothing
sPosition :: String -> Maybe Position
sPosition s =
  let table =
        [ (["1", "NW"], NW)
        , (["2", "N"], N)
        , (["3", "NE"], NE)
        , (["4", "W"], W)
        , (["5", "C"], C)
        , (["6", "E"], E)
        , (["7", "SW"], SW)
        , (["8", "S"], S)
        , (["9", "SE"], SE)
        ]
      toUppers = map toUpper
  in fmap snd . find (\(t, _) -> elem (toUppers s) (toUppers <$> t)) $ table

currentBoard :: Game Board
currentBoard = initLoop $ \env -> readIORef (envvalL `getL` env)

withCurrentBoard :: (Board -> (Board, a)) -> Game a
withCurrentBoard f = initLoop $ \env -> atomicModifyIORef (envvalL `getL` env) f

lastBoard :: Game Board
lastBoard = Loop $ \_ (s, t) -> return (s, (s, t))

putBoard :: Board -> Game ()
putBoard s = Loop $ \_ (_, t) -> return ((), (s, t))

modifyFinishedGames :: (FinishedGames -> FinishedGames) -> Game ()
modifyFinishedGames f = Loop $ \_ (s, t) -> return ((), (s, f t))

addToFinished :: FinishedBoard -> Game ()
addToFinished board = modifyFinishedGames (board :)

finishedGames :: Game FinishedGames
finishedGames = Loop $ \_ (s, t) -> return (t, (s, t))

eGetLine :: Game String
eGetLine = initLoop (hGetLine . getL handleL)

ePutStrLn :: String -> Game ()
ePutStrLn s = initLoop (\env -> (hPutStrLn (handleL `getL` env) s))

allClients :: Game (Set Ref)
allClients = initLoop $ \env -> (readIORef (clientsL `getL` env))

process :: Command -> Game ()
process (Unknown c) = pPutStrLn ("unknown command " ++ c)
process Current = currentBoard >>= pPutStrLn . show
process Finished = finishedGames >>= mapM_ (pPutStrLn . show)
process (Chat s) = allClientsButThis ! s
process (Move p) = do
  outcome <- withCurrentBoard (makeMove p)
  case outcome of
    InvalidMove -> pPutStrLn "invalid move"
    Inplay board -> pPutStrLn (show board)
    Done finished -> addToFinished finished >> pPutStrLn (show finished)

makeMove :: Position -> Board -> (Board, Outcome)
makeMove p b =
  case outcome of
    InvalidMove -> (b, outcome)
    Inplay next -> (next, outcome)
    Done _ -> (initialBoard, outcome)
  where
    outcome = move b p

initialBoard :: Board
initialBoard = start N

game
  :: Game x -- client accepted (post)
  -> (String -> Game w) -- read line from client
  -> IO a
game g f = iorefLoop initial (initial, []) g f
  where
    initial = initialBoard

play :: IO a
play = game (currentBoard >>= pPutStrLn . show) (process . command)
