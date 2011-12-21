{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Array 
import Data.List
import Data.Maybe
import System.IO

-- Data definitions

-- Slots can be empty, or can have player one or player two
data Player  = Empty | One | Two deriving (Eq, Ord)

instance Show Player where
  show Empty = " "
  show One = "X"
  show Two = "O"

other One = Two
other Two = One


-- A board is a rectangular space with slots.  We also record what regions of
-- four will result in a win.

data Board = Board Int Int (Array Int Player) [[(Int, Int)]]

blank rn cn = Board rn cn xs (regions rn cn)
  where xs = listArray (0, rn*cn-1) (replicate (rn*cn) Empty)

instance Show Board where
  show b = unlines (map srow (reverse [0..rn-1])) ++ nums
    where Board rn cn xs regs = b
          row r = map (\c -> slot b r c) [0..cn-1]
          srow r = "|" ++ (intercalate "|" $ map show $ row r) ++ "|"
          nums = " " ++ (intercalate " " $ map show [0..cn-1]) ++ " "
      
-- Raw slot read/write
slot b r c = xs ! ((r * cn) + c)                           
  where Board rn cn xs regs = b
slotput b r c v = Board rn cn (xs // [((r*cn)+c, v)]) regs 
  where Board rn cn xs regs = b

-- Checks
bbounds b = (rn, cn) where Board rn cn _ _ = b
validcol b c = (0 <= c) && (c < cn) where (_, cn) = bbounds b

-- Where can we place a piece?
available b = filter (\c -> Empty == slot b (rn-1) c) [0..cn-1]
  where (rn, cn) = bbounds b

-- Place a piece in a column.
safeput :: Board -> Int -> Player -> Maybe Board
safeput b c v
  | validcol b c && isJust mr = Just $ slotput b (fromJust mr) c v
  | otherwise                 = Nothing
  where (rn, _) = bbounds b
        mr = find (\r -> Empty == slot b r c) [0..rn-1]

-- The regions of four on the board.
regions :: Int -> Int -> [[(Int, Int)]]
regions rn cn = let
    horiz r c = [(r, c + i) | i <- [0..3]]
    verti r c = [(r + i, c) | i <- [0..3]]
    diag1 r c = [(r + i, c + i)  | i <- [0..3]]
    diag2 r c = [(r - i, c + i)  | i <- [0..3]]
  in
    concat [
        [horiz r c | r <- [0..rn-1], c <- [0..cn-4]]
      , [verti r c | r <- [0..rn-4], c <- [0..cn-1]]
      , [diag1 r c | r <- [0..rn-4], c <- [0..cn-4]]
      , [diag2 r c | r <- [3..rn-1], c <- [0..cn-4]]
      ]


-- minimax AI
--   TODO: add pruning?

-- Does the board have a winner?
winner b 
  | haswin One = Just One
  | haswin Two = Just Two
  | otherwise   = Nothing
  where Board rn cn xs regs = b
        wonhere pl reg = all (\(r, c) -> pl == slot b r c) reg
        haswin pl = any (\reg -> wonhere pl reg) regs
finished b = isJust $ winner b

count y xs = foldl' (\n x -> if x == y then n + 1 else n) 0 xs

-- Large value that won't overflow Int.
infty  = 2^28

-- Heuristic scoring function on a board, with respect to a player.
scoreboard b pl
  | (w == Just pl) = infty
  | (w == Just (other pl)) = - infty
  | otherwise = sum [scorereg reg | reg <- regs]
  where
    Board _ _ _ regs = b
    w = winner b
    scorereg reg 
      | s1 > 0  && s2 == 0 = s1^3
      | s1 == 0 && s2 > 0  = - s2^3
      | otherwise          = 0
      where
        vals = [slot b r c | (r, c) <- reg]
        s1 = count pl vals
        s2 = count (other pl) vals

-- Recursive minimax algorithm.
minimax :: Board -> Int -> Player -> (Int, [Int])
minimax b depth pl 
  | depth == 0 || finished b = (scoreboard b pl, [])
  | otherwise             = maximum [movec c | c <- available b]
  where
    movec c = (-n, c:cs)
      where
        b' = fromJust $ safeput b c pl
        (n, cs) = minimax b' (depth - 1) (other pl)

-- interaction

type GameState = (Board, Player)
type Controller = GameState -> IO Int

getinput prompt validate = do
    putStr prompt >> hFlush stdout >> f
  where f = do
          s <- getLine 
          case reads s of
            [(x, _)] | validate x -> return x
            _                     -> f

play :: Controller -> Controller -> GameState -> IO ()
play f g s = let (b, pl) = s in 
  if finished b then do
    putStrLn ""
    putStrLn (show b) >> putStrLn "Game over."
  else do
    putStrLn ""
    putStrLn $ "It is the " ++ show pl ++ "'s turn."
    putStrLn $ show b
    c <- case pl of One -> f s; Two -> g s
    case safeput b c pl of
      Nothing -> putStrLn "Invalid command\n" >> play f g s
      Just b' -> play f g (b', other pl)

human (b, _) = getinput prompt validate
  where Board _ cn _ _ = b
        validate c = validcol b c
        prompt = "Next move? [0-" ++ show (cn-1) ++ "]: "

ai (b, pl) = return c
  where (_, (c:_)) = minimax b 4 pl

main = do
  plnum <- getinput "Go first as Xs, or second as Os [1/2]: " (\n -> n == 1 || n == 2)
  case plnum of 
    1 -> play human ai ((blank 5 7), One)
    2 -> play ai human ((blank 5 7), One)

