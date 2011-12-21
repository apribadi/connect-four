{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}

module Main where

import Data.Array 
import Data.List
import Data.Maybe

data Player  = Empty | One | Two deriving (Eq, Ord)

instance Show Player where
  show Empty = " "
  show One = "X"
  show Two = "O"

other One = Two
other Two = One

data Board = Board Int Int (Array Int Player) [[(Int, Int)]]

-- Constructor
blank rn cn = Board rn cn xs (regions rn cn)
  where xs = listArray (0, rn*cn-1) (replicate (rn*cn) Empty)

instance Show Board where
  show b = unlines (map srow (reverse [0..rn-1])) ++ nums
    where Board rn cn xs regs = b
          row r = map (\c -> slot b r c) [0..cn-1]
          srow r = "|" ++ (intercalate "|" $ map show $ row r) ++ "|"
          nums = " " ++ (intercalate " " $ map show [0..cn-1]) ++ " "
      
-- slot access
slot b r c = xs ! ((r * cn) + c)                           
  where Board rn cn xs regs = b
slotput b r c v = Board rn cn (xs // [((r*cn)+c, v)]) regs 
  where Board rn cn xs regs = b

-- validation
bbounds b = (rn, cn) where Board rn cn _ _ = b
validcol b c = (0 <= c) && (c < cn) where (_, cn) = bbounds b
available b = filter (\c -> Empty == slot b (rn-1) c) [0..cn-1]
  where (rn, cn) = bbounds b

-- place a piece in a column
safeput :: Board -> Int -> Player -> Maybe Board
safeput b c v
  | validcol b c && isJust mr = Just $ slotput b (fromJust mr) c v
  | otherwise                 = Nothing
  where (rn, _) = bbounds b
        mr = find (\r -> Empty == slot b r c) [0..rn-1]

-- groups of four
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
--   TODO: add pruning

winner b 
  | haswin One = Just One
  | haswin Two = Just Two
  | otherwise   = Nothing
  where Board rn cn xs regs = b
        wonhere pl reg = all (\(r, c) -> pl == slot b r c) reg
        haswin pl = any (\reg -> wonhere pl reg) regs
iswon b = isJust $ winner b

count y xs = foldl' (\n x -> if x == y then n + 1 else n) 0 xs

-- won't overflow Int
infty  = 2^28

scoreboard b pl
  | (w == Just pl) = infty
  | (w == Just (other pl)) = - infty
  | otherwise = sum [scorereg reg | reg <- regs]
  where
    Board _ _ _ regs = b
    w = winner b
    scorereg reg 
      | s1 > 0  && s2 == 0 = s1*s1
      | s1 == 0 && s2 > 0  = -s2*s2
      | otherwise          = 0
      where
        vals = [slot b r c | (r, c) <- reg]
        s1 = count pl vals
        s2 = count (other pl) vals

minimax :: Board -> Int -> Player -> (Int, [Int])
minimax b depth pl 
  | depth == 0 || iswon b = (scoreboard b pl, [])
  | otherwise             = maximum [movec c | c <- available b]
  where
    pl' = other pl
    movec c = (-n, c:cs)
      where
        b' = fromJust $ safeput b c pl
        (n, cs) = minimax b' (depth - 1) pl'

-- interaction

type GameState = (Board, Player)

play :: (GameState -> IO Int) -> (GameState -> IO Int) -> GameState -> IO ()
play f g s = let (b, turn) = s in 
  if iswon b then 
    putStrLn (show b) >> putStrLn "Game over."
  else do
    putStrLn $ "It is the " ++ show turn ++ "'s turn."
    putStrLn $ show b
    c <- case turn of One -> f s; Two -> g s
    case safeput b c turn of
      Nothing -> play f g s
      Just b' -> play f g (b', other turn)

inputcol (b, _) = do
  putStrLn "Enter a column:" >> readcol
  where 
    readcol = do
      s <- getLine
      case reads s of
        [(c :: Int, _)] | validcol b c -> return c
        _                              -> readcol

aimove (b, pl) = return c
  where (_, (c:_)) = minimax b 4 pl

main = play inputcol aimove ((blank 5 6), One)
