{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}

module Main where

import Data.Array 
import Data.List
import Data.Maybe

data Player  = Empty | One | Two deriving (Eq, Ord)
data Board = Board Int Int (Array Int Player) [[(Int, Int)]]

instance Show Player where
  show Empty = " "
  show One = "X"
  show Two = "O"

nextplayer One = Two
nextplayer Two = One

instance Show Board where
  show b = unlines (map srow (reverse [0..rn-1])) ++ nums
    where Board rn cn xs regs = b
          row r = map (\c -> slot b r c) [0..cn-1]
          srow r = "|" ++ (intercalate "|" $ map show $ row r) ++ "|"
          nums = " " ++ (intercalate " " $ map show [0..cn-1]) ++ " "
      
bbounds b = (rn, cn) where Board rn cn _ _ = b
blank rn cn = Board rn cn xs (regions rn cn)
  where xs = listArray (0, rn*cn-1) (replicate (rn*cn) Empty)
slot b r c = xs ! ((r * cn) + c)                           
  where Board rn cn xs regs = b
slotput b r c v = Board rn cn (xs // [((r*cn)+c, v)]) regs 
  where Board rn cn xs regs = b
validcol b c = (0 <= c) && (c < cn) where (_, cn) = bbounds b
available b = filter (\c -> Empty == slot b (rn-1) c) [0..cn-1]
  where (rn, cn) = bbounds b

safeput :: Board -> Int -> Player -> Maybe Board
safeput b c v
  | validcol b c && isJust mr = Just $ slotput b (fromJust mr) c v
  | otherwise                 = Nothing
  where (rn, _) = bbounds b
        mr = find (\r -> Empty == slot b r c) [0..rn-1]

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

winner b 
  | winregs One = Just One
  | winregs Two = Just Two
  | otherwise   = Nothing
  where Board rn cn xs regs = b
        winreg pl reg = all (\(r, c) -> pl == slot b r c) reg
        winregs pl = any (\reg -> winreg pl reg) regs
iswon b = isJust $ winner b

count y xs = count' y xs 0
count' _ [] n = n
count' y (x:xs) !n 
  | (x == y) = count' y xs (n+1) 
  | otherwise = count' y xs n

infty  = 2^28

scoreboard b pl
  | (w == Just pl1) = infty
  | (w == Just pl2) = -infty
  | otherwise = sum [scorereg reg | reg <- regs]
  where
    Board _ _ _ regs = b
    pl1 = pl
    pl2 = nextplayer pl
    w = winner b
    scorereg reg 
      | s1 > 0  && s2 == 0 = s1*s1
      | s1 == 0 && s2 > 0  = -s2*s2
      | otherwise          = 0
      where
        vals = [slot b r c | (r, c) <- reg]
        s1 = count pl1 vals
        s2 = count pl2 vals

minimax :: Board -> Int -> Player -> (Int, [Int])
minimax b depth pl 
  | depth == 0 || iswon b = (scoreboard b pl, [])
  | otherwise             = maximum [movec c | c <- available b]
  where
    pl' = nextplayer pl
    movec c = (-n, c:cs)
      where
        b' = fromJust $ safeput b c pl
        (n, cs) = minimax b' (depth - 1) pl'

aimove (b, pl) = return c
  where (_, (c:_)) = minimax b 4 pl

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
      Just b' -> play f g (b', nextplayer turn)

consolegetcol (b, _) = do
  putStrLn "Enter a column:" >> readcol
  where 
    readcol = do
      s <- getLine
      case reads s of
        [(c :: Int, _)] | validcol b c -> return c
        _                              -> readcol

main = play aimove consolegetcol ((blank 4 4), One)
