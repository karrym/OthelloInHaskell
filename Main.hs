
module Main where

import Data.List
import System.IO
import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.State

data Stone = Black | White deriving (Eq, Show)

toChar :: Stone -> Char
toChar Black = 'X'
toChar White = 'O'

type Pos = (Int, Int)
type Vec = Pos
type Board = [(Pos, Stone)]

type Action = State Board
type ActionIO = StateT Board IO

data Result = Win Stone | Draw deriving (Show)

vectors :: [Vec]
vectors = [(1,-1),(1,0),(1,1),(0,-1),(0,1),(-1,-1),(-1,0),(-1,1)]

width :: Int
width = 8

height :: Int
height = 8

widths :: [Int]
widths = [0 .. width - 1]

heights :: [Int]
heights = [0 .. height - 1]

board :: Board
board = [((width `div` 2 - 1, height `div` 2 - 1), White),
         ((width `div` 2 - 1, height `div` 2), Black),
         ((width `div` 2, height `div` 2 - 1), Black),
         ((width `div` 2, height `div` 2), White)]

addv :: Vec -> Vec -> Vec
addv (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

putStAt :: Stone -> Pos -> Action ()
putStAt s p = get >>= put . put' where
    put' [] = []
    put' ((pos, st):xs) = if p == pos
                             then (pos, s) : xs
                             else (pos, st) : put' xs

putable :: Stone -> Board -> [Pos]
putable s b = do
        x <- widths
        y <- heights
        guard $ isSpace (x, y) b && evalState (putStone s (x, y) False) b
        return $ (x, y)

score :: Stone -> Board -> Int
score s = foldl' (\n (_, st) -> if s == st then n + 1 else n) 0

isSpace :: Pos -> Board -> Bool
isSpace p = not . elem p . map fst 

getStone :: Pos -> Board -> Stone
getStone p ((pos, s):xs) = if p == pos
                               then s
                               else getStone p xs

putStoneTo :: Stone -> Pos -> Vec -> Bool -> Action Bool
putStoneTo s' p' v' set' = put' s' (p' `addv` v') v' set' [] where
    put' s p v set ps = do
        b <- get
        if (p, s) `elem` b
            then if null ps
                     then return False
                     else if set
                              then mapM_ (putStAt s) ps >> return True
                              else return True
            else if isSpace p b
                     then return False
                     else put' s (p `addv` v) v set (p:ps)

putStone :: Stone -> Pos -> Bool -> Action Bool
putStone s p set = do
        when set $ modify ((p, s):)
        or <$> mapM (flip (putStoneTo s p) set) vectors

putLine :: IO ()
putLine = do
        putStr "   -"
        mapM_ (const $ putStr "----") widths
        putChar '\n'

showBoard :: Board -> IO ()
showBoard b = do
        putStr "   "
        mapM_ (\n -> putStr "  " >> putStr (show n) >> putChar ' ') widths
        putChar '\n'
        putLine
        forM_ heights $ \y -> do
            putStr $ " " ++ show y ++ " |"
            (forM_ widths $ \x -> do
            if isSpace (x, y) b
                then putStr "   |"
                else putStr $ ' ':toChar (getStone (x, y) b):" |")
            putChar '\n'
            putLine

showMap :: ActionIO ()
showMap = get >>= liftIO . showBoard

readPos :: String -> IO Pos
readPos str = putStr str >> hFlush stdout >> read <$> getLine

player :: Stone -> ActionIO Bool
player s = do
        b <- get
        let pl = putable s b
        liftIO $ print pl
        if null pl
            then return False
            else do
                liftIO $ putStrLn $ "Your stone is " ++ show s
                p <- liftIO $ readPos "Please input position: "
                r <- callState $ putStone s p True
                if r then return True else player s

callState :: Monad m => State s a -> StateT s m a
callState m = do
        s <- get
        let (r, st) = runState m s
        put st
        return r

mainLoop :: ActionIO Result
mainLoop = do
        showMap
        rb <- player Black
        showMap
        rw <- player White
        if not rb && not rw
            then return Draw
            else do
                b <- get
                if length b == width * height
                    then return $ case score Black b `compare` score White b of
                                    LT -> Win White
                                    EQ -> Draw
                                    GT -> Win Black
                    else mainLoop

main :: IO ()
main = print =<< evalStateT (mainLoop >>= \a ->  showMap >> return a) board
