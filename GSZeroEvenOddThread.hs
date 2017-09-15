-- https://www.careercup.com/question?id=5082791237648384
-- https://wiki.haskell.org/Haskell_for_multicores#Synchronisation_with_locks
-- https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/bang-patterns.html

{-|
First time trying out concurrent in Haskell.
Do PM me if there is anything to be improve.

$ ghc GSZeroEvenOddThread.hs -o GSZeroEvenOddThread-threaded --make -O2 -threaded
[1 of 1] Compiling Main             ( GSZeroEvenOddThread.hs, GSZeroEvenOddThread.o )
Linking GSZeroEvenOddThread-threaded ...
$ time ./GSZeroEvenOddThread-threaded +RTS -N2 -RTS 2GB 2GB 2GB 2GB
[0,0,0,0,0,0]
[2,4,6]
[1,3,5]

real	0m0.016s
user	0m0.004s
sys	    0m0.009s
-}

import Control.Concurrent
import Control.Monad (replicateM_)

printNrResults :: Int -> MVar String ->  IO ()
printNrResults i var = replicateM_ i (takeMVar var >>= putStrLn)

toPrint :: MVar String -> [Int] -> (Int -> Bool) -> IO ()
toPrint str i fn = do
   let h = show $ filter fn i
   putMVar str h

even' :: Int -> Bool -- 0 as non-even
even' = \x -> x /= 0 && even x

isZero :: Int -> Bool
isZero = (==) 0

main :: IO ()
main = do
   let series = [0,1,0,2,0,3,0,4,0,5,0,6]
       fn = [isZero, even', odd] -- 0 as non-even
   str <- newEmptyMVar
   mapM_ (\fn -> forkIO (toPrint str series fn)) fn
   printNrResults (length fn) str
