-- https://leetcode.com/problems/design-memory-allocator/description/

import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import           Data.List  (group)

initMemory :: Int -> [Int]
initMemory s = replicate s 0

allocateMemory :: [Int] -> (Int, Int) -> (Int, [Int])
allocateMemory memory sm@(s, mId) = chunkAllocate [] (group memory) sm
    where   chunkAllocate :: [[Int]] -> [[Int]] -> (Int, Int) -> (Int, [Int])
            chunkAllocate ps [] _ = (-1, concat ps)
            chunkAllocate ps (c:cs) sm@(s,mId)
                | head c == 0 && length c >= s = (length ps', ps' ++ concat ([c']++cs))
                | otherwise = chunkAllocate (ps++[c]) cs sm
                where   c' = (replicate s mId) ++ (replicate (length c - s) 0)
                        ps' = concat ps

freeMemory :: [Int] -> Int -> (Int, [Int])
freeMemory memory mId = foldl (\(x,y) a -> if a == mId then (succ x, y++[0]) else (x, y++[a])) (0,[]) memory


type IOmemory = [Int] -> IO ()

operations :: [(String, [Int])] -> IO ()
operations (action:actions) = do
    afd <- initMemory' (head $ snd action)
    op afd (actions++[("display",[0])])
    where
        op :: (IOmemory, IOmemory, IO ()) -> [(String, [Int])] -> IO ()
        op _ [] = return ()
        op afd@(a,f,d) ((action,vs):actions)
            | action == "allocate" = a vs >> d >> op afd actions
            | action == "freeMemory" = f vs >> d >> op afd actions
            | action == "display" = d >> op afd actions
        initMemory' :: Int -> IO (IOmemory, IOmemory, IO ())
        initMemory' s = do
                loc <- newIORef $ initMemory s
                return (allocateMemory' loc, freeMemory' loc, displayMemory' loc)
        allocateMemory' :: IORef [Int] -> [Int] -> IO ()
        allocateMemory' loc (size:mId:_) = do
            m <- readIORef loc
            let (i, m') = allocateMemory m (size,mId)
            print i
            writeIORef loc m'
        freeMemory' :: IORef [Int] -> [Int] -> IO ()
        freeMemory' loc (mId:_) = do
            m <- readIORef loc
            let (i, m') = freeMemory m mId
            print i
            writeIORef loc m'
        displayMemory' :: IORef [Int] -> IO ()
        displayMemory' loc = do
            m <- readIORef loc
            print m

main :: IO ()
main = do
    let
        m0 = initMemory          10
        m1 = allocateMemory      m0  (1,1)
        m2 = allocateMemory (snd m1) (1,2)
        m3 = allocateMemory (snd m2) (1,3)
        m4 = freeMemory     (snd m3) (2)
        m5 = allocateMemory (snd m4) (3,4)
        m6 = allocateMemory (snd m5) (1,1)
        m7 = allocateMemory (snd m6) (1,1)
        m8 = freeMemory     (snd m7) (1)
        m9 = allocateMemory (snd m8) (10,1)
        m10 = freeMemory    (snd m9) (7)
    print m0
    print m1
    print m2
    print m3
    print m4
    print m5
    print m6
    print m7
    print m8
    print m9
    print m10
    -------------
    let actions = zip ["Allocator", "allocate", "allocate", "allocate", "freeMemory", "allocate", "allocate", "allocate", "freeMemory", "allocate", "freeMemory"]
                      [[10], [1, 1], [1, 2], [1, 3], [2], [3, 4], [1, 1], [1, 1], [1], [10, 2], [7]]
    operations actions
