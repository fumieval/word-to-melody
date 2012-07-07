
import Control.Monad (replicateM)
import Control.Applicative
import Control.Arrow
import Data.Function (on)
import Data.List (sortBy)
import Data.Array
import Data.Tuple (swap)
import Data.Char (toLower)
import System.Random
import System.Environment (getArgs)
import System.IO (stderr)
import System.Time (getClockTime, diffClockTimes, TimeDiff, ClockTime
                    ,tdYear, tdMonth, tdHour, tdMin, tdSec)
import Text.Printf (printf, hPrintf)

import qualified Haskore.Basic.Pitch as Pitch

type Mapping = Array Char Int

dom = ('a', 'z') -- アルファベットの範囲

fitness :: [Pitch.Class] -> [String] -> Mapping -> Float
fitness scale ws m = sum $ map (valueMelody scale . map (m!)) $ ws

valueMelody :: [Pitch.Class] -> [Int] -> Float
valueMelody scale (x:xs) = f xs x (-1) (0, 0, 0)
    where
        oct = length scale 
        f (x:xs) p q (b, c, d)
            | m > oct = 0.0
            | b >= 1 && m == 0 = 0.0
            | m == 0    = f' (b + 1, c, d)
            | m == 1    = f' (b, c + 1, d)
            | otherwise = f' (b, c, d + 1)
            where
                m = abs (p - x)
                f' = f xs x p 
        
        f [] p q (b, c, d) = case (p `mod` oct == 0, q == p - 1) of
            (True, True) -> 1.2 * base
            (True, False) -> 1.1 * base
            _ -> base
            where
                base = -1 + (fromIntegral c / fromIntegral (c + d) - 0.8)

main = getArgs >>= \args -> case args of
    (paramFile:scaleFile:_) -> do
        (n, g, c, m, e) <- readIO =<< readFile paramFile
        (scale, cod)    <- readIO =<< readFile scaleFile
        samples         <- lines <$> getContents
        
        let f = fitness scale $ map (map toLower) samples
        
        putStrLn $ "Samples:" ++ show (length samples)
        
        origin <- sortBy (on compare f) <$> firstGeneration dom cod n
        progbar <- getProgressBar "Waiting for the convergence:" g
        result <- head . snd <$> compute f origin cod progbar c m e !! g
        
        progbar g
        print (scale, assocs result)
             
    _ -> print "Usage: dist-map <param> <scale>"
    
    where
        compute f origin cod progbar c m e = scanl (>>=) (return (0, origin))
            $ repeat $ progress *** next >>> uncurry (liftA2 (,))
            where
                progress = progbar &&& return . (+1) >>> uncurry (>>)
                next = nextGeneration c m f
                    `id` selectRanking e
                    `id` crossoverUniform dom
                    `id` mutate dom cod

firstGeneration :: (Ix d, Random d, Random c) => (d, d) -> (c, c)
    -> Int -> IO [Array d c]

firstGeneration d c n = replicateM n $ listArray d
    <$> (rangeSize d `replicateM` randomRIO c)

getProgressBar :: Integral a => String -> a -> IO (a -> IO ())
getProgressBar title total = f <$> getClockTime
    where        
        f start n
            | n >= total = display start 1.0 >> putStrLn ""
            | otherwise = display start $ fromIntegral n / fromIntegral total

        fmt = "\r%-15s %3d%% |%s| Time: %s"
        len = 40

        display :: RealFrac a => ClockTime -> a -> IO ()
        display start p = getClockTime
            >>= hPrintf stderr fmt title (floor $ p * 100 :: Int) bar
            . showTime . flip diffClockTimes start
            where
                bar = let n = floor $ p * fromIntegral len in
                          replicate n '#' ++ replicate (len - n) ' '
        
        showTime :: TimeDiff -> String
        showTime elapsed = printf "%02d:%02d:%02d" hour minute second
            where
                minute  = tdMin elapsed
                second  = tdSec elapsed
                hour    = (tdYear elapsed * 365 * 24)
                            * (tdMonth elapsed * 30)
                            * (tdHour elapsed)
						

selectRanking :: Float -> [a] -> IO a
selectRanking e xs = (xs!!) . floor . (* fromIntegral (length xs))
    . exp . (*e) . log
    <$> randomRIO (0.0, 1.0)

mutate :: (Ix d, Random d, Random c) => (d, d) -> (c, c)
    -> Array d c -> IO (Array d c)

mutate dom cod = (<$> fmap pure point) . (//)
    where point = (,) <$> randomRIO dom <*> randomRIO cod

crossoverUniform :: Ix d => (d, d)
    -> (Array d e, Array d e) -> IO (Array d e, Array d e)

crossoverUniform dom = elems *** elems >>> uncurry zip
    >>> map pure >>> zipWith (<*>) (repeat ch)
    >>> sequence >>> fmap (unzip >>> listArray dom *** listArray dom)
    where ch = ([id, swap]!!) <$> randomRIO (0, 1)

nextGeneration :: Float -> Float -- crossover rate, mutational rate
   -> Ord b => (a -> b) -- fitness
   -> ([a] -> IO a) -- selector
   -> ((a, a) -> IO (a, a)) -- crossover
   -> (a -> IO a) -- mutation
   -> [a] -- current generation
   -> IO [a]

nextGeneration rC rM f fS fC fM p = fmap (sortBy $ on compare f)
    $ collect (length p) $ repeat $ randomRIO (0.0, 1.0) >>= operate
    where
        operate x
            | x < rM      = fmap pure $ fS p                  >>= fM
            | x < rM + rC = fmap qure $ (,) <$> fS p <*> fS p >>= fC
            | otherwise   = fmap pure $ fS p				  >>= return
            where qure (a, b) = [a, b] -- There are no meanings in 'qure'
                
        collect :: Int -> [IO [a]] -> IO [a]
        collect 0 _      = return []
        collect _ []     = return []
        collect 1 (x:_)  = take 1 <$> x
        collect n (x:xs) = x >>= \v -> (v ++) <$> collect (n - length v) xs
