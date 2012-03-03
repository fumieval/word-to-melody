import Control.Applicative
import Control.Arrow
import Control.Monad (replicateM)
import Data.Function (on)
import Data.List (sortBy)
import Data.Array
import Data.Tuple (swap)
import Data.Char (toLower)
import System.Random
import System.Environment (getArgs)

import Control.Concurrent (threadDelay)
import System.IO (stderr)
import System.Time (getClockTime, diffClockTimes, 
                    ClockTime, tdYear, tdMonth, tdHour, tdMin, tdSec)
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
            | m == 0 = if b >= 1 then 0.0 else f xs x p (b + 1, c, d)
            | m > oct = 0.0
            | m == 1 = f xs x p (b, c + 1, d)
            | otherwise = f xs x p (b, c, d + 1)
            where
                m = abs (p - x)
        f [] p q (b, c, d)
            | p `mod` oct == 0 && q == p - 1 = base * 2.0
            | p `mod` oct == 0 = base * 1.5
            | otherwise = base
            where
                base = -1 + (fromIntegral c / fromIntegral (c + d) - 0.8)

main = do
    args <- getArgs
    case args of
         (param : tone : _) -> do
             (n, g, c, m, e) <- read <$> readFile param
             (scale, cod) <- read <$> readFile tone
             samples <- lines <$> getContents
             
             let f = fitness scale $ map (map toLower) samples
             
             putStrLn $ "Samples:" ++ show (length samples)
            
             origin <- sortBy (on compare f) <$> firstGeneration dom cod n
             progbar <- getProgressBar "Waiting for the convergence:" g
             result <- head . snd <$> compute f origin cod progbar c m e !! g
            
             progbar g
             putStrLn ""
             print (scale, assocs result)
         _ -> print "Usage: dist-map <param> <tone>"
    where
        compute f origin cod progbar c m e = scanl (>>=) (return (0, origin))
            $ repeat $ progress *** next >>> uncurry ((<*>) . fmap (,))
            where
                progress = progbar &&& return . (+1) >>> uncurry (>>)
                next = nextGeneration c m f
                    `id` selectRanking e
                    `id` crossoverUniform dom
                    `id` mutate dom cod
        
selectRanking :: Float -> [a] -> IO a
selectRanking e xs = (xs!!) . floor . (* fromIntegral (length xs))
    . exp . (*e) . log
    <$> randomRIO (0.0, 1.0)

firstGeneration :: (Ix d, Random d, Random c) => (d,d) -> (c,c)
    -> Int -> IO [Array d c]

firstGeneration d c n = replicateM n $ listArray d
    <$> (rangeSize d `replicateM` randomRIO c)
            
mutate :: (Ix d, Random d, Random c) => (d,d) -> (c,c)
    -> Array d c -> IO (Array d c)

mutate d c = (<$> fmap pure point) . (//)
    where point = (,) <$> randomRIO d <*> randomRIO c

crossoverUniform :: Ix d => (d,d)
    -> (Array d e, Array d e) -> IO (Array d e, Array d e)

crossoverUniform d = elems *** elems >>> uncurry zip
    >>> map pure >>> zipWith (<*>) (repeat ch)
    >>> sequence >>> fmap (unzip >>> listArray d *** listArray d)
    where
        ch = ([id, swap]!!) <$> randomRIO (0, 1)

nextGeneration :: (Ix d, Random d, Random c)
    => Float -> Float -- crossover rate, mutational rate
   -> Ord a => (Array d c -> a) -- fitness
   -> ([Array d c] -> IO (Array d c)) -- selector
   -> ((Array d c, Array d c) -> IO (Array d c, Array d c)) -- crossover
   -> (Array d c -> IO (Array d c)) -- mutation
   -> [Array d c] -- current generation
   -> IO [Array d c]

nextGeneration rC rM f fS fC fM p = fmap (sortBy $ on compare f)
    $ collect (length p) $ repeat $ randomRIO (0.0, 1.0) >>= operate
    where
        operate x
            | x < rM      = fmap pure $ fS p >>= fM
            | x < rM + rC = fmap qure $ (,) <$> fS p <*> fS p >>= fC
            | otherwise   = fmap pure $ fS p
            where
                qure (a, b) = [a, b] -- There are no meanings in 'qure'
        
        collect 0 _      = return []
        collect _ []     = return []
        collect 1 (x:_)  = take 1 <$> x
        collect n (x:xs) = x >>= \v -> (v ++) <$> collect (n - length v) xs

-- http://d.hatena.ne.jp/ha-tan/20061217/1166316640

getProgressBar :: String -> Int -> IO (Int -> IO ())
getProgressBar title total = return . f =<< getClockTime
  where
    f start n
      | n > total = f' start 100 >> putStrLn ""
      | otherwise = f' start $ n * 100 `div` total

    f' :: ClockTime -> Int -> IO ()
    f' start percent = do
      now <- getClockTime
      hPrintf stderr "\r%-15s %3d%% |%s| Time: %s" title percent bar $ time now
      where
        bar =
          let max = 40
              n   = percent * max `div` 100
          in (replicate n '#') ++ (replicate (max - n) ' ')

        time :: ClockTime -> String
        time now =
          let elapsed = diffClockTimes now start
              hour    = (tdYear elapsed * 365 * 24) * 
                          (tdMonth elapsed * 30) * 
                          (tdHour elapsed)
              min     = tdMin elapsed
              sec     = tdSec elapsed
          in printf "%02d:%02d:%02d" hour min sec