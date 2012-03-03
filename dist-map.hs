import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Function
import Data.List
import Data.Array
import Data.Tuple
import Data.Char
import System.Random

type Mapping = Array Char Int

dom = ('a', 'z') -- アルファベットの範囲
cod = (-2, 7) -- 音の範囲
oct = 6 -- 1オクターブの音の数

fitness :: [String] -> Mapping -> Float
fitness ws m = sum $ map (valueMelody . map (m!)) $ ws

valueMelody :: [Int] -> Float
valueMelody (x:xs) = f xs x (-1) (0, 0, 0)
    where
        f (x:xs) p q (b, c, d)
            | m == 0 = if b >= 2 then 0.0 else f xs x p (b + 1, c, d)
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
    f <- fitness <$> map (map toLower) <$> lines <$> getContents
    origin <- sortBy (on compare f) <$> firstGeneration dom cod 256
    print . head =<< compute f origin !! 50
    where
        compute f origin = scanl (>>=) (return origin) $ repeat $ next f
        next f = nextGeneration 0.7 0.01 f 
            `id` selectRanking 2 `id` crossoverUniform dom `id` mutate dom cod
        
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
