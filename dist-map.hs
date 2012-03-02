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

dom = ('a', 'z')
cod = (-2, 7)
oct = 6

selectRanking :: Float -> [a] -> IO a
selectRanking e xs = (xs!!) . floor . (* fromIntegral (length xs))
    . exp . (*e) . log
    <$> randomRIO (0.0, 1.0)

mutate :: Mapping -> IO Mapping
mutate = (<$> fmap pure point) . (//)
    where
        point = (,) <$> randomRIO dom <*> randomRIO cod

crossoverUniform :: (Mapping, Mapping) -> IO (Mapping, Mapping)
crossoverUniform = elems *** elems >>> uncurry zip
    >>> map pure >>> zipWith (<*>) (repeat ch)
    >>> sequence >>> fmap (unzip >>> listArray dom *** listArray dom)
    where
        ch = ([id, swap]!!) <$> randomRIO (0, 1)

nextGeneration :: Float -> Float -- crossover rate, mutational rate
   -> (Mapping -> Float) -- fitness
   -> ([Mapping] -> IO Mapping) -- selector
   -> ((Mapping, Mapping) -> IO (Mapping, Mapping)) -- crossover
   -> (Mapping -> IO Mapping) -- mutation
   -> [Mapping] -- current generation
   -> IO [Mapping]

nextGeneration rC rM f fS fC fM p = fmap (sortBy (compare `on` f))
    $ collect (length p)
    $ repeat (randomRIO (0.0, 1.0) >>= operate)
    where
        operate x
            | x < rM      = fmap pure $ fS p >>= fM
            | x < rM + rC = fmap g    $ (,) <$> fS p <*> fS p >>= fC
            | otherwise   = fmap pure $ fS p
            where
                g (a, b) = [a, b]
        
        collect 0 _     = return []
        collect _ []     = return []
        collect 1 (x:_)  = take 1 <$> x
        collect n (x:xs) = x >>= \v -> (v ++) <$> collect (n - length v) xs

firstGeneration :: Int -> IO [Mapping]
firstGeneration n = replicateM n $ listArray dom
    <$> (rangeSize dom `replicateM` randomRIO cod)

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
    ws <- map (map toLower) <$> lines <$> getContents
    compute ws !! 50 >>= print . head
    where
        next ws = nextGeneration 0.7 0.01 (fitness ws)
        compute ws = scanl (>>=) (sortBy (compare `on` fitness ws) <$> firstGeneration 256)
            $ repeat $ next ws (selectRanking 2) crossoverUniform mutate