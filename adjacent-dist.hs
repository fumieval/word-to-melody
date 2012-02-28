{-
単語の列から、隣接するアルファベットの対の出現頻度の分布を求める
-}
import Control.Arrow
import Control.Applicative
import Data.Array
import Data.Char
import Data.List

main = print =<< filter ((/=0).snd) . assocs
        . accumArray (+) 0 (('a', 'a'), ('z', 'z')) . flip zip (repeat 1)
            <$> concatMap (map asc . uncurry zip . (id &&& tail) . map toLower)
            <$> lines <$> getContents
    where
        asc (a, b)
            | a <= b = (a, b)
            | otherwise = (b, a)