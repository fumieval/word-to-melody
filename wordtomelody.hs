{-
アルファベットの列から旋律を作る
-}
import Control.Applicative
import Data.Char
import qualified Data.Map as M

import Haskore.Melody.Standard as Melody
import Haskore.Music.GeneralMIDI as MidiMusic
import qualified Haskore.Music as Music

import Haskore.Interface.MIDI.Render

mapping = M.fromList $ zip ['k', 'w', 'v', 'g', 'h', 'c', 'm', 'e', 't', 's', 'i', 'r', 'n', 'a', 'l', 'o', 'u', 'p', 'd', 'b', 'f', 'x', 'q', 'y', 'z', 'j'] [0..]

scale = [c, ef, f, gf, g, bf] -- blues scale
tone n = scale !! mod n l $ div n l - 1
    where
        l = length scale

fromChar c
    | isAlpha c = tone (mapping M.! toLower c) en ()
    | otherwise = enr

make = MidiMusic.fromMelodyNullAttr MidiMusic.AcousticGrandPiano . (Music.changeTempo 3) . line . map fromChar

main = playTimidity =<< make <$> unwords . lines <$> getContents