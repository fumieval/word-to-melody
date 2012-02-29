import Control.Applicative
import Control.Arrow
import Data.Char
import qualified Data.Map as M

import Haskore.Melody.Standard as Melody
import Haskore.Music.GeneralMIDI as MidiMusic
import qualified Haskore.Music as Music

import Haskore.Interface.MIDI.Render

mapping = M.fromList $ [('g', -2), ('m', -2), ('j', -1), ('l', -1), ('s', -1), ('v', -1), ('e', 0), ('o', 0), ('y', 0), ('u', 1), ('h', 2), ('r', 3), ('x', 3), ('z', 3), ('n', 4), ('a', 5), ('i', 5), ('d', 6), ('f', 6), ('k', 6), ('p', 6), ('t', 6), ('w', 6), ('c', 7), ('b', 8), ('q', 8)]

scale = [c, ef, f, gf, g, bf] -- blues scale
tone = uncurry (flip id) . (id *** (scale !!)) . flip divMod (length scale)

padding n m = (m `div` n + 1) * n - m

fromWord w = line
    $ map (($()) . ($note) . tone . (mapping M.!) . toLower) w
    ++ padding m l `replicate` rest
    where
        l = length w
        (note, rest, m)
            | l >= 16 = (sn, snr, 16)
            | otherwise = (en, enr, 8)
    
make = MidiMusic.fromMelodyNullAttr MidiMusic.AcousticGrandPiano . (Music.changeTempo 3) . line

main = playTimidity =<< make . map fromWord <$> words . unwords . lines <$> getContents