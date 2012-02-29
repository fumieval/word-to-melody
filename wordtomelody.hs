import Control.Applicative
import Control.Arrow
import Data.Char
import qualified Data.Map as M

import Haskore.Melody.Standard as Melody
import Haskore.Music.GeneralMIDI as MidiMusic
import qualified Haskore.Music as Music

import Haskore.Interface.MIDI.Render

mapping = M.fromList $ [('a', 5), ('c', 7), ('b', 7), ('e', 4), ('d', 10), ('g', 7), ('f', 6), ('i', 9), ('h', 0), ('k', 0), ('j', 10), ('m', 3), ('l', 8), ('o', 5), ('n', 3), ('q', 0), ('p', 6), ('s', 2), ('r', 8), ('u', 1), ('t', 10), ('w', 6), ('v', 0), ('y', 1), ('x', 0), ('z', 2)]

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
            | l >= 8 = (en, enr, 8)
            | otherwise = (qn, qnr, 4)
    
make = MidiMusic.fromMelodyNullAttr MidiMusic.AcousticGrandPiano . (Music.changeTempo 3) . line

main = playTimidity =<< make . map fromWord <$> words . unwords . lines <$> getContents