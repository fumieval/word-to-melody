import Control.Applicative
import Control.Arrow
import Data.Char
import qualified Data.Map as M

import Haskore.Melody.Standard as Melody
import Haskore.Music.GeneralMIDI as MidiMusic
import qualified Haskore.Music as Music

import Haskore.Interface.MIDI.Render

mapping = M.fromList $ [('a',2),('b',2),('c',-1),('d',-2),('e',-2),('f',3),('g',1),('h',5),('i',-1),('j',4),('k',6),('l',3),('m',-2),('n',0),('o',0),('p',0),('q',-2),('r',-2),('s',-2),('t',2),('u',1),('v',-2),('w',-2),('x',0),('y',7),('z',-2)] -- [('k', -2), ('m', -1), ('e', 0), ('g', 0), ('y', 0), ('c', 1), ('j', 1), ('n', 1), ('r', 1), ('z', 1), ('i', 2), ('o', 2), ('b', 3), ('v', 3), ('d', 4), ('q', 4), ('p', 4), ('t', 4), ('a', 5), ('u', 5), ('f', 6), ('l', 6), ('s', 6), ('w', 6), ('x', 6), ('h', 7)] -- 音の割り当て(一番大事！)

scale = [c, ef, f, gf, g, bf] -- blues scale
tone = uncurry (flip id) . (id *** (scale !!)) . flip divMod (length scale)

padding n m = (m `div` n + 1) * n - m

fromInterval (x:xs@(x':xs')) -- 音階からノートを生成する
    | x == x' = tone x qn () : fromInterval xs'
    | otherwise = tone x en () : fromInterval xs

fromInterval (x:[]) = [tone x en ()]
fromInterval [] = []

fromWord w = (line $ fromInterval $ map ((mapping M.!) . toLower) w) +:+ enr -- 文字列から旋律を生成する

make = MidiMusic.fromMelodyNullAttr MidiMusic.AcousticGrandPiano . (Music.changeTempo 3) . transpose 12 . line

main = lines <$> getContents >>= mapM_ (fileFromGeneralMIDIMusic . (++".mid") &&& make . map fromWord . words >>> uncurry id)