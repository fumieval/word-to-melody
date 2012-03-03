import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Array
import Data.Char

import Haskore.Melody.Standard as Melody
import Haskore.Music.GeneralMIDI as MidiMusic
import qualified Haskore.Music as Music

import Haskore.Interface.MIDI.Render

mapping = array ('a', 'z') [
    ('a',2),('b',2),('c',-1),('d',-2),('e',-2),('f',3),('g',1)
    ,('h',5),('i',-1),('j',4),('k',6),('l',3),('m',-2),('n',0)
    ,('o',0),('p',0),('q',-2),('r',-2),('s',-2),('t',2),('u',1)
    ,('v',-2),('w',-2),('x',0),('y',7),('z',-2)
    ]

scale = [c, ef, f, gf, g, bf] -- blues scale
tone = uncurry (flip id) . (id *** (scale !!)) . flip divMod (length scale)

padding n m = (m `div` n + 1) * n - m

fromInterval (x:xs@(x':xs')) -- 音階からノートを生成する
    | x == x' = tone x qn () : fromInterval xs'
    | otherwise = tone x en () : fromInterval xs

fromInterval (x:[]) = [tone x en ()]
fromInterval [] = []

fromWord w = (line $ fromInterval $ map ((mapping!) . toLower) w) +:+ enr -- 文字列から旋律を生成する

make = MidiMusic.fromMelodyNullAttr MidiMusic.AcousticGrandPiano
    . (Music.changeTempo 3) . transpose 12 . line

main = lines <$> getContents >>= mapM_ save
    where
        save name = fileFromGeneralMIDIMusic (name ++ ".mid")
            $ make $ map fromWord $ words name