import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Array
import Data.Char
import Data.List
import Data.Ratio
import System.Environment (getArgs)

import qualified Haskore.Melody as Melody
import qualified Haskore.Music.GeneralMIDI as MidiMusic
import qualified Haskore.Music as Music
import qualified Haskore.Basic.Pitch as Pitch
import qualified Haskore.Basic.Duration as Duration

import Haskore.Interface.MIDI.Render

tone :: [Pitch.Class] -> Int -> Duration.T -> attr -> Melody.T attr
tone scale n = Melody.note' (scale !! i) o
    where
        (o, i) = divMod n $ length scale

fromInterval :: [Pitch.Class] -> [Int] -> Music.T (Melody.Note ())
fromInterval scale = group
    >>> map (head &&& length
             >>> tone scale *** (Duration.en*) . fromIntegral
             >>> uncurry id >>> ($()))
    >>> Music.line

main = getArgs >>= \args -> case args of
    (method:profile:path:_) -> do
        (tempo, trans)   <- readIO =<< readFile profile
        (scale, mapping) <- readIO =<< readFile path
        phrases          <- lines <$> getContents
        let f = case method of {
            "playWinNT"    -> const $ void . playWinNT;
            "playLinux"    -> const $ void . playLinux;
            "playAlsa"     -> const $ void . playAlsa;
            "playTimidity" -> const $ void . playTimidity;
            "save"         -> fileFromGeneralMIDIMusic . (++".mid") }
        forM_ phrases $ \phrase -> f phrase
            $ make tempo trans scale mapping phrase
    _ -> putStrLn "Usage: wordtomelody [playWinNT|playLinux|playAlsa|playTimidity|save] <profile> <mapping>"

make tempo trans scale mapping = id
    . MidiMusic.fromMelodyNullAttr MidiMusic.AcousticGrandPiano
    . Music.changeTempo (Duration.fromRatio $ approxRational tempo 0.01)
    . Music.transpose trans
    . Music.line
    . intersperse Music.enr
    . map (fromInterval scale)
    . map (map $ (array ('a', 'z') mapping!) . toLower)
    . words