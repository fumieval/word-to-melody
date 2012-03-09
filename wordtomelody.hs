import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Array
import Data.Char (toLower)
import Data.List
import Data.Ratio (approxRational)
import System.Environment (getArgs)
import Data.Maybe (fromJust)

import qualified Haskore.Melody as Melody
import qualified Haskore.Music.GeneralMIDI as GM
import qualified Haskore.Music as Music
import qualified Haskore.Basic.Pitch as Pitch
import qualified Haskore.Basic.Duration as Duration
import qualified Haskore.Interface.MIDI.Render as Render

fromInterval :: [Pitch.Class] -> [Int] -> Music.T (Melody.Note ())
fromInterval scale = group
    >>> map (head &&& length
             >>> f . (`divMod` length scale) *** (Duration.en*) . fromIntegral
             >>> uncurry id >>> ($()))
    >>> Music.line
    where
        f (o, i) = Melody.note' (scale !! i) o

make :: RealFrac a => a -> Pitch.Relative -- tempo, transpose
    -> GM.Instrument
    -> [Pitch.Class] -- scale
    -> [(Char, Int)] -- mapping
    -> String -- sentence
    -> GM.T -- melody

make tempo trans instr scale mapping = GM.fromMelodyNullAttr instr
    . Music.changeTempo (Duration.fromRatio $ approxRational tempo 0.01)
    . Music.transpose trans
    . Music.line
    . intersperse Music.enr
    . map (fromInterval scale . map (array ('a', 'z') mapping!))
    . map (filter $ inRange ('a', 'z'))
    . words . map toLower

main = getArgs >>= \args -> case args of
    (method:profile:path:_) -> do
        (patch, tempo, trans) <- readIO =<< readFile profile
        let !instr = toEnum $ fromJust $ elemIndex patch
                        $ map show $ enumFrom GM.AcousticGrandPiano
        (scale, mapping) <- readIO =<< readFile path
        phrases <- lines <$> getContents
        let f = case method of
                "playWinNT"    -> const $ void . Render.playWinNT;
                "playLinux"    -> const $ void . Render.playLinux;
                "playAlsa"     -> const $ void . Render.playAlsa;
                "playTimidity" -> const $ void . Render.playTimidity;
                "save"         -> Render.fileFromGeneralMIDIMusic . (++".mid")
                x              -> const $ const $ putStrLn "[playWinNT|playLinux|playAlsa|playTimidity|save]"
        forM_ phrases $ \phrase -> f phrase
            $ make tempo trans instr scale mapping phrase
    _ -> putStrLn "Usage: wordtomelody [playWinNT|playLinux|playAlsa|playTimidity|save] <profile> <mapping>"
