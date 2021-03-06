-- Anthony Assi, Chris Egan, Remy Pham
-- Spectrogram
import System.Environment
import Data.WAVE
import Data.Int (Int32)
import Data.List.Split (splitOn)
import Data.Complex
import Data.List
import Data.List.Split
import System.IO     

main = do arg <- getArgs
          w <- getWAVEFile (head arg)
          writeFile "piano.txt" (toSpectro w 0 100 200) 

-- Main function that takes in the args below and writes output data to file 
-- .wav -> channel -> timebins -> freqbins -> string
toSpectro :: WAVE -> Int -> Int -> Int -> String
toSpectro w i t f = (toPrint(map (\l -> getBandDB f l) (getDB (fftBins ((plexifyList (waveToChan (getSamples w))) !! i) (getBinSize (getHeader w) t)))))

--turns a list of doubles into a line of strings separated by commas
toPrintLine :: [Double] -> String
toPrintLine ds = foldr (\d a -> (show d) ++ "," ++ a) [] ds

--turns each list of doubles into a line of strings separted by commas 
toPrint :: [[Double]] -> String
toPrint ls = foldr (\l a -> (toPrintLine l) ++ "\n" ++ a) [] ls

getSamples :: WAVE -> WAVESamples
getSamples (WAVE _ w) = w

-- breaks an audio file(ds) into (n) time bins
fftBins :: [Complex Double] ->Int -> [[Complex Double]]
fftBins ds n = fst (splitAt 100 (map fft (chunksOf n ds)))

convertToInt :: Maybe Int -> Int
convertToInt Nothing = 0
convertToInt (Just s) = s

getBinSize :: WAVEHeader -> Int -> Int
getBinSize (WAVEHeader _ _ _ f) n = (convertToInt f) `div` n

--returns [[L0,L1...Ln],[R0,R1...Rn]]
waveToChan :: WAVESamples -> [[Double]]
waveToChan fs = [(foldr (\f a-> (head (frameToDoubles f)):a) [] fs),
    (foldr (\f a-> ((frameToDoubles f) !! 1):a) [] fs)]  

--returns [[L0,R0],[L1,R1]...[Ln,Rn]]
waveToDoubles :: WAVESamples -> [[Double]]
waveToDoubles []= []
waveToDoubles (f:fs) = (frameToDoubles f):(waveToDoubles fs)

--returns [Lx,Rx]
frameToDoubles :: [WAVESample] -> [Double]
frameToDoubles [] = []
frameToDoubles (s:ss) = (sampleToDouble s):(frameToDoubles ss)

--DB =10*log10(sqrt(re^2 + im^2))
getDB :: [[Complex Double]] -> [[Double]]
getDB ls = map (\ds -> (map (\d -> 10*(logBase 10 (sqrt(((realPart d)^2)+((imagPart d)^2))))) ds)) ls

toComplex :: Double -> Complex Double
toComplex x = x :+ 0

plexifyList :: [[Double]] -> [[Complex Double]]
plexifyList xs = (toComplex <$>) <$> xs

--returns list b DB's for b freq bin from 0-20,000 Hz
getBandDB :: Int -> [Double] -> [Double]
getBandDB b f = map sum fftSplit
    where n = length f
          fs = 44100
          minI = quot (20 * n) fs  -- min index when f=20Hz
          maxI = quot (20000 * n) fs  -- max index when f=20kHz
          fft = fst (splitAt maxI f)  --ret freqs < 20,000 Hz 
          fftSplit = chunksOf (quot (length fft) b) fft  --splits fft to b+1 bins 

getHeader :: WAVE -> WAVEHeader
getHeader (WAVE h _) = h

--prints .wav file metadata 
headerToString :: WAVEHeader -> String
headerToString (WAVEHeader c fps bps f) = "Number of channels: "++ show c
    ++ "\nSample rate: " ++ show fps ++ "\nBit depth: "
    ++ show bps ++ "\nNumber of frames: " ++ show f ++ "\n"

-- Cooley-Tukey
-- Code taken from "rosettacode.org/wiki/Fast_Fourier_transform#Haskell"
fft :: [Complex Double] -> [Complex Double]
fft [] = []
fft [x] = [x]
fft xs = zipWith (+) ys ts ++ zipWith (-) ys ts
    where n = length xs
          ys = fft evens
          zs = fft odds 
          (evens, odds) = split xs
          split [] = ([], [])
          split [x] = ([x], [])
          split (x:y:xs) = (x:xt, y:yt) where (xt, yt) = split xs
          ts = zipWith (\z k -> exp' k n * z) zs [0..]
          exp' k n = cis $ -2 * pi * (fromIntegral k) / (fromIntegral n)
