import System.Environment
import Data.WAVE
import Data.Int (Int32)
import Data.List.Split (splitOn)
import Data.Complex
import Data.List
import Data.List.Split

main = do
    arg <- getArgs
    w <- getWAVEFile (head arg)
    print (getBandMag 200 (getMag (fft (head (plexifyList (waveToChan (getSamples w)))))))

getSamples :: WAVE -> WAVESamples
getSamples (WAVE _ w) = w

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

--mag = sqrt(re^2 + im^2)
getMag :: [Complex Double] -> [Double]
getMag ds = map (\d -> sqrt(((realPart d)^2)+((imagPart d)^2))) ds

toComplex :: Double -> Complex Double
toComplex x = x :+ 0

plexifyList :: [[Double]] -> [[Complex Double]]
plexifyList xs = (toComplex <$>) <$> xs

freqToMag :: Double -> [Double] -> Double
freqToMag f fft = fft !! (round (f * n / fs))
    where n =  fromIntegral (length fft)
          fs = 44100 

--returns list b+1 magnitudes for b+1 freq bin from 0-20,000 Hz
getBandMag :: Int -> [Double] -> [Double]
getBandMag b f = map sum fftSplit
    where n = length f
          fs = 44100
          minI = quot (20 * n) fs  -- min index when f=20Hz
          maxI = quot (20000 * n) fs  -- max index when f=20kHz
          fft = fst (splitAt maxI f)  --ret freqs < 20,000 Hz 
          fftSplit = chunksOf (quot (length fft) b) fft  --splits fft to b+1 bins 

--returns fundemental freq of fft
getFund :: [Double] -> Maybe Double
getFund ds = (*t) <$> i
    where n = length ds
          i = fromIntegral <$> elemIndex (maximum ds) ds
          t = 44100 / (fromIntegral n)

fftToStr :: [Double] -> String
fftToStr fft = foldr (\d a -> (show d)++"\n"++a) [] list
    where list = [freqToMag x fft | x <- [100,200..20000]]

getHeader :: WAVE -> WAVEHeader
getHeader (WAVE h _) = h

headerToString :: WAVEHeader -> String
headerToString (WAVEHeader c fps bps f) = "Number of channels: "++ show c
    ++ "\nSample rate: " ++ show fps ++ "\nBit depth: "
    ++ show bps ++ "\nNumber of frames: " ++ show f ++ "\n"

-- Cooley-Tukey
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
