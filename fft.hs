import System.Environment
import Data.WAVE
import Data.Int (Int32)
import Data.List.Split (splitOn)
import Data.Complex
import Data.List

main = do
    arg <- getArgs
    w <- getWAVEFile (head arg)
    print (getFund (getMag (fft (head (plexifyList (waveToChan (getSamples w)))))))
 --   print (headerToString (getHeader w))
getSamples :: WAVE -> WAVESamples
getSamples (WAVE _ w) = w

largest :: [Double] -> Double
largest [] = 0
largest (head : tail) = max head (largest tail)

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

getFund :: [Double] -> Maybe Double
getFund ds = (*t) <$> i
    where n = length ds
          i = fromIntegral <$> elemIndex (maximum ds) ds
          t = 44100 / (fromIntegral n)

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
