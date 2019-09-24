{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Monad
import qualified Data.ByteString.Lazy as BS
import qualified Data.Csv as CSV
import Data.Either.Combinators (fromRight')
import Data.List (group)
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import GHC.Exts (sortWith)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import System.Exit (exitFailure)
import System.Environment (getArgs)

type Time = Double

data Sample = Sample { startTime :: !Time
                     , finishTime :: !Time }
  deriving (Generic, CSV.FromRecord)

type TimeSeries a = [(Time, a)]

average :: (Fractional a, Foldable t) => t a -> a
average xs = sum xs / fromIntegral (length xs)

requestsPerSecond :: [Sample] -> TimeSeries Int
requestsPerSecond = init . map (\rs -> (fromIntegral (head rs), length rs)) . group . map (\s -> floor $ finishTime s :: Int)

smooth :: (Real a, Fractional b) => Time -> TimeSeries a -> TimeSeries b
smooth window = map windowToSample . NE.groupWith (toWindow . fst)
  where toWindow :: Time -> Int
        toWindow t = floor $ t / window
        fromWindow :: Int -> Time
        fromWindow w = fromIntegral w * window
        windowToSample :: (Real a, Fractional b) => NE.NonEmpty (Time, a) -> (Time, b)
        windowToSample samples = (time, value)
          where time = fromWindow $ toWindow $ fst $ NE.head samples
                value = average $ NE.map (realToFrac . snd) $ samples

latencies :: [Sample] -> TimeSeries Time
latencies = map $ \(Sample start finish) -> (finish, finish - start)

normalizeTime :: [Sample] -> [Sample]
normalizeTime samples = sortWith finishTime $ map (\(Sample start finish) -> Sample (normalize start) (normalize finish)) samples
  where origin :: Time
        origin = minimum $ map startTime samples
        normalize :: Time -> Time
        normalize t = (t - origin) / (1000 * 1000) -- translate microseconds to seconds

parseData :: BS.ByteString -> [Sample]
parseData input = Vector.toList $ fromRight' $ CSV.decode CSV.NoHeader input

dataForEngine :: FilePath -> IO [Sample]
dataForEngine fileName = normalizeTime <$> parseData <$> BS.readFile fileName

dataPaths :: String -> [(String, FilePath)]
dataPaths prefix = [ ("OpenJDK + Nashorn", prefix ++ "warmup")
                   , ("GraalVM + GraalVM JavaScript", prefix ++ "warmup-graalvmce-graaljs")
                   ]

main :: IO ()
main = do args <- getArgs
          case args of
            [dataRoot] -> do wrkData <- readData $ dataPaths (dataRoot ++ "/")
                             jsData <- readData $ dataPaths (dataRoot ++ "/js-")
                             toFile def (dataRoot ++ "/throughput.svg") $ do
                               layout_title .= "Talkyard throughput during warmup"
                               setupXAxis
                               setupThroughputYAxis
                               forM_ wrkData $ \(label, samples) ->
                                 plot (line label [ smooth 30 $ requestsPerSecond $ samples :: TimeSeries Double ])
                             toFile def (dataRoot ++ "/latency.svg") $ do
                               layout_title .= "Talkyard latency during warmup"
                               setupXAxis
                               setupTotalLatencyYAxis
                               forM_ wrkData $ \(label, samples) ->
                                 plot (line label [ smooth 30 $ latencies $ samples ])
                             toFile def (dataRoot ++ "/js-throughput.svg") $ do
                               layout_title .= "Throughput of Talkyard server-side rendering during warmup"
                               setupXAxis
                               setupThroughputYAxis
                               forM_ jsData $ \(label, samples) ->
                                 plot (line label [ smooth 30 $ requestsPerSecond $ samples :: TimeSeries Double ])
                             toFile def  (dataRoot ++ "/js-latency.svg") $ do
                               layout_title .= "Latency of Talkyard server-side rendering during warmup"
                               setupXAxis
                               setupJSLatencyYAxis
                               forM_ jsData $ \(label, samples) ->
                                 plot (line label [ smooth 30 $ latencies $ samples])
            _ -> do putStrLn "Please specify the data folder as a command-line argument."
                    exitFailure
  where readData :: [(String, FilePath)] -> IO [(String, [Sample])]
        readData = mapM $ \(title, path) -> do samples <- dataForEngine path
                                               return (title, samples)
        setupXAxis :: forall y. EC (Layout Time y) ()
        setupXAxis = do layout_x_axis . laxis_generate .= scaledAxis laParams (0, 30 * 60)
                        layout_x_axis . laxis_override .= const ad
          where laParams :: LinearAxisParams Time
                laParams = la_labelf .~ map showMinutes $ def
                ad :: AxisData Time
                ad = makeAxis (map showMinutes) (labels, ticks, labels)
                labels :: [Time]
                labels = [0, 5 * 60.. 30 * 60]
                ticks :: [Time]
                ticks = [0, 30.. 30 * 60]
                showMinutes :: Time -> String
                showMinutes t = leftPad 2 '0' (show minutes) ++ ":" ++ leftPad 2 '0' (show seconds)
                  where minutes :: Int
                        minutes = floor $ t / 60
                        seconds :: Int
                        seconds = floor $ t - 60 * fromIntegral minutes
                        leftPad n c str = take (n - length str) (repeat c) ++ str
        setupThroughputYAxis :: EC (Layout Time Double) ()
        setupThroughputYAxis = do layout_y_axis . laxis_generate .= autoScaledAxis laParams
                                  layout_y_axis . laxis_override .= const ad
          where laParams :: LinearAxisParams Double
                laParams = la_labelf .~ map showThroughput $ def
                ad :: AxisData Double
                ad = makeAxis (map showThroughput) (labels, ticks, labels)
                labels :: [Double]
                labels = [0, 100.. 600]
                ticks :: [Double]
                ticks = [0, 10.. 600]
                showThroughput :: Double -> String
                showThroughput throughput = show throughput ++ " req/s"
        setupLatencyYAxis :: [Double] -> [Double] -> EC (Layout Time Double) ()
        setupLatencyYAxis labels ticks = do layout_y_axis . laxis_generate .= autoScaledLogAxis logaParams
                                            layout_y_axis . laxis_override .= const ad
          where logaParams :: LogAxisParams Double
                logaParams = loga_labelf .~ map showLatency $ def
                ad :: AxisData Double
                ad = makeAxis' log exp (map showLatency) (labels, ticks, labels)
                showLatency :: Double -> String
                showLatency latency = show (latency * 1000) ++ " ms"
        setupJSLatencyYAxis :: EC (Layout Time Double) ()
        setupJSLatencyYAxis = setupLatencyYAxis labels ticks
          where labels :: [Double]
                labels = takeWhile (<= 0.5) [d * 10 ** o | o <- [-3..], d <- [1, 5]]
                ticks :: [Double]
                ticks = takeWhile (<= 0.5) [d * 10 ** o | o <- [-3..], d <- [1..9]]
        setupTotalLatencyYAxis :: EC (Layout Time Double) ()
        setupTotalLatencyYAxis = setupLatencyYAxis labels ticks
          where labels :: [Double]
                labels = takeWhile (<= 0.4) [d * 10 ** o | o <- [-2..], d <- [1, 2, 4, 6, 8]]
                ticks :: [Double]
                ticks = takeWhile (<= 0.4) [d * 10 ** o | o <- [-2..], d <- [1..9]]
