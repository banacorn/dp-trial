{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified    Data.Text.IO as T
import qualified    Data.Text as T
import              Data.Text (Text)
import qualified    Data.ByteString.Lazy as B
import              Data.ByteString.Lazy (ByteString)
import qualified    Data.Map as Map
import              Data.Map (Map)
import qualified    Data.Vector as V
import              Data.Vector (Vector)
import              Data.Maybe (maybeToList)

import              Parser
import              Type

import              Control.Monad
import              System.Random.MWC
import              Statistics.Distribution
import              Statistics.Distribution.Laplace




main :: IO ()
main = do
    raw <- B.readFile "asset/sim_2.csv"
    case parse raw of
        Left err -> putStrLn err
        Right rows -> do
            let mapping = buildMapping rows

            putStrLn "全國平均就醫距離"
            pprint (nationwideAvg mapping)

            -- putStrLn "各縣市平均就醫距離"
            -- pprint (tier1Avg mapping)
            --
            -- putStrLn "各鄉鎮市區平均就醫距離"
            -- pprint (tier2Avg mapping)


            putStrLn "全國平均就醫距離 (added noise)"
            addNoise 10 (nationwideAvg mapping) >>= pprint

            -- putStrLn "各縣市平均就醫距離 (added noise)"
            -- mapM (addNoise scale) (tier1Avg mapping) >>= pprint
            --
            -- putStrLn "各鄉鎮市區平均就醫距離 (added noise)"
            -- mapM (mapM (addNoise scale)) (tier2Avg mapping) >>= pprint


        where

            esp = 0.5 -- e.g. e^0.5 ≈ 1.65× advantage over random guessing

            sensitivity = 100

            scale = sensitivity / esp


-- 1. 各鄉鎮市區的平均就醫距離
tier2Avg :: Mapping -> Tier1 Double
tier2Avg = fmap (fmap (average . V.toList . fmap distance))

-- 2. 各縣市的平均就醫距離
tier1Avg :: Mapping -> Tier2 Double
tier1Avg = fmap (average . Map.elems) . tier2Avg

-- 3. 全國的平均就醫距離
nationwideAvg :: Mapping -> Double
nationwideAvg = average . Map.elems . tier1Avg

type Mapping = Tier1 (Vector Row)
-- 全國 > 縣市
type Tier1 a = Map Text (Tier2 a)
-- 全國 > 縣市 > 鄉鎮市區
type Tier2 a = Map Text a


buildMapping :: Vector Row -> Mapping
buildMapping = foldl insertTier1 Map.empty
    where

        singletonTier2 :: Row -> Tier2 (Vector Row)
        singletonTier2 row = Map.singleton (snd (key row)) (V.singleton row)

        insertTier1 :: Mapping -> Row -> Mapping
        insertTier1 m row = Map.insertWith
            (insertTier2 row)
            (fst (key row))         -- key for Tier1
            (singletonTier2 row)    -- key for Tier1 (which is Tier2)
            m

        insertTier2 :: Row -> Tier2 (Vector Row) -> Tier2 (Vector Row) -> Tier2 (Vector Row)
        insertTier2 row _ old = Map.insertWith
            (V.++)
            (snd (key row))         -- key for Tier2
            (V.singleton row)       -- value for Tier2
            old

        key :: Row -> (Text, Text)
        key = T.splitAt 3 . townshipName . township . from

        townshipName :: Township -> Text
        townshipName (Township name _) = name


--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

average :: [Double] -> Double
average vec = sum vec / fromIntegral (length vec)

distance :: Row -> Double
distance row = sqrt ((fromX - toX) * (fromX - toX) + (fromY - toY) * (fromY - toY))
    where
        (fromX, fromY) = coord (from row)
        (toX  , toY  ) = coord (to row)

--------------------------------------------------------------------------------
-- Noise
--------------------------------------------------------------------------------

sampleLaplace :: Double -> IO Double
sampleLaplace scale = withSystemRandom . asGenST $ genContVar (laplace 0 scale)

addNoise :: Double -> Double -> IO Double
addNoise scale x = do
    noise <- sampleLaplace scale
    return (x + noise)

sampleLaplaceList :: Double -> Int -> IO [Double]
sampleLaplaceList scale times = withSystemRandom . asGenST $
    replicateM times . genContVar (laplace 0 scale)

addNoise' :: Double -> [Double] -> IO [Double]
addNoise' scale xs = do
    noise' <- sampleLaplaceList scale (length xs)
    return $ fmap (uncurry (+)) (zip xs noise')





-- 研究問題

-- 1. 各鄉鎮市區的平均就醫距離
-- 2. 各縣市的平均就醫距離
-- 3. 全國的平均就醫距離

-- 距離上限: 400 公里
