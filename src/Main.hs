{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified    Data.Text.IO as T
import qualified    Data.Text as T
import              Data.Text (Text)
import              Data.Text.Encoding (encodeUtf8)

import qualified    Data.ByteString.Lazy as B
import              Data.ByteString.Lazy (ByteString)
import qualified    Data.Map as Map
import              Data.Map (Map)
import qualified    Data.Vector as V
import              Data.Vector (Vector)
import              Data.Csv
import              Data.Monoid

import              Parser
import              Type

import              Control.Monad
import              Control.Monad.Primitive
import              System.Random.MWC
import              Statistics.Distribution
import              Statistics.Distribution.Laplace

type County a = Map Text a
type District a = Map Text (County a)
type FlatDistrict a = Map Text a

iteration :: Int
iteration = 1000

main :: IO ()
main = do
    raw <- B.readFile "asset/sim_2.csv"
    case parse raw of
        Left err -> putStrLn err
        Right rows -> do
            let nation = buildUp rows
            calculateNation nation
            calculateCounty nation
            calculateDistrict nation

calculateNation :: District (Vector Row) -> IO ()
calculateNation nation = do
    putStrLn "全國平均就醫距離"
    let header = V.fromList [encodeUtf8 "全國"] :: Vector Name
    let result = [NationResult (nationAvg nation)]
    B.writeFile "result/original/nationwide.csv" (encodeByName header result)

    putStrLn "全國平均就醫距離 (added noise)"
    forM_ [-9 .. 5] $ \level -> do
        results <- replicateM iteration (nationAddNoise level nation)
        B.writeFile ("result/pertubated/nationwide/2^" <> show level <> ".csv") (encodeByName header results)
--
calculateCounty :: District (Vector Row) -> IO ()
calculateCounty nation = do
    putStrLn "各縣市平均就醫距離"
    let header = V.fromList (map encodeUtf8 (Map.keys nation)) :: Vector Name
    let avgs = fmap countyAvg nation
    let result = [CountyResult avgs]
    B.writeFile "result/original/countywide.csv" (encodeByName header result)

    withSystemRandom . asGenIO $ \gen -> do
        putStrLn "各縣市平均就醫距離 (added noise)"
        forM_ [-4 .. 10] $ \level -> do
            results <- replicateM iteration (countyAddNoise level nation gen)
            B.writeFile ("result/pertubated/countywide/2^" <> show level <> ".csv") (encodeByName header results)

calculateDistrict :: District (Vector Row) -> IO ()
calculateDistrict nation = do
    let flattened = flatten nation
    putStrLn "各鄉鎮市區的平均就醫距離"
    let header = V.fromList $ map encodeUtf8 $ Map.keys flattened
    let result = [DistrictResult (flatten $ fmap (fmap districtAvg) nation)]
    B.writeFile "result/original/districtwide.csv" (encodeByName header result)

    putStrLn "各鄉鎮市區的平均就醫距離 (added noise)"

    withSystemRandom . asGenIO $ \gen -> do
        forM_ [5 .. 20] $ \level -> do
            results <- replicateM iteration (districtAddNoise level flattened gen)
            B.writeFile ("result/pertubated/districtwide/2^" <> show level <> ".csv") (encodeByName header results)


nationAddNoise :: Int -> District (Vector Row) -> IO NationResult
nationAddNoise level nation = do
    let avg = nationAvg nation
    pertubated <- addNoise scale avg
    return (NationResult pertubated)
    where
        size = Map.foldl (\s elem -> s + (Map.foldl (\s vec -> s + V.length vec) 0 elem)) 0 nation
        eps = 2.0 ^^ level
        scale = 400000.0 / (fromIntegral size * eps)
            -- pertubated <- addNoise' scale (replicate iteration (nationwideAvg mapping))

countyAddNoise :: Int -> District (Vector Row) -> Gen (PrimState IO) -> IO CountyResult
countyAddNoise level nation gen = do
    pertubated <- mapM (\county -> do
        let avg = countyAvg county
        noise <- genContVar (laplace 0 (scale county)) gen
        return (avg + noise)) nation
    return (CountyResult pertubated)

    where
        countySize = Map.foldl (\s vec -> s + V.length vec) 0
        eps = 2.0 ^^ level
        scale county = 400000.0 / (fromIntegral (countySize county) * eps)

districtAddNoise :: Int -> FlatDistrict (Vector Row) -> Gen (PrimState IO) -> IO DistrictResult
districtAddNoise level flattened gen = do
    pertubated <- mapM (\district -> do
        let avg = districtAvg district
        noise <- genContVar (laplace 0 (scale district)) gen
        return (avg + noise)) flattened
    return (DistrictResult pertubated)

    where
        districtSize = V.length -- Map.foldl (\s vec -> s + V.length vec) 0
        eps = 2.0 ^^ level
        scale district = 400000.0 / (fromIntegral (districtSize district) * eps)




data NationResult = NationResult Double deriving (Show)
data CountyResult = CountyResult (County Double) deriving (Show)
data DistrictResult = DistrictResult (FlatDistrict Double) deriving (Show)

instance ToNamedRecord NationResult where
    toNamedRecord (NationResult value) = namedRecord
        [ (encodeUtf8 "全國") .= value ]

instance ToNamedRecord CountyResult where
    toNamedRecord (CountyResult value)
        = namedRecord (map (\(key, val) -> (encodeUtf8 key) .= val) (Map.assocs value))

flatten :: District a -> FlatDistrict a
flatten = Map.fromList . concat . map fuseName . Map.assocs
    where
        fuseName :: (Text, County a) -> [(Text, a)]
        fuseName (countyName, county) = map (\(districtName, distrct) -> (countyName <> districtName, distrct)) (Map.assocs county)


instance ToNamedRecord DistrictResult where
    toNamedRecord (DistrictResult nation)
        = namedRecord
            $ Map.elems
            $ Map.mapWithKey (\name value -> encodeUtf8 name .= value)
            $ nation

districtAvg :: Vector Row -> Double
districtAvg = average . V.toList . fmap distance

countyAvg :: County (Vector Row) -> Double
countyAvg = average . Map.elems . fmap districtAvg
    -- Map.foldl (\ acc distrct -> acc + districtAvg distrct) 0.0

nationAvg :: District (Vector Row) -> Double
nationAvg = average . Map.elems . fmap countyAvg

buildUp :: Vector Row -> District (Vector Row)
buildUp = foldl insertCounty Map.empty
    where

        districtSingleton :: Row -> County (Vector Row)
        districtSingleton row = Map.singleton (snd (key row)) (V.singleton row)

        insertCounty :: District (Vector Row) -> Row -> District (Vector Row)
        insertCounty m row = Map.insertWith
            (insertDistrict row)
            (fst (key row))         -- key for TierCounty
            (districtSingleton row)    -- key for TierCounty (which is TierDistrict)
            m

        insertDistrict :: Row -> County (Vector Row) -> County (Vector Row) -> County (Vector Row)
        insertDistrict row _ old = Map.insertWith
            (V.++)
            (snd (key row))         -- key for TierDistrict
            (V.singleton row)       -- value for TierDistrict
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

-- -- 研究問題
--
-- -- 1. 各鄉鎮市區的平均就醫距離
-- -- 2. 各縣市的平均就醫距離
-- -- 3. 全國的平均就醫距離
--
-- -- 距離上限: 400 公里
