{-# LANGUAGE DeriveGeneric #-}

module Parser where


import qualified    Data.Text as T
import              Data.Text (Text)
import qualified    Data.ByteString.Lazy as B
import              Data.ByteString.Lazy (ByteString)
import qualified    Data.Vector as V
import              Data.Vector (Vector)
import              Data.Csv
import              GHC.Generics (Generic)

import              Type

data Raw = Raw
    {   personID :: !Text
    ,   address :: !Text
    ,   addressX :: !(Maybe Double)
    ,   addressY :: !(Maybe Double)
    ,   townshipName :: !Text
    ,   townshipX :: !Double
    ,   townshipY :: !Double
    ,   randomID :: !Double
    ,   link_ :: !Int
    ,   typ_ :: !Char
    ,   hospitalID :: !Text
    ,   hospitalAddress :: !Text
    ,   hospitalX :: !(Maybe Double)
    ,   hospitalY :: !(Maybe Double)
    ,   hospitalTownship :: !Text
    ,   hospitalTownshipX :: !Double
    ,   hospitalTownshipY :: !Double
    }
    deriving (Generic, Show)

instance FromRecord Raw
instance ToRecord Raw

parseRow :: Raw -> Maybe Row
parseRow raw = do
    fromX <- addressX raw
    fromY <- addressY raw
    let from = Place (personID raw) (address raw) (fromX, fromY)
            (Township (townshipName raw) (townshipX raw, townshipY raw))
    toX <- hospitalX raw
    toY <- hospitalY raw
    let to = Place (hospitalID raw) (hospitalAddress raw) (toX, toY)
            (Township (hospitalTownship raw) (hospitalTownshipX raw, hospitalTownshipY raw))
    return $ Row from to (link_ raw) (typ_ raw)

parse :: ByteString -> Either String (Vector Row)
parse raw = do
    rows <- decode HasHeader raw
    return (V.mapMaybe parseRow rows)
