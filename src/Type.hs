{-# LANGUAGE OverloadedStrings #-}

module Type where

import              Data.Text (Text, pack)
import qualified    Data.Text.IO as T
import qualified    Data.Text as T
import qualified    Data.Map as Map
import              Data.Map (Map)
import qualified    Data.Vector as V
import              Data.Vector (Vector)
import              Data.Monoid

--------------------------------------------------------------------------------
-- Data
--------------------------------------------------------------------------------

type Coord = (Double, Double)
type Address = Text
data Township = Township !Text Coord
    deriving (Show)
data Place = Place
    {   name :: !Text
    ,   addr :: !Address
    ,   coord :: Coord
    ,   township :: Township
    }
    deriving (Show)

data Row = Row
    {   from :: Place
    ,   to   :: Place
    ,   link :: !Int
    ,   typ :: !Char
    }
    deriving (Show)


pprint :: Pretty a => a -> IO ()
pprint v = T.putStrLn (display v)

class Pretty a where
    display :: a -> Text

instance Pretty Double where
    display x = T.pack (show x)

instance Pretty Township where
    display (Township name (x, y)) = name
        <> " ("
        <> pack (show x)
        <> ", "
        <> pack (show y)
        <> ")"

instance Pretty Place where
    display (Place name addr (x, y) (Township town _)) =
           name
        <> " ["
        <> addr
        <> " ("
        <> pack (show x)
        <> ", "
        <> pack (show y)
        <> ") #"
        <> town
        <> " ]"

instance Pretty Row where
    display (Row from to link typ) = display from
        <> " ⇒ "
        <> display to
        <> " ("
        <> pack (show link)
        <> ", "
        <> pack (show typ)
        <> ")"

instance Pretty Text where
    display = id

instance (Pretty value) => Pretty (Vector value) where
    display = foldl (\xs x -> xs <> "\n" <> x) "" . fmap display

instance (Pretty key, Pretty value) => Pretty (Map key value) where
    display = Map.foldlWithKey (\xs key x -> xs <> display key <> ":\n" <> indent (display x)) ""
        where
            indent :: Text -> Text
            indent = T.unlines . fmap ((<>) "   ") . T.lines
