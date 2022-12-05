module Lib
    (
        module Lib
    ) where
import Text.Read (readEither)
import Data.Typeable(typeOf)
import Data.Data (Typeable)

data Shape = Triangle | Square | Circle deriving (Show, Enum, Bounded)

-- instance Show Shape where
--     show Triangle = "Triangle"
--     show Square = "Square"
--     show Circle = "Circle"

area :: Shape -> Float -> Float
area shape =
    case shape of
        Triangle -> (\x -> 0.433 * x * x)
        Square -> (\x -> x * x)
        Circle -> (\x -> p * x * x)
    where
        p = 3.142

toEnumEither :: (Bounded a, Enum a, Typeable a) => Int -> Either String a
toEnumEither i =
    let r = toEnum i
        max = maxBound `asTypeOf` r
        min = minBound `asTypeOf` r
    in if i <= fromEnum max && i >= fromEnum min
        then Right r
        else Left (show i <> " out of " <> show (typeOf max) <> " enum boundaries")

getAreaByInputs :: String -> String -> Either String Float
getAreaByInputs shapeString sideString = do
    shapeNumber <- readEither shapeString
    shape <- toEnumEither shapeNumber 
    sideLong <- readEither sideString
    return (area shape sideLong)


someFunc :: Maybe Float
someFunc = area <$> Just Triangle <*> Just 2
