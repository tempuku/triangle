{-# LANGUAGE RecordWildCards #-}
module Lib
    (
        module Lib
    ) where
import Text.Read (readEither)
import Data.Typeable(typeOf)
import Data.Data (Typeable)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Writer


data AppEnv = AppEnv {
    rounds :: Int,
    name :: String
}

type MyApp logEntry state = StateT state (ReaderT AppEnv (WriterT [logEntry] (ExceptT String IO)))

runMyApp :: MyApp logEntry state a -> AppEnv -> state -> IO (Either String (a, [logEntry]))
runMyApp app config st = runExceptT (runWriterT (runReaderT (evalStateT app st) config))

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
    return $ area shape sideLong

getArea :: MyApp String Float Float
getArea = do
    AppEnv {..} <- ask
    when (rounds > 0) $ do
        liftIO $ putStrLn "Write shape number"
        shapeString <- liftIO getLine
        shapeNumber <- liftEither $ readEither shapeString
        shape <- liftEither $ toEnumEither shapeNumber
        
        liftIO $ putStrLn "Write side long"
        sideString <- liftIO getLine
        sideLong <- liftEither $ readEither sideString
        local_state <- flip local getArea
              $ \env -> env {
                  rounds = rounds - 1
                }
        modify' (+area shape sideLong)
        tell ["Sum equal " <> show local_state <> " on round " <> show (rounds-1) <> "\n"]
    get
