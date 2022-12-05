module Main (main) where

import Lib
-- import Safe (toEnumMay)
-- import Text.Read (readEither)
-- import Text.Printf (printf)

main :: IO ()
main = do
    -- putStrLn "Write shape number"
    -- shapeInt <- readMaybe <$> getLine
    -- print shapeInt
    -- let shape = shapeInt >>= toEnumMay
    -- putStrLn ("Your choose: " ++ show shape)
    -- putStrLn "Write side long"
    -- sideLong <- readMaybe <$> getLine
    -- let result = area <$> shape <*> sideLong
    -- print result

    -- putStrLn "Write shape number"
    -- shapeEither <- readEither <$> getLine
    -- case shapeEither of
    --     Left e -> putStrLn ("error while reading shape number: " ++ e)
    --     Right shapeInt -> do
    --         let shapeMaybe = toEnumMay shapeInt
    --         case shapeMaybe of
    --             Nothing -> putStrLn . printf "Shape with number %d doesn't exist" $ shapeInt
    --             Just shape -> do
    --                 putStrLn ("Your choose: " ++ show shape)
    --                 putStrLn "Write side long"
    --                 sideEither <- readEither <$> getLine
    --                 case sideEither of
    --                     Left e -> putStrLn ("error while reading side long: " ++ e)
    --                     Right sideLong -> do
    --                         let result = area shape sideLong
    --                         putStrLn . (++) "Area equal " $ show result

    -- putStrLn "Write shape number"
    -- shape <- getLine
    -- putStrLn "Write side long"
    -- side <- getLine
    -- putStrLn "Write side long"
    
    result <- runMyApp getArea
    case result of
        Right a -> putStrLn  ("Area equal " <> show a)
        Left e -> putStrLn e
