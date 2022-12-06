module Main (main) where

import Lib
import Options.Applicative as Opt

main :: IO ()
main = do
    config <- execParser opts
    result <- runMyApp getArea config 0
    case result of
        Right a -> putStrLn . report $ a
        Left e -> putStrLn e
    where
        opts = info (mkConfig <**> helper)
                    (fullDesc <> progDesc "Directory usage info")
        report (result, log_output) = "Area equal "
            <> show result <> "\n"
            <> foldl (<>) "" log_output

mkConfig :: Opt.Parser AppEnv
mkConfig =
  AppEnv
  <$> option auto
      (metavar "ROUNDS" <> short 'r' <> long "round_number" <> value maxBound <>
       help "How much times to start the programm")
  <*> strOption
      (metavar "NAME" <> long "name" <> short 'n' <>
       help "Name for you")

