
{-# LANGUAGE OverloadedStrings #-}

module Colors (main, resetColor, textColor) where

import System.Console.ANSI

resetColor :: IO ()
resetColor = setSGR [Reset]

-- Black	 
-- Red	 
-- Green	 
-- Yellow	 
-- Blue	 
-- Magenta	 
-- Cyan	 
-- White
-- https://hackage.haskell.org/package/ansi-terminal-types-0.11.5/docs/System-Console-ANSI-Types.html

textColor :: String -> IO ()
textColor color = 
  -- map color to ANSI color
  case color of
    "Red" -> setSGR [SetColor Foreground Vivid Red]
    "Blue" -> setSGR [SetColor Foreground Vivid Blue]
    "Yellow" -> setSGR [SetColor Foreground Vivid Yellow]
    "Brown" -> setSGR [SetColor Foreground Dull Red]
    "Green" -> setSGR [SetColor Foreground Vivid Green]
    "Magenta" -> setSGR [SetColor Foreground Vivid Magenta]
    "Cyan" -> setSGR [SetColor Foreground Vivid Cyan]
    _ -> setSGR [Reset]  -- Reset to default


main :: IO ()
main = do
  setSGR [SetColor Foreground Vivid Red]
  putStrLn "This will be red"
  setSGR [Reset]  -- Reset to default

  setSGR [SetColor Foreground Vivid Blue]
  putStr "This will be Blue "
  setSGR [SetColor Foreground Vivid Yellow]
  putStrLn " and this will be Yellow "

  setSGR [Reset]  -- Reset to default