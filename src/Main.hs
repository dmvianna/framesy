{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}


module Main where

import Frames.CSV (tableTypes)
import Data.Text

tableTypes "Movies" "../data/ml-100k/u.user"

main :: IO ()
main = do
  putStrLn "hello world"
