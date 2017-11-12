{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import qualified Control.Foldl as L
import qualified Data.Foldable as F
import Data.Proxy (Proxy(..))

import Pipes (Producer)
import Lens.Micro.Extras (view)

import Frames.Frame
import Frames.InCore (inCoreAoS)
import Frames.CSV ( tableTypes'
                  , separator
                  , rowGen
                  , rowTypeName
                  , readTableOpt
                  , columnNames
                  )
import Data.Text

-- tableTypes "Movies" "../data/ml-100k/u.user"

tableTypes' rowGen { rowTypeName = "User"
                   , columnNames = [ "user id", "age", "gender"
                                   , "occupation", "zip code" ]
                   , separator = "|" }
  "../data/ml-100k/u.user"

movieStream :: Producer User IO ()
movieStream = readTableOpt userParser "../data/ml-100k/u.user"

loadMovies :: IO (Frame User)
loadMovies = inCoreAoS movieStream

main :: IO ()
main = do
  putStrLn "hello world"
