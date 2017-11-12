{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}


module Main where

import Data.Tuple
import Control.Applicative
import qualified Control.Foldl as L
import qualified Data.Foldable as F
import Data.Proxy (Proxy(..))

import Pipes (Producer, Pipe)
import qualified Pipes.Prelude as P
import Lens.Micro.Extras (view)
import Data.Vinyl.Lens (rcast)

import Frames.Rec
import Frames.Frame
import Frames.Melt
import Frames.Exploration (select, pr)
import Frames.InCore (inCoreAoS)
import Frames.CSV ( tableTypes'
                  , separator
                  , rowGen
                  , rowTypeName
                  , readTableOpt
                  , columnNames
                  )
import Data.Text (Text)

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

minMax :: Ord a => L.Fold a (Maybe a, Maybe a)
minMax = (,) <$> L.minimum <*> L.maximum

miniUser :: User -> Record '[Occupation, Gender, Age]
miniUser = rcast

writers :: (ElemOf Occupation rs, Monad m) => Pipe (Record rs) (Record rs) m r
writers = P.filter ((=="writer") . view occupation)

main :: IO ()
main = do
  putStrLn "hello world"
