{-# LANGUAGE TypeFamilies #-}
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

import Pipes (Producer, Pipe, runEffect, (>->), mzero)
import qualified Pipes.Prelude as P
import Lens.Micro ((%~))
import Lens.Micro.Extras (view)
import Data.Vinyl.Lens -- ∈ comes from here

import Frames.ColumnUniverse
import Frames.Rec
import Frames.Frame
import Frames.Melt
import Frames.RecF
import Frames.Exploration (select, pr, pipePreview)
import Frames.InCore
import Frames.CSV ( tableTypes'
                  , separator
                  , rowGen
                  , rowTypeName
                  , readTableOpt
                  , columnNames
                  , colQ
                  , tablePrefix
                  , columnUniverse
                  )
import Data.Text (Text)
import Data.Typeable
import qualified Data.Text as T
import qualified Data.Char as C
import Data.Readable
import qualified Data.Vector as V

import TutorialZipCode

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

writers :: (Occupation ∈ rs, Monad m) => Pipe (Record rs) (Record rs) m r
writers = P.filter ((=="writer") . view occupation)

intFieldDoubler :: Record '[UserId, Age] -> Record '[UserId, Age]
intFieldDoubler = mapMono (* 2)

-- fun types

tableTypes' rowGen { rowTypeName = "U2"
                   , columnNames = [ "user id", "age", "gender"
                                   , "occupation", "zip code" ]
                   , separator = "|"
                   , tablePrefix = "u2"
                   , columnUniverse = $(colQ ''MyColumns) }
  "../data/ml-100k/u.user"

movieStream2 :: Producer U2 IO ()
movieStream2 = readTableOpt u2Parser "../data/ml-100k/u.user"

neOccupations
  :: (U2zipCode ∈ rs, U2occupation ∈ rs, Monad m)
  => Pipe (Record rs) Text m r
neOccupations = P.filter (isNewEngland . view u2zipCode)
                >-> P.map (view u2occupation)
  where isNewEngland (ZipUS 0 _ _ _ _) = True
        isNewEngland _ = False

main :: IO ()
main = do
  putStrLn "hello world"
