{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH
import Database.PostgreSQL.PG
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)

main = putStrLn "ok"