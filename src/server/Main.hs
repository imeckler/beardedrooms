{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Control.Monad.IO.Class

main = scotty 3000 $ do
  get "/data/:path" $ do
    path <- param "path"
    setHeader "Access-Control-Allow-Origin" "*"
    liftIO $ print path
    setHeader "Content-type" "application/json charset=utf-8"
    file path
