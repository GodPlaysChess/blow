{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty

main :: IO ()
main =  scotty 3000 $ do 
  get "/" $ do
    html "hello world"

  get "/hello" $ do 
     text "nothing to look up here"


plus2 :: Integer -> Integer
plus2 = (+ 12)