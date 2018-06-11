{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified LibMain(main)

main :: IO ()
main = LibMain.main