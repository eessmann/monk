{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Monk (projectName)

main :: IO ()
main = do
    let banner = "Benchmarks for " <> projectName
    putTextLn banner
