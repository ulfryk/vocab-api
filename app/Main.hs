{-# LANGUAGE OverloadedStrings #-}

module Main where

import Card.Api
import Web.Spock (runSpock, spock)
import Web.Spock.Config (PoolOrConn (PCNoDatabase), defaultSpockCfg)

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)
