{-# LANGUAGE OverloadedStrings #-}

module CardsApi (app, Api, ApiAction, Card) where

import CardDTO
import CardRepository
import Control.Monad.Trans (liftIO)
import Web.Spock (SpockAction, SpockM, get, json, jsonBody', post, root, text)
import Web.Spock.Config ()

type Api = SpockM () () () ()

type ApiAction a = SpockAction () () () a

app :: Api
app =
  do
    get root $ text "Cards can be found under '/cards' path"

    get "cards" $ do
      cards <- liftIO getAllCards
      json cards

    post "cards" $ do
      input <- jsonBody' :: ApiAction CardInput
      card <- liftIO $ createCard input
      json card
