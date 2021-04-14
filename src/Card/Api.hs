{-# LANGUAGE OverloadedStrings #-}

module Card.Api where

import Card.Dto.CardInput (CardInput)
import Card.Dto.CardUpdate (CardUpdate)
import Card.Repository
import Control.Monad.Trans (liftIO)
import Data.Text (pack)
import Web.Spock (SpockAction, SpockM, get, json, jsonBody', var, post, patch, root, text, (<//>))

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

    patch ("cards" <//> var) $ \cid ->
      do
        p <- jsonBody' :: ApiAction CardUpdate
        text . pack $ show p <> " by id " <> cid
