{-# LANGUAGE OverloadedStrings #-}

module Card.Api where

import Card.Dto.CardInput (CardInput)
import Card.Dto.CardUpdate (CardUpdate)
import Card.Repository
import ApiError (ApiError, apiErr)
import Control.Monad.Trans (liftIO)
import Data.Text (Text)
import Network.HTTP.Types.Status (notFound404)
import Web.Spock (SpockAction, SpockM, get, json, jsonBody', patch, post, root, setStatus, text, var, (<//>))
import Data.Aeson (ToJSON)

type Api = SpockM () () () ()

type ApiAction a = SpockAction () () () a

cardNotFound :: Text -> ApiError
cardNotFound cid = apiErr ("Card with '" <> cid <> "' id not found.")

returnOr404 :: (ToJSON a) => Text -> Maybe a -> ApiAction b
returnOr404 cid card =
  case card of
    Just c -> json c
    Nothing -> do
      setStatus notFound404
      json $ cardNotFound cid

app :: Api
app =
  do
    get root $ text "Cards can be found under '/cards' path"

    get "cards" $ do
      cards <- liftIO getAllCards
      json cards

    get ("cards" <//> var) $ \cid -> do
      card <- liftIO $ getOneCard cid
      returnOr404 cid card

    post "cards" $ do
      input <- jsonBody' :: ApiAction CardInput
      card <- liftIO $ createCard input
      json card

    patch ("cards" <//> var) $ \cid -> do
      p <- jsonBody' :: ApiAction CardUpdate
      card <- liftIO $ updateCard cid p
      returnOr404 cid card

