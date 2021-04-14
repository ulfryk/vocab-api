{-# LANGUAGE OverloadedStrings #-}

module Card.Repository where

import Card.Dto.Card
import qualified Card.Dto.CardInput as Inp
import qualified Data.List as Lst (find)
import Data.Text (Text, pack)
import Data.Time.Clock (UTCTime)
import Database.MongoDB
  ( Action,
    Document,
    Field,
    Label,
    ObjectId,
    Value (Null),
    access,
    cast,
    close,
    connect,
    find,
    host,
    insert,
    label,
    look,
    master,
    rest,
    select,
    timestamp,
    typed,
    value,
    valueAt,
    (=:),
  )

getString :: Label -> Document -> Text
getString l = pack . typed . valueAt l

maybeNotNull :: Field -> Maybe Field
maybeNotNull f =
  case value f of
    Null -> Nothing
    _ -> Just f

getStringMaybe :: Label -> Document -> Maybe Text
getStringMaybe k doc = do
  fld <- Lst.find ((k ==) . label) doc
  notNullFld <- maybeNotNull fld
  return $ (pack . cast . value) notNullFld

getBool :: Label -> Document -> Bool
getBool l = typed . valueAt l

getObjId :: Document -> ObjectId
getObjId = typed . valueAt "_id"

asDocument :: Card -> Document
asDocument card = ["aSide" =: aSide card, "bSide" =: bSide card]

inputAsDocument :: Inp.CardInput -> Document
inputAsDocument card =
  [ "aSide" =: Inp.aSide card,
    "aSideDetails" =: Inp.aSideDetails card,
    "bSide" =: Inp.bSide card,
    "suspended" =: False,
    "archived" =: False
  ]

getCreatedTime :: Document -> UTCTime
getCreatedTime = timestamp . getObjId

getUpdatedTime :: Label -> Document -> UTCTime
getUpdatedTime l doc =
  case look l doc of
    Just v -> typed v
    Nothing -> getCreatedTime doc

fromDocument :: Document -> Card
fromDocument doc =
  Card
    { aSide = getString "aSide" doc,
      aSideDetails = getStringMaybe "aSideDetails" doc,
      bSide = getString "bSide" doc,
      createdAt = getCreatedTime doc,
      updatedAt = getUpdatedTime "updatedAt" doc,
      suspended = getBool "suspended" doc,
      archived = getBool "archived" doc,
      _id = pack . show $ getObjId doc
    }

toId :: Value -> Text
toId = pack . show

addCard :: Inp.CardInput -> Action IO Value
addCard card = insert "cards" $ inputAsDocument card

allCards :: Action IO [Document]
allCards = rest =<< find (select [] "cards")

createCard :: Inp.CardInput -> IO Card
createCard input = do
  ident <- processRequest $ addCard input
  return
    Card
      { aSide = Inp.aSide input,
        aSideDetails = Inp.aSideDetails input,
        bSide = Inp.bSide input,
        createdAt = timestamp $ typed ident,
        updatedAt = timestamp $ typed ident,
        suspended = False,
        archived = False,
        _id = toId ident
      }

getAllCards :: IO [Card]
getAllCards = do
  cardDocs <- processRequest allCards
  return $ fmap fromDocument cardDocs

processRequest :: Action IO b -> IO b
processRequest action = do
  pipe <- connect (host "127.0.0.1")
  response <- access pipe master "vocabtest" action
  close pipe
  return response
