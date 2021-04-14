{-# LANGUAGE OverloadedStrings #-}

module Card.Repository where

import Card.Dto.Card
import qualified Card.Dto.CardInput as Inp
import qualified Card.Dto.CardUpdate as Upd
import Data.Foldable (forM_)
import qualified Data.List as Lst (find)
import Data.Maybe (fromJust)
import Data.Text (Text, pack, unpack)
import Data.Time.Clock (UTCTime)
import Database.MongoDB
  ( Action,
    Document,
    Field,
    Label,
    ObjectId,
    Value (Null, ObjId),
    access,
    cast,
    close,
    connect,
    find,
    findOne,
    host,
    insert,
    label,
    look,
    master,
    merge,
    rest,
    select,
    timestamp,
    typed,
    upsert,
    value,
    valueAt,
    (=:),
    (=?),
  )
import Debug.Trace (trace)

getString :: Label -> Document -> Text
getString l = pack . typed . valueAt l

maybeNotNull :: Field -> Maybe Field
maybeNotNull f =
  case value f of
    Null -> Nothing
    _ -> Just f

-- TODO: returns only first letter ( FIXME )
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

isFieldNotEmpty :: Field -> Bool
isFieldNotEmpty fld =
  case value fld of
    Null -> False
    _ -> True

updateAsDocument :: Upd.CardUpdate -> Document
updateAsDocument card =
  filter isFieldNotEmpty $
    ("aSide" =? Upd.aSide card)
      <> ("aSideDetails" =? Upd.aSideDetails card)
      <> ("bSide" =? Upd.bSide card)
      <> ("suspended" =? Upd.suspended card)
      <> ("archived" =? Upd.archived card)

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

upsertCard :: Text -> Document -> Action IO (Maybe Document)
upsertCard cid patch = do
  let theId = ObjId . read . unpack $ cid
  rec <- findOne $ select ["_id" =: theId] "cards"
  let maybeUpdated = fmap (merge patch) rec
  forM_ maybeUpdated (upsert (select ["_id" =: theId] "cards"))
  return maybeUpdated

updateCard :: Text -> Upd.CardUpdate -> IO (Maybe Card)
updateCard cid upd = do
  updated <- processRequest . upsertCard cid . updateAsDocument $ upd
  return $ fmap fromDocument updated

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
