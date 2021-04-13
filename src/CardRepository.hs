{-# LANGUAGE OverloadedStrings #-}

module CardRepository where

import CardDTO
import Data.Text (Text, pack)
import Database.MongoDB (Action, Document, Label, ObjectId, Value, access, close, connect, find, host, insert, master, rest, select, typed, valueAt, (=:))

getString :: Label -> Document -> Text
getString label = pack . typed . valueAt label

getObjId :: Document -> ObjectId
getObjId = typed . valueAt "_id"

asDocument :: Card -> Document
asDocument card = ["aSide" =: aSide card, "bSide" =: bSide card]

inputAsDocument :: CardInput -> Document
inputAsDocument card = ["aSide" =: initASide card, "bSide" =: initBSide card]

fromDocument :: Document -> Card
fromDocument doc =
  Card
    { aSide = getString "aSide" doc,
      bSide = getString "bSide" doc,
      _id = pack . show $ getObjId doc
    }

toId :: Value -> Text
toId = pack . show

addCard :: CardInput -> Action IO Value
addCard card = insert "cards" $ inputAsDocument card

allCards :: Action IO [Document]
allCards = rest =<< find (select [] "cards")

createCard :: CardInput -> IO Card
createCard input = do
  ident <- processRequest $ addCard input
  return Card {aSide = initASide input, bSide = initBSide input, _id = toId ident}

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
