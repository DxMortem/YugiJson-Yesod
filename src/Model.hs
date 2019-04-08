{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

-- { "id": 1, "fieldOne": "example", "fieldTwo": 1, "fieldThree": true }
instance ToJSON (Entity Card) where
    toJSON (Entity cardId card) = object
        [ "id"          .= (String $ toPathPiece cardId)
        , "name"        .= cardName card
        , "description" .= cardDescription card
        , "type"        .= cardType card
        ]

instance FromJSON Card where
    parseJSON (Object card) = Card
        <$> card .: "cardName"
        <*> card .: "cardDescription"
        <*> card .: "cardType"

    parseJSON _ = mzero
