{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Card where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

getCardsJsonR :: Handler Value 
getCardsJsonR = do
    cards <- runDB $ selectList [] [] :: Handler [Entity Card]

    return $ object ["cards" .= cards]

postCardsJsonR :: Handler Value 
postCardsJsonR = do
    card <- requireJsonBody :: Handler Card
    _    <- runDB $ insert card

    sendResponseStatus status201 ("CREATED" :: Text)

getCardJsonR :: CardId -> Handler Value 
getCardJsonR cardId = do
    card <- runDB $ get404 cardId

    return $ object ["card" .= (Entity cardId card)]

putCardJsonR :: CardId -> Handler Value 
putCardJsonR cardId = do
    card <- requireJsonBody :: Handler Card

    runDB $ replace cardId card

    sendResponseStatus status200 ("UPDATED" :: Text)

deleteCardJsonR :: CardId -> Handler Value 
deleteCardJsonR cardId = do
    runDB $ delete cardId

    sendResponseStatus status200 ("DELETED" :: Text)