{-# LANGUAGE OverloadedStrings #-}

module I18n.Dao where

import Common.Utility (column, space)
import Data.Text (Text)
import qualified Data.Text as T
import I18n.Common.I18n

data DaoMessage
    = EmptyDaoMessage
    | Delegate
    | DelegateEncsWindowTitle
    | Relay
    | Total
    | UrlText
    | Unstake
    | Poll
    | ActivePoll
    | ConcludedPolls
    | EndDate
    | Download
    | DownloadResults
    deriving stock (Eq, Show)

instance HasI18n Locale DaoMessage Text where
    localizeWith locale t = case locale of
        Locale_EN -> showDaoMessageEn t
        Locale_RU -> showDaoMessageRu t -- TODO: update it

showDaoMessageEn :: DaoMessage -> Text
showDaoMessageEn = \case
    EmptyDaoMessage -> T.empty
    Delegate -> "DELEGATE"
    DelegateEncsWindowTitle -> "Delegate Encs"
    Relay -> "Relay"
    Total -> "Total"
    UrlText -> "Choose a relay URL from the list or enter a new one"
    Unstake -> "Unstake"
    Poll -> "Poll"
    ActivePoll -> "Active poll"
    ConcludedPolls -> "Concluded polls"
    EndDate -> "The vote ended on" <> space
    Download -> "DOWNLOAD"
    DownloadResults -> "Download poll results"

showDaoMessageRu :: DaoMessage -> Text
showDaoMessageRu = \case
    EmptyDaoMessage -> T.empty
    Delegate -> "Делегировать"
    DelegateEncsWindowTitle -> "Делегирование токенов"
    Relay -> "Релей"
    Total -> "Всего"
    UrlText -> "Выберите релей из списка новый"
    Unstake -> "Разделегировать"
    Poll -> "Голосовать"
    ActivePoll -> "Активное голосование"
    ConcludedPolls -> "Завершенные голосования"
    EndDate -> "Голосование завершится" <> space
    Download -> "Скачать"
    DownloadResults -> "Скачать результаты голосования"
