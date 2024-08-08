{-# LANGUAGE OverloadedStrings #-}

module I18n.Dao where

import I18n.Common.I18n
import Data.Text (Text)


data DaoMessage
    = Delegate
    | DelegateEncs
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
    deriving stock (Eq)

instance HasI18n Locale DaoMessage Text where
  localizeWith locale t = case locale of
    Locale_EN -> showDaoMessageEn t
    Locale_RU -> showDaoMessageEn t -- TODO: update it

showDaoMessageEn :: DaoMessage -> Text 
showDaoMessageEn = \case 
    Delegate -> "Delegate"
    DelegateEncs -> "Delegate Encs"
    Relay -> "Relay"
    Total -> "Total"
    UrlText -> "Choose a relay URL above or enter a new one below"
    Unstake -> "Unstake"
    Poll -> "Poll"
    ActivePoll -> "Active poll"
    ConcludedPolls -> "Concluded polls"
    EndDate -> "The vote ended on"
    Download -> "Download"
    DownloadResults -> "Download poll results"

