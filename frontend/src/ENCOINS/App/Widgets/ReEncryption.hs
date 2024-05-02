module ENCOINS.App.Widgets.ReEncryption where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (ToJSON)
import Data.Text (Text)
import Reflex.Dom

import Backend.Protocol.Types (AesKeyRaw, PasswordRaw (..), TokenCacheV3)
import ENCOINS.App.Widgets.Basic (loadAppDataM)
import ENCOINS.Bulletproofs (Secret)
import ENCOINS.Common.Cache (aesKey, encoinsV1, encoinsV2, encoinsV3)
import ENCOINS.Common.Events
import ENCOINS.Common.Utils (toJsonText)
import JS.Website (saveJSON)

-------------------------------------------------------------------------------
-- Re-encrypt current cache
-------------------------------------------------------------------------------

-- 1. Token cache of version 3
-- 2. Cloud key

reEncryptCurrentCache ::
    (MonadWidget t m) =>
    Dynamic t [TokenCacheV3]
    -> Dynamic t (Maybe AesKeyRaw)
    -> Event t (Maybe PasswordRaw)
    -> m ()
reEncryptCurrentCache dTokensV3 dmKey eReEncrypt = do
    performEvent_ $
        fmap (reEncrypt encoinsV3) $
            attachPromptlyDyn dTokensV3 eReEncrypt

    performEvent_ $
        fmap (reEncrypt aesKey) $
            attachPromptlyDynWithMaybe (\mKey e -> (,e) <$> mKey) dmKey eReEncrypt

-- Save value of particular key with new password
reEncrypt :: (ToJSON a, MonadIO m) => Text -> (a, Maybe PasswordRaw) -> m ()
reEncrypt key (d, mNewPass) =
    saveJSON
        (getPassRaw <$> mNewPass)
        key
        . toJsonText
        $ d

-------------------------------------------------------------------------------
-- Re-encrypt old cache
-------------------------------------------------------------------------------

-- After migration to new cache add re-encryption for outdated one

-- 1. Token cache of version 1
-- 2. Token cache of version 2

reEncryptOldCache ::
    (MonadWidget t m) =>
    Maybe PasswordRaw
    -> Event t (Maybe PasswordRaw)
    -> m ()
reEncryptOldCache mPass emReEncrypt = do
    reEncryptTokensV2 mPass emReEncrypt
    reEncryptTokensV1 mPass emReEncrypt

reEncryptTokensV2 ::
    (MonadWidget t m) =>
    Maybe PasswordRaw
    -> Event t (Maybe PasswordRaw)
    -> m ()
reEncryptTokensV2 mPass emReEncrypt = do
    dmReEncrypt <- holdDyn Nothing emReEncrypt
    dmTokensV2 :: Dynamic t (Maybe [(Secret, Text)]) <-
        loadAppDataM mPass encoinsV2 "reEncryptOldCache-encoinsV2" $
            () <$ emReEncrypt
    let emFireSave = tagPromptlyDyn dmReEncrypt $ updated dmTokensV2
    performEvent_ $
        fmap (reEncrypt encoinsV2) $
            attachPromptlyDynWithMaybe
                (\mTokens pass -> (,pass) <$> mTokens)
                dmTokensV2
                emFireSave

reEncryptTokensV1 ::
    (MonadWidget t m) =>
    Maybe PasswordRaw
    -> Event t (Maybe PasswordRaw)
    -> m ()
reEncryptTokensV1 mPass emReEncrypt = do
    dmReEncrypt <- holdDyn Nothing emReEncrypt
    dmTokensV1 :: Dynamic t (Maybe [Secret]) <-
        loadAppDataM mPass encoinsV1 "reEncryptOldCache-encoinsV1" $
            () <$ emReEncrypt
    let emFireSave = tagPromptlyDyn dmReEncrypt $ updated dmTokensV1
    performEvent_ $
        fmap (reEncrypt encoinsV1) $
            attachPromptlyDynWithMaybe
                (\mTokens pass -> (,pass) <$> mTokens)
                dmTokensV1
                emFireSave
