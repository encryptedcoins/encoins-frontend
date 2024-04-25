{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.Migration where

import Data.Bool (bool)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Reflex.Dom

import Backend.Protocol.Types (PasswordRaw (..), TokenCacheV3, showTokens)
import Backend.Status (AppStatus, Status (..))
import Backend.Utility (switchHoldDyn)
import ENCOINS.App.Widgets.Basic
    ( loadAppData
    , loadAppDataME
    , saveAppData
    , tellTxStatus
    )
import ENCOINS.App.Widgets.Coin (coinV3)
import ENCOINS.App.Widgets.PasswordWindow (reEncrypt)
import ENCOINS.Bulletproofs (Secret)
import ENCOINS.Common.Cache (encoinsV1, encoinsV2, encoinsV3)
import ENCOINS.Common.Events

{-
Evolutions of encoins cache by key
1. encoins - first version of cache that contains Secrets only
2. encoins-with-cache - second version of cache that contains (Secret, AssetName)
3. encoins-v3 - third version of the cache
   that contains record with AssetName, Secret, SaveStatus
-}

-- Update cache only when
-- "encoins-v3" cache doesn't exist (or empty)
-- and
-- "encoins-with-name" cache exists
--  or
-- "encoins" cache exists.
migrateTokenCacheV3 ::
    (MonadWidget t m, EventWriter t [AppStatus] m) =>
    Maybe PasswordRaw
    -> Dynamic t (Maybe [TokenCacheV3])
    -> m (Dynamic t [TokenCacheV3])
migrateTokenCacheV3 mPass dmTokensV3 = do
    -- Delay is required for waiting first token loading
    eFireMigration <-
        delay 2 $
            attachPromptlyDynWithMaybe
                (\mToken _ -> if isNothing mToken then Just () else Nothing)
                dmTokensV3
                (updated dmTokensV3)
    -- logEvent "migrateTokenCacheV3: eFireMigration" eFireMigration

    let eTokens =
            attachPromptlyDynWithMaybe
                (\mToken _ -> if isJust mToken then mToken else Nothing)
                dmTokensV3
                (updated dmTokensV3)
    -- let (eFireMigration2, eTokens) = eventMaybe emLoadedTokens
    -- logEvent "migrateTokenCacheV3: eTokens" $ showTokens <$> eTokens

    eTokensMigrated <- switchHoldDyn dmTokensV3 $ \case
        -- when tokenV3 is empty, try to migrate from previous versions
        Nothing -> do
            logEvent "In Nothing: eFireMigration" eFireMigration
            migrateCacheV3 mPass eFireMigration
        -- when some tokenV3 exist, return them
        Just _ -> pure never
    -- ev <- newEvent
    -- pure $ tokens <$ ev
    holdDyn [] $ leftmost [eTokens, eTokensMigrated]

migrateCacheV3 ::
    (MonadWidget t m, EventWriter t [AppStatus] m) =>
    Maybe PasswordRaw
    -> Event t ()
    -> m (Event t [TokenCacheV3])
migrateCacheV3 mPass ev = do
    logEvent "migrateCacheV3 occur: ev" ev
    dSecretsV1 :: Dynamic t [Secret] <-
        loadAppData mPass encoinsV1 "migrateCacheV3-key-eSecretsV1" ev id []
    dSecretsV2 :: Dynamic t [(Secret, Text)] <-
        loadAppData mPass encoinsV2 "migrateCacheV3-key-eSecretsV2" ev id []

    let dIsOldCacheEmpty = zipDynWith (\v1 v2 -> null v1 && null v2) dSecretsV1 dSecretsV2

    let dMigrateStatus =
            bool
                (CustomStatus "Cache structure is updating. Please wait.")
                Ready
                <$> dIsOldCacheEmpty
    tellTxStatus "App status" $ updated dMigrateStatus

    let dCacheV3 = zipDynWith migrateV3 dSecretsV1 dSecretsV2

    eSaved <- saveAppData mPass encoinsV3 $ updated dCacheV3
    eTokensV3 :: Event t [TokenCacheV3] <-
        updated
            <$> loadAppData mPass encoinsV3 "migrateCacheV3-key-eSecretsV3" eSaved id []

    tellTxStatus "App status" $ CacheMigrated <$ eTokensV3
    pure eTokensV3

migrateV1 :: [Secret] -> [TokenCacheV3]
migrateV1 v1
    | not (null v1) = map coinV3 v1
    | otherwise = []

migrateV2 :: [(Secret, Text)] -> [TokenCacheV3]
migrateV2 v2
    | not (null v2) = map (coinV3 . fst) v2
    | otherwise = []

migrateV3 :: [Secret] -> [(Secret, Text)] -> [TokenCacheV3]
migrateV3 v1 v2
    | not (null v2) = map (coinV3 . fst) v2
    | not (null v1) = map coinV3 v1
    | otherwise = []
