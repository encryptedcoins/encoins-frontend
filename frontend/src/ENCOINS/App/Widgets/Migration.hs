{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.Migration where

import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Reflex.Dom

import Backend.Protocol.Types (PasswordRaw (..), TokenCacheV3)
import Backend.Status (AppStatus (..), MigrateStatus (..))
import Backend.Utility (switchHoldDyn)
import ENCOINS.App.Widgets.Basic
    ( loadAppData
    , saveAppData
    , tellAppStatus
    )
import ENCOINS.App.Widgets.Coin (coinV3)
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
    eFireTokens' <- tailE $ updated dmTokensV3
    eFireTokens <- delay 2 eFireTokens'

    let eFireMigration =
            attachPromptlyDynWithMaybe
                (\mToken _ -> if isNothing mToken then Just () else Nothing)
                dmTokensV3
                eFireTokens

    let eTokens =
            attachPromptlyDynWithMaybe
                (\mToken _ -> if isJust mToken then mToken else Nothing)
                dmTokensV3
                (updated dmTokensV3)

    eTokensMigrated <- switchHoldDyn dmTokensV3 $ \case
        -- when tokenV3 is empty, try to migrate from previous versions
        Nothing -> do
            migrateCacheV3 mPass eFireMigration
        -- when some tokenV3 exist, return them
        Just _ -> pure never
    holdDyn [] $ leftmost [eTokens, eTokensMigrated]

migrateCacheV3 ::
    (MonadWidget t m, EventWriter t [AppStatus] m) =>
    Maybe PasswordRaw
    -> Event t ()
    -> m (Event t [TokenCacheV3])
migrateCacheV3 mPass ev = do
    logEvent "The migration launched" ev
    tellAppStatus $ Migrate MigUpdating <$ ev
    dSecretsV1 :: Dynamic t [Secret] <-
        loadAppData mPass encoinsV1 "migrateCacheV3-key-eSecretsV1" ev id []
    dSecretsV2 :: Dynamic t [(Secret, Text)] <-
        loadAppData mPass encoinsV2 "migrateCacheV3-key-eSecretsV2" ev id []

    -- As V1 and V2 fire two events on load result, fire saving on second one.
    eCacheV3 <- tailE $ updated $ zipDynWith migrateV3 dSecretsV1 dSecretsV2
    eSaved <- saveAppData mPass encoinsV3 eCacheV3
    eTokensV3 :: Event t [TokenCacheV3] <- updated <$>
        loadAppData mPass encoinsV3 "migrateCacheV3-key-eSecretsV3" eSaved id []

    -- migration is too quick, that's why we delay Success message
    eMigSuccess <- delay 2 eTokensV3
    tellAppStatus $ Migrate MigSuccess <$ eMigSuccess

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
