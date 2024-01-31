{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.Migration where

import           Data.Align                (alignWith)
import           Data.Bool                 (bool)
import           Data.Text                 (Text)
import           Data.These                (these)
import           Reflex.Dom

import           Backend.Protocol.Types    (PasswordRaw (..), TokenCacheV3)
import           Backend.Status            (Status (..))
import           ENCOINS.App.Widgets.Basic (elementResultJS, loadAppData,
                                            saveAppDataId_, tellTxStatus)
import           ENCOINS.App.Widgets.Coin  (coinV3)
import           ENCOINS.Bulletproofs      (Secret)
import           ENCOINS.Common.Cache      (encoinsV1, encoinsV2, encoinsV3)
import           ENCOINS.Common.Events


{-
Evolutions of encoins cache by key
1. encoins - first version of cache that contains Secrets only
2. encoins-with-cache - second version of cache that contains (Secret, AssetName)
3. encoins-v3 - third version of the cache
   that contains record with AssetName, Secret, IpfsStatus (pinned/unpinned) and CoinStatus (minted/burned)
-}

-- Update cache only when
-- "encoins-v3" cache doesn't exist (or empty)
-- and
-- "encoins-with-name" cache exists
--  or
-- "encoins" cache exists.
updateCacheV3 :: (MonadWidget t m, EventWriter t [Event t (Text, Status)] m)
  => Maybe PasswordRaw
  -> Dynamic t [TokenCacheV3]
  -> m ()
updateCacheV3 mPass dSecretsV3 = do
  let eSecretsV3 = updated $ null <$> dSecretsV3
  widgetHold_ blank $
    bool blank (migrateCacheV3 mPass) <$> eSecretsV3

migrateCacheV3 :: (MonadWidget t m, EventWriter t [Event t (Text, Status)] m)
  => Maybe PasswordRaw
  -> m ()
migrateCacheV3 mPass = do
  eSecretsV1 :: Event t [Secret] <-
    updated <$> loadAppData mPass encoinsV1 id []
  eSecretsV2 :: Event t [(Secret, Text)] <-
    updated <$> loadAppData mPass encoinsV2 id []

  -- logEvent "migrateCacheV3: eSecretsV1" eSecretsV1
  -- logEvent "migrateCacheV3: eSecretsV2" eSecretsV2

  let eIsOldCacheEmpty = mergeWith (&&) [null <$> eSecretsV1, null <$> eSecretsV2]
  -- logEvent "migrateCacheV3: eIsOldCacheEmpty" eIsOldCacheEmpty

  let eMigrateStatus = bool
        (CustomStatus "Cache structure is updating. Please wait.")
        Ready
        <$> eIsOldCacheEmpty
  tellTxStatus "App status" eMigrateStatus
  -- logEvent "migrateCacheV3: eMigrateStatus" eMigrateStatus

  let cacheV3 = alignWith
        (these migrateV1 migrateV2 migrateV3)
        eSecretsV1
        eSecretsV2
  -- Migrate old cache (when it is not empty) to encoins-v3
  let eSecretsV3 = ffilter (not . null) cacheV3
  -- logEvent "migrateCacheV3: eSecretsV3" eSecretsV3

  saveAppDataId_ mPass encoinsV3 eSecretsV3

  -- Ask user to reload when cache structure is updated
  let eIsOldCacheEmptyLog = () <$ ffilter id eIsOldCacheEmpty
  -- logEvent "migrateCacheV3: eIsOldCacheEmptyLog" eIsOldCacheEmptyLog

  eSaved <- updated <$> elementResultJS "encoins-v3" (const ())
  -- logEvent "migrateCacheV3: eSaved" eSaved
  tellTxStatus "App status" $ CustomStatus "Please reload the page"
      <$ leftmost [eIsOldCacheEmptyLog, eSaved]

migrateV1 :: [Secret] -> [TokenCacheV3]
migrateV1 v1
  | not (null v1) = map coinV3 v1
  | otherwise = []

migrateV2 :: [(Secret,Text)] -> [TokenCacheV3]
migrateV2 v2
  | not (null v2) = map (coinV3 . fst) v2
  | otherwise = []

migrateV3 :: [Secret] -> [(Secret,Text)] -> [TokenCacheV3]
migrateV3 v1 v2
  | not (null v2) = map (coinV3 . fst) v2
  | not (null v1) = map coinV3 v1
  | otherwise = []
