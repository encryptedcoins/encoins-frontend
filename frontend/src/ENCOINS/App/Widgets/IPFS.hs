module ENCOINS.App.Widgets.IPFS where

import           Backend.Protocol.Types
import           Backend.Servant.Requests           (cacheRequest)
import           Backend.Utility                    (eventEither, switchHoldDyn)
import           ENCOINS.App.Widgets.Basic          (elementDynResultJS,
                                                     elementResultJS, genUid,
                                                     loadAppDataId)
import           ENCOINS.App.Widgets.PasswordWindow (PasswordRaw, getPassRaw)
import           ENCOINS.Common.Events
import           ENCOINS.Common.Utils               (toText)
import qualified JS.App                             as JS
import           JS.Website                         (saveJSON)

import qualified Crypto.Hash                        as Hash
import           Data.Aeson                         (encode)
import           Data.ByteString.Lazy               (toStrict)
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Maybe                         (fromMaybe)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Text.Encoding                 (decodeUtf8, encodeUtf8)
import           Reflex.Dom

updateCacheStatus :: Map Text CloudResponse -> [TokenCacheV3] -> [TokenCacheV3]
updateCacheStatus clouds = map updateCloud
  where
    updateCloud :: TokenCacheV3 -> TokenCacheV3
    updateCloud t =
      let name = tcAssetName t
          mStatus = Map.lookup name clouds
      in case mStatus of
        Nothing -> t
        Just (MkCloudResponse mIpfs mCoin) ->
           t{ tcIpfsStatus = fromMaybe (tcIpfsStatus t) mIpfs
            , tcCoinStatus = fromMaybe (tcCoinStatus t) mCoin
            }

mkCloudRequest :: TokenCacheV3 -> Maybe CloudRequest
mkCloudRequest cache = case (tcIpfsStatus cache, tcCoinStatus cache)  of
  (Unpinned, Minted) -> Just $ MkCloudRequest
    { reqAssetName  = tcAssetName cache
    , reqSecretKey  = "encrypted secret"
    }
  _ -> Nothing

pinValidTokensInIpfs :: MonadWidget t m
  => Dynamic t Text
  -> Dynamic t [TokenCacheV3]
  -> m (Dynamic t [TokenCacheV3])
pinValidTokensInIpfs dWalletAddress dTokenCache = do
  let dValidTokens = mapMaybe mkCloudRequest <$> dTokenCache
  -- logDyn "dValidTokens" dValidTokens
  let eFireCaching = () <$ (ffilter (not . null) $ updated dValidTokens)
  -- logEvent "eFireCaching" eFireCaching
  let dClientId = mkClientId <$> dWalletAddress
  let dReq = zipDynWith (,) dClientId dValidTokens
  -- logDyn "dClientId" dClientId
  eeCloudResponse <- cacheRequest dReq eFireCaching
  -- logEvent "walletTab: cache response:" eeCloudResponse
  let (eCacheError, eCloudResponse) = eventEither eeCloudResponse
  dCloudResponse <- holdDyn Map.empty eCloudResponse
  pure $ ffor2 dCloudResponse dTokenCache updateCacheStatus

mkClientId :: Text -> Text
mkClientId address = toText $ Hash.hash @Hash.SHA256 $ encodeUtf8 address

-- TODO: remove after debug
tokenSample :: CloudRequest
tokenSample = MkCloudRequest
  { reqAssetName = "b47f55bdc1d7615409cf8cc714e3885c42d6cb48629d44ff5a9265c88aa30cdc"
  -- , reqAssetName = "495090d7e6f2911cf0e1bc59ce244983ac5f1fe4adbaec9ce6af3429ad7aec79"
  , reqSecretKey = "super secret key"
  }

getAesKey :: MonadWidget t m
  => Maybe PasswordRaw
  -> Event t ()
  -> m (Event t Text)
getAesKey mPass ev1 = do
  -- logEvent "getAesKey: ev1" ev1
  let cacheKey = "encoins-aes-key"
  dLoadedKey <- loadAppDataId
    (getPassRaw <$> mPass)
    cacheKey
    "first-load-of-aes-key"
    ev1
    id
    Nothing
  -- logDyn "getAesKey: dLoadedKey1" dLoadedKey
  let ev2 = () <$ updated dLoadedKey
  ev3 <- delay 0.1 ev2
  switchHoldDyn dLoadedKey $ \case
    Nothing -> do
        -- logEvent "empty event" ev3
        let genElId = "genAESKeyId"
        performEvent_ $ JS.generateAESKey genElId <$ ev3
        eAesKey <- updated <$> elementResultJS genElId id
        -- logEvent "getAesKey: eAesKey" eAesKey
        ev4 <- performEvent $ saveJSON (getPassRaw <$> mPass) cacheKey
          . decodeUtf8
          . toStrict
          . encode <$> eAesKey
        eLoadedKey <- updated <$> loadAppDataId
          (getPassRaw <$> mPass)
          cacheKey
          "second-load-of-aes-key"
          ev4
          id
          ""
        -- logEvent "getAesKey: eLoadedKey2" eLoadedKey
        pure eLoadedKey
    Just loadedKey -> do
      -- logEvent "key event" $ loadedKey <$ ev3
      pure $ loadedKey <$ ev3

encryptToken :: MonadWidget t m
  => Dynamic t Text
  -> Event t ()
  -- -> Dynamic t TokenCacheV3
  -- -> m (Dynamic t TokenCacheV3)
  -> Dynamic t Text
  -> m (Event t Text)
encryptToken dKey ev dSecret = do
  dUid <- genUid ev
  let dEncryptParam = ffor3 dKey dUid dSecret (,,)
  logDyn "encryptToken: dEncryptParam" dEncryptParam
  eDelayed <- delay 0.1 $ updated dUid
  performEvent_ $ JS.encryptAES <$> tagPromptlyDyn dEncryptParam eDelayed
  dEncryptedToken <- elementDynResultJS id dUid
  logDyn "encryptToken: dEncryptedToken" dEncryptedToken
  let eToken = ffilter (not . T.null) $ updated dEncryptedToken
  logEvent "encryptedToken: eToken" eToken
  pure eToken
