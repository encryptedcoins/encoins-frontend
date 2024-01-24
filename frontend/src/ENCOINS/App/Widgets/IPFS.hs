module ENCOINS.App.Widgets.IPFS where

import           Backend.Protocol.Types
import           Backend.Servant.Requests           (cacheRequest)
import           Backend.Utility                    (eventEither, switchHoldDyn)
import           ENCOINS.App.Widgets.Basic          (elementDynResultJS,
                                                     elementResultJS, genUid,
                                                     loadAppDataId)
import           ENCOINS.App.Widgets.PasswordWindow (PasswordRaw, getPassRaw)
import           ENCOINS.Bulletproofs               (Secret)
import           ENCOINS.Common.Events
import           ENCOINS.Common.Utils               (toText)
import qualified JS.App                             as JS
import           JS.Website                         (saveJSON)

import           Control.Monad                      (forM)
import qualified Crypto.Hash                        as Hash
import           Data.Aeson                         (encode)
import           Data.ByteString.Lazy               (toStrict)
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Maybe                         (catMaybes, fromMaybe)
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

mkCloudRequestS :: TokenCacheV3 -> Text -> Maybe CloudRequest
mkCloudRequestS cache encryptedSecret = case (tcIpfsStatus cache, tcCoinStatus cache, T.null encryptedSecret)  of
  (Unpinned, Minted, False) -> Just $ MkCloudRequest
    { reqAssetName  = tcAssetName cache
    , reqSecretKey  = encryptedSecret
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

pinValidTokensInIpfs2 :: MonadWidget t m
  => Dynamic t Text
  -> Maybe PasswordRaw
  -> Dynamic t Text
  -> Dynamic t [TokenCacheV3]
  -> m (Dynamic t [TokenCacheV3])
pinValidTokensInIpfs2 dWalletAddress mPass dKey dTokenCache = do
  dCloudTokens <- encryptTokens dKey dTokenCache
  logDyn "dCloudTokens" dCloudTokens
  let eFireCaching = () <$ (ffilter (not . null) $ updated dCloudTokens)
  -- logEvent "eFireCaching" eFireCaching
  let dClientId = mkClientId <$> dWalletAddress
  let dReq = zipDynWith (,) dClientId dCloudTokens
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
        logEvent "getAesKey: eAesKey" eAesKey
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

getAesKey2 :: MonadWidget t m
  => Maybe PasswordRaw
  -> m (Event t Text)
getAesKey2 mPass = do
  ev1 <- newEventWithDelay 0.1
  let cacheKey = "encoins-aes-key"
  dLoadedKey <- loadAppDataId
    (getPassRaw <$> mPass) cacheKey "first-load-of-aes-key" ev1 id Nothing
  let ev2 = () <$ updated dLoadedKey
  ev3 <- delay 0.1 ev2
  switchHoldDyn dLoadedKey $ \case
    Nothing -> do
        let genElId = "genAESKeyId"
        performEvent_ $ JS.generateAESKey genElId <$ ev3
        eAesKey <- updated <$> elementResultJS genElId id
        logEvent "getAesKey: eAesKey" eAesKey
        ev4 <- performEvent $ saveJSON (getPassRaw <$> mPass) cacheKey
          . decodeUtf8
          . toStrict
          . encode <$> eAesKey
        eLoadedKey <- updated <$> loadAppDataId
          (getPassRaw <$> mPass)  cacheKey  "second-load-of-aes-key"  ev4 id ""
        pure eLoadedKey
    Just loadedKey -> pure $ loadedKey <$ ev3

encryptToken :: MonadWidget t m
  => Text
  -> Event t ()
  -> Secret
  -> m (Dynamic t Text)
encryptToken key ev secret = do
  let secretByte = toStrict $ encode secret
  let elementId = toText $ Hash.hash @Hash.SHA256 secretByte
  performEvent_ $ JS.encryptAES (key, elementId, decodeUtf8 secretByte) <$ ev
  dEncryptedToken <- elementResultJS elementId id
  -- logDyn "encryptToken: dEncryptedToken" dEncryptedToken
  pure dEncryptedToken

encryptTokens :: MonadWidget t m
  => Dynamic t Text
  -> Dynamic t [TokenCacheV3]
  -> m (Dynamic t [CloudRequest])
encryptTokens dKey dTokens = do
  eClouds <- switchHoldDyn dKey $ \case
    "" -> pure never
    key -> switchHoldDyn dTokens $ \case
      [] -> pure never
      tokens -> do
        fmap (updated . sequence) $ flip traverse tokens $ \t -> do
            ev <- newEvent
            dEncryptedSecret <- encryptToken key ev $ tcSecret t
            let dmCloud = mkCloudRequestS t <$> dEncryptedSecret
            pure dmCloud
  holdDyn [] $ catMaybes <$> eClouds


decryptToken :: MonadWidget t m
  => Dynamic t Text
  -> Event t ()
  -> Dynamic t Text
  -> m (Dynamic t Text)
decryptToken dKey ev dEncryptedHex = do
  eUid <- genUid ev
  dUid <- holdDyn "default-decrypt-uuid" eUid
  let dDecryptParam = ffor3 dKey dUid dEncryptedHex (,,)
  logDyn "decryptToken: dDecryptParam" dDecryptParam
  eDelayed <- delay 0.5 $ updated dUid
  performEvent_ $ JS.decryptAES <$> tagPromptlyDyn dDecryptParam eDelayed
  dDecryptedToken <- elementDynResultJS id dUid
  logDyn "decryptToken: dDecryptedToken" dDecryptedToken
  pure dDecryptedToken
