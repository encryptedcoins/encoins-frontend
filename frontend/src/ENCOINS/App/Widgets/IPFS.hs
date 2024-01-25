module ENCOINS.App.Widgets.IPFS where

import           Backend.Protocol.Types
import           Backend.Servant.Requests           (cacheRequest,
                                                     restoreRequest)
import           Backend.Utility                    (eventEither, eventTuple,
                                                     switchHoldDyn)
import           ENCOINS.App.Widgets.Basic          (elementDynResultJS,
                                                     elementResultJS, genUid,
                                                     loadAppDataId)
import           ENCOINS.App.Widgets.PasswordWindow (PasswordRaw, getPassRaw)
import           ENCOINS.Bulletproofs               (Secret (..))
import           ENCOINS.Common.Events
import           ENCOINS.Common.Utils               (toText)
import           ENCOINS.Crypto.Field               (Field (F))
import qualified JS.App                             as JS
import           JS.Website                         (saveJSON)

import           Control.Monad                      (forM, void)
import qualified Crypto.Hash                        as Hash
import           Data.Aeson                         (eitherDecodeStrict',
                                                     encode)
import           Data.ByteString.Lazy               (toStrict)
import           Data.Either                        (partitionEithers)
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
  logDyn "dClientId" dClientId
  let dReq = zipDynWith (,) dClientId dValidTokens
  -- logDyn "dClientId" dClientId
  eeCloudResponse <- cacheRequest dReq eFireCaching
  -- logEvent "walletTab: cache response:" eeCloudResponse
  let (eCacheError, eCloudResponse) = eventEither eeCloudResponse
  dCloudResponse <- holdDyn Map.empty eCloudResponse
  pure $ ffor2 dCloudResponse dTokenCache updateCacheStatus

pinEncryptedTokens :: MonadWidget t m
  => Dynamic t Text
  -> Maybe PasswordRaw
  -> Dynamic t Text
  -> Dynamic t [TokenCacheV3]
  -> m (Dynamic t [TokenCacheV3])
pinEncryptedTokens dWalletAddress mPass dKey dTokenCache = do
  -- TODO depend on pass
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

-- restore tokens from ipfs
-- that are minted and pinned only
restoreValidTokens :: MonadWidget t m
  => Dynamic t Text
  -> Dynamic t Text
  -> m (Dynamic t [TokenCacheV3])
restoreValidTokens dKey dWalletAddress = do
  let dClientId = mkClientId <$> dWalletAddress
  ev <- newEvent
  eeResotres <- restoreRequest dClientId ev
  let (eRestoreError, eRestoreResponses) = eventEither eeResotres
  dRestoreResponses <- holdDyn [] eRestoreResponses
  decryptTokens dKey dRestoreResponses

decryptToken :: MonadWidget t m
  => Text
  -> Event t ()
  -> Text
  -> m (Dynamic t Text)
decryptToken key ev encryptedHex = do
  let elementId = toText $ Hash.hash @Hash.SHA256 $ encodeUtf8 encryptedHex
  performEvent_ $ JS.decryptAES (key, elementId, encryptedHex) <$ ev
  dDecryptedToken <- elementResultJS elementId id
  -- logDyn "decryptToken: dDecryptedToken" dDecryptedToken
  pure dDecryptedToken

decryptTokens :: MonadWidget t m
  => Dynamic t Text
  -> Dynamic t [RestoreResponse]
  -> m (Dynamic t [TokenCacheV3])
decryptTokens dKey dRestores = do
  eTokens <- switchHoldDyn dKey $ \case
    "" -> pure never
    key -> switchHoldDyn dRestores $ \case
      [] -> pure never
      restores -> do
        fmap (updated . sequence) $ flip traverse restores $ \r -> do
            ev <- newEvent
            deDecryptedSecret <- decryptToken key ev $ tkTokenKey $ rrSecretKey r
            let deToken = mkTokenCacheV3 (rrAssetName r) <$> deDecryptedSecret
            pure deToken
  let (eErrors, eResponses) = eventTuple $ partitionEithers <$> eTokens
  dErrors <- holdDyn [] eErrors
  void $ switchHoldDyn dErrors $ \case
    [] -> pure never
    errs  -> do
      logDyn "decryptTokens: json decode errors" (constDyn errs)
      pure never
  let dValidLength = length <$> dRestores
  -- Returns tokens when they all are decrypted
  dRes <- foldDynMaybe
    (\(ac,ar) (bc,br) -> if length ar == ac then Just (ac,ar) else Nothing) (0,[]) $
    attachPromptlyDyn dValidLength eResponses
  logDyn "decryptTokens: dRes" dRes
  pure $ snd <$> dRes

mkTokenCacheV3 :: Text -> Text -> Either String TokenCacheV3
mkTokenCacheV3 name decrypted = case eSecret of
  Left err -> Left err
  Right secret ->
    let v = secretV secret
    in if F 0 <= v && v < F (2^(20 :: Integer))
          then Right $ MkTokenCacheV3
            { tcAssetName  = name
            , tcSecret     = secret
            , tcIpfsStatus = Pinned
            , tcCoinStatus = Minted
            }
          else Left "Invalid amount in the token from ipfs"
  where
    eSecret = eitherDecodeStrict' $ encodeUtf8 decrypted
