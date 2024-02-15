{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.IPFS where

import           Backend.Protocol.Types
import           Backend.Servant.Requests        (ipfsCacheRequest, restoreRequest)
import           Backend.Utility                 (eventEither, eventTuple, eventMaybeDynDef,
                                                  switchHoldDyn, toText)
import           ENCOINS.App.Widgets.Basic       (elementResultJS,
                                                  loadAppDataId, saveAppDataId,
                                                  saveAppDataId_)
import           ENCOINS.Bulletproofs            (Secret (..))
import           ENCOINS.Common.Cache            (encoinsV3, ipfsCacheKey,
                                                  isIpfsOn)
import           ENCOINS.Common.Events
import           ENCOINS.Common.Utils            (toJsonStrict)
import           ENCOINS.Common.Widgets.Advanced (dialogWindow)
import           ENCOINS.Crypto.Field            (Field (F))
import qualified JS.App                          as JS

import           Control.Monad                   (void)
import qualified Crypto.Hash                     as Hash
import           Data.Aeson                      (eitherDecodeStrict')
import           Data.Either                     (partitionEithers)
import           Data.List                       (find, foldl')
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Maybe                      (catMaybes, fromMaybe)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Text.Encoding              (decodeUtf8, encodeUtf8)
import           Reflex.Dom
import qualified Data.List.NonEmpty as NE
import  Data.List.NonEmpty (NonEmpty)

-- TODO:
-- 1. Add pinned status
  -- Visual statuses reload, done, failed


-- IPFS saving launches
-- when first page load for unpinned token on the left
-- and
-- when new token(s) appear on the left
saveTokensOnIpfs :: MonadWidget t m
  => Maybe PasswordRaw
  -> Dynamic t Bool
  -> Dynamic t (Maybe AesKeyRaw)
  -> Dynamic t [TokenCacheV3]
  -> m (Event t [TokenCacheV3])
saveTokensOnIpfs mPass dIpfsOn dmKey dTokenCache = mdo
  let dIpfsConditions = zipDynWith (,) dIpfsOn dmKey
  let dmValidTokens = getValidTokens <$> dTokenCache
  let dMkFullConditions = zipDynWith
        (\(isOn, mKey) ts -> (isOn, mKey, ts))
        dIpfsConditions

  -- logDyn "saveTokensOnIpfs: valid tokens before ipfs pin" dmValidTokens
  eTokenIpfsPinAttempt <- pinEncryptedTokens $ dMkFullConditions dmValidTokens
  -- logEvent "saveTokensOnIpfs: tokens after ipfs pin" $ showTokens <$> eTokenIpfsPinAttempt
  logEvent "saveTokensOnIpfs: token left unpinned" $ getValidTokens <$> eTokenIpfsPinAttempt

  let eTokenIpfsUnpinned = fmapMaybe id $ getValidTokens <$> eTokenIpfsPinAttempt
  logEvent "saveTokensOnIpfs: eTokenIpfsUnpinned" $ () <$ eTokenIpfsUnpinned

  dmValidTokens2 <- holdDyn Nothing $ Just <$> eTryPinTokens
  eTokenIpfsPinAttemptNext <- pinEncryptedTokens $ dMkFullConditions dmValidTokens2
  dTokenIpfsPinAttemptNext <- holdDyn [] eTokenIpfsPinAttemptNext
  eDelayTokens <- delay 10
    $ leftmost [Just <$> eTokenIpfsUnpinned, getValidTokens <$> eTokenIpfsPinAttemptNext]
  logEvent "saveTokensOnIpfs: eDelayTokens" $ () <$ eDelayTokens
  let (eTokenIpfsCachedComplete, eTryPinTokens) =
        eventMaybeDynDef dTokenIpfsPinAttemptNext eDelayTokens
  logEvent "saveTokensOnIpfs: token left unpinned 2" eTryPinTokens
  logEvent "saveTokensOnIpfs: eTokenIpfsCachedComplete" eTokenIpfsCachedComplete

  let eTokensToSave = leftmost [eTokenIpfsPinAttempt, eTokenIpfsCachedComplete]
  eSaved <- saveAppDataId mPass encoinsV3 eTokensToSave
  logEvent "saveTokensOnIpfs: eSaved 2" eSaved

  dTokenIpfsCached <- holdDyn [] eTokenIpfsPinAttempt
  pure $ tagPromptlyDyn dTokenIpfsCached eSaved

pinEncryptedTokens :: MonadWidget t m
  => Dynamic t (Bool, Maybe AesKeyRaw, Maybe (NonEmpty TokenCacheV3))
  -> m (Event t [TokenCacheV3])
pinEncryptedTokens dConditions = do
  switchHoldDyn dConditions $ \case
    (True, Just key, Just (NE.toList -> tokenCache)) -> do
              dCloudTokens <- encryptTokens key tokenCache
              let eFireCaching = ffilter (not . null) $ updated dCloudTokens
              -- logEvent "pinEncryptedTokens: fire ipfs pinning" $ () <$ eFireCaching
              let clientId = mkClientId key
              let dReq = (clientId,) <$> dCloudTokens
              eeCloudResponse <- ipfsCacheRequest dReq $ () <$ eFireCaching
              let (eCacheError, eCloudResponse) = eventEither eeCloudResponse
              dCacheError <- holdDyn "" eCacheError
              void $ switchHoldDyn dCacheError $ \case
                "" -> pure never
                errs  -> do
                  logDyn "pinEncryptedTokens: ipfsCacheRequest return error" (constDyn errs)
                  pure never
              dCloudResponse <- holdDyn Map.empty eCloudResponse
              pure $ updated $ updateCacheStatus tokenCache <$> dCloudResponse
    _ -> pure never

-- Encrypt valid tokens only and make request
encryptTokens :: MonadWidget t m
  => AesKeyRaw
  -> [TokenCacheV3]
  -> m (Dynamic t [CloudRequest])
encryptTokens key validTokens = do
  let validTokenNumber = length validTokens
  emClouds <- fmap (updated . sequence) $ flip traverse validTokens $ \t -> do
      ev <- newEvent
      dEncryptedSecret <- encryptToken key ev $ tcSecret t
      let dmCloud = fmap (mkCloudRequest t) <$> dEncryptedSecret
      pure dmCloud
    -- Wait until all tokens are encrypted and return them
  dRes <- foldDynMaybe
    (\(ac,ar) _ -> if length ar == ac then Just (ac,ar) else Nothing) (0,[]) $
    attachPromptlyDyn (constDyn validTokenNumber) $ catMaybes <$> emClouds
  pure $ snd <$> dRes

encryptToken :: MonadWidget t m
  => AesKeyRaw
  -> Event t ()
  -> Secret
  -> m (Dynamic t (Maybe EncryptedSecret))
encryptToken (MkAesKeyRaw keyText) ev secret = do
  let secretByte = toJsonStrict secret
  let elementId = toText $ Hash.hash @Hash.SHA256 secretByte
  performEvent_ $ JS.encryptAES (keyText, elementId, decodeUtf8 secretByte) <$ ev
  dEncryptedSecretT <- elementResultJS elementId id
  let checkEmpty t = if T.null t then Nothing else Just t
  pure $ fmap MkEncryptedSecret . checkEmpty <$> dEncryptedSecretT

-- Update ipfs metadata of tokens with response from ipfs
updateCacheStatus :: [TokenCacheV3]
  -> Map AssetName CloudResponse
  -> [TokenCacheV3]
updateCacheStatus oldToken tokensFromClouds = map updateCloud oldToken
  where
    updateCloud :: TokenCacheV3 -> TokenCacheV3
    updateCloud t =
      let name = tcAssetName t
          mStatus = Map.lookup name tokensFromClouds
      in case mStatus of
        Nothing -> t
        Just (MkCloudResponse mIpfs mCoin) ->
           t{ tcIpfsStatus = fromMaybe (tcIpfsStatus t) mIpfs
            , tcCoinStatus = fromMaybe (tcCoinStatus t) mCoin
            }

-- Valid tokens are the ones that are Minted and are not Pinned
getValidToken :: TokenCacheV3 -> Maybe TokenCacheV3
getValidToken t = case (tcIpfsStatus t, tcCoinStatus t)  of
  (Pinned, _) -> Nothing
  (_, Minted) -> Just t
  _           -> Nothing

getValidTokens :: [TokenCacheV3] -> Maybe (NonEmpty TokenCacheV3)
getValidTokens = NE.nonEmpty . catMaybes . map getValidToken

mkCloudRequest :: TokenCacheV3 -> EncryptedSecret -> CloudRequest
mkCloudRequest cache encryptedSecret = MkCloudRequest
    { reqAssetName  = tcAssetName cache
    , reqSecretKey  = encryptedSecret
    }

mkClientId :: AesKeyRaw -> AesKeyHash
mkClientId (MkAesKeyRaw key)
  = MkAesKeyHash
  $ toText
  $ Hash.hash @Hash.SHA256
  $ encodeUtf8 key

-- Generate aes 256 length key with function builtin any browser
-- And save it to local cache
getAesKey :: MonadWidget t m
  => Maybe PasswordRaw
  -> m (Event t (Maybe AesKeyRaw))
getAesKey mPass = do
  ev1 <- newEventWithDelay 0.1
  dLoadedKey <- fetchAesKey mPass "first-load-of-aes-key" ev1
  let ev2 = () <$ updated dLoadedKey
  ev3 <- delay 0.1 ev2
  switchHoldDyn dLoadedKey $ \case
    Nothing -> do
        let genElId = "genAESKeyId"
        performEvent_ $ JS.generateAESKey genElId <$ ev3
        eAesKeyText <- updated <$> elementResultJS genElId id
        logEvent "getAesKey: eAesKeyText" eAesKeyText
        let eAesKey = MkAesKeyRaw <$> eAesKeyText
        ev4 <- saveAppDataId mPass ipfsCacheKey eAesKey
        eLoadedKey <- updated <$> fetchAesKey mPass "second-load-of-aes-key" ev4
        pure eLoadedKey
    Just loadedKey -> pure $ (Just loadedKey) <$ ev3

fetchAesKey :: MonadWidget t m
  => Maybe PasswordRaw
  -> Text
  -> Event t ()
  -> m (Dynamic t (Maybe AesKeyRaw))
fetchAesKey mPass resId ev = loadAppDataId mPass ipfsCacheKey resId ev id Nothing

fetchIpfsFlag :: MonadWidget t m
  => Text
  -> Event t ()
  -> m (Dynamic t Bool)
fetchIpfsFlag resId ev = loadAppDataId Nothing isIpfsOn resId ev id False

-- restore tokens from ipfs
-- that are minted and pinned only
restoreValidTokens :: MonadWidget t m
  => Dynamic t (Maybe AesKeyRaw)
  -> m (Dynamic t [TokenCacheV3])
restoreValidTokens dmKey = do
  eTokens <- switchHoldDyn dmKey $ \case
    Nothing -> pure never
    Just key -> do
      let clientId = mkClientId key
      ev <- newEvent
      eeResotres <- restoreRequest (constDyn clientId) ev
      let (eRestoreError, eRestoreResponses) = eventEither eeResotres
      dRestoreResponses <- holdDyn [] eRestoreResponses
      updated <$> decryptTokens key dRestoreResponses
  holdDyn [] eTokens

decryptToken :: MonadWidget t m
  => AesKeyRaw
  -> Event t ()
  -> EncryptedSecret
  -> m (Dynamic t Text)
decryptToken (MkAesKeyRaw key) ev (MkEncryptedSecret encryptedHex) = do
  let elementId = toText $ Hash.hash @Hash.SHA256 $ encodeUtf8 encryptedHex
  performEvent_ $ JS.decryptAES (key, elementId, encryptedHex) <$ ev
  dDecryptedToken <- elementResultJS elementId id
  -- logDyn "decryptToken: dDecryptedToken" dDecryptedToken
  pure dDecryptedToken

decryptTokens :: MonadWidget t m
  => AesKeyRaw
  -> Dynamic t [RestoreResponse]
  -> m (Dynamic t [TokenCacheV3])
decryptTokens key dRestores = do
  eTokens <- switchHoldDyn dRestores $ \case
    [] -> pure never
    restores -> do
      fmap (updated . sequence) $ flip traverse restores $ \r -> do
          ev <- newEvent
          deDecryptedSecret <- decryptToken key ev $ rrEncryptedSecret r
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
  -- Wait until all tokens are decrypted and return them
  dRes <- foldDynMaybe
    (\(ac,ar) _ -> if length ar == ac then Just (ac,ar) else Nothing) (0,[]) $
    attachPromptlyDyn dValidLength eResponses
  pure $ snd <$> dRes

mkTokenCacheV3 :: AssetName -> Text -> Either String TokenCacheV3
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
          else Left "Invalid amount in the Secret from ipfs"
  where
    eSecret = eitherDecodeStrict' $ encodeUtf8 decrypted

ipfsSettingsWindow :: MonadWidget t m
  => Maybe PasswordRaw
  -> Dynamic t Bool
  -> Event t ()
  -> m (Dynamic t Bool, Dynamic t (Maybe AesKeyRaw))
ipfsSettingsWindow mPass ipfsCacheFlag eOpen = do
  dialogWindow
    True
    eOpen
    never
    "app-Ipfs_Window"
    "Save encoins on IPFS" $ do
      dIsIpfsOn <- ipfsCheckbox ipfsCacheFlag
      emKey <- switchHoldDyn dIsIpfsOn $ \case
        False -> pure never
        True -> do
          emKey <- getAesKey mPass
          dmKey <- holdDyn Nothing emKey
          divClass "app-Ipfs_AesKeyDescription" $ text "The following AES key is used to encrypt encoins before saving them"
          showKeyWidget dmKey
          pure emKey
      dmKey <- holdDyn Nothing emKey
      pure (dIsIpfsOn, dmKey)

ipfsCheckbox :: MonadWidget t m
  => Dynamic t Bool
  -> m (Dynamic t Bool)
ipfsCheckbox ipfsCacheFlag = do
  dIsChecked <- checkboxWidget (updated ipfsCacheFlag) "app-Ipfs_CheckboxToggle"
  saveAppDataId_ Nothing isIpfsOn $ updated dIsChecked
  pure dIsChecked

checkboxWidget :: MonadWidget t m
  => Event t Bool
  -> Text
  -> m (Dynamic t Bool)
checkboxWidget initial checkBoxClass = divClass "w-row app-Ipfs_CheckboxContainer" $ do
    inp <- inputElement $ def
      & initialAttributes .~
          ( "class" =: checkBoxClass <>
            "type" =: "checkbox"
          )
      & inputElementConfig_setChecked .~ initial
    divClass "app-Ipfs_CheckboxDescription" $ text "Check box in order to save your coins on IPFS"
    return $ _inputElement_checked inp

showKeyWidget :: MonadWidget t m
  => Dynamic t (Maybe AesKeyRaw)
  -> m ()
showKeyWidget dmKey = divClass "app-Ipfs_Key"
  $ dynText
  $ maybe "Ipfs key is not generated" getAesKeyRaw <$> dmKey

-- With ipfs wallet and ledger modes have two dynamics for token and save each of them separately
-- This function synchronize them.
-- Otherwise dynamic with old tokens rewrite new one.
-- TODO: consider use MVar or TWar to manage state.
-- O(n^2)
updateTokenState :: [TokenCacheV3] -> [TokenCacheV3] -> [TokenCacheV3]
updateTokenState old new = foldl' (update new) [] old
  where
    update ns acc o =
      case find (\n -> tcAssetName o == tcAssetName n) ns of
        Nothing -> o : acc
        Just n' -> n' : acc
