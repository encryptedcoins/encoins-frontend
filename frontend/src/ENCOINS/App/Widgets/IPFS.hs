{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.IPFS where

import           Backend.Protocol.Types
import           Backend.Servant.Requests        (ipfsCacheRequest,
                                                  restoreRequest)
import           Backend.Status                  (AppStatus,
                                                  IpfsSaveStatus (..))
import           Backend.Utility                 (eventEither, eventMaybeDynDef,
                                                  eventTuple, space,
                                                  switchHoldDyn, toText)
import           ENCOINS.App.Widgets.Basic       (elementResultJS,
                                                  loadAppDataId, saveAppDataId,
                                                  saveAppDataId_,
                                                  tellIpfsStatus)
import           ENCOINS.Bulletproofs            (Secret (..))
import           ENCOINS.Common.Cache            (encoinsV3, ipfsCacheKey,
                                                  isIpfsOn)
import           ENCOINS.Common.Events
import           ENCOINS.Common.Utils            (toJsonStrict)
import           ENCOINS.Common.Widgets.Advanced (copyEvent, dialogWindow,
                                                  withTooltip)
import           ENCOINS.Common.Widgets.Basic    (image)
import           ENCOINS.Crypto.Field            (Field (F))
import qualified JS.App                          as JS
import           JS.Website                      (copyText)

import           Control.Monad                   (void)
import           Control.Monad.IO.Class          (MonadIO (..))
import qualified Crypto.Hash                     as Hash
import           Data.Aeson                      (eitherDecodeStrict')
import           Data.Either                     (partitionEithers)
import           Data.List                       (find, foldl')
import           Data.List.NonEmpty              (NonEmpty)
import qualified Data.List.NonEmpty              as NE
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Maybe                      (catMaybes, fromMaybe, isJust,
                                                  isNothing)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Text.Encoding              (decodeUtf8, encodeUtf8)
import           Reflex.Dom
-- TODO:
-- 1. Add pinned status
  -- Visual statuses reload, done, failed


-- IPFS saving launches
-- when first page load for unpinned token on the left
-- and
-- when new token(s) appear on the left
saveTokensOnIpfs :: (MonadWidget t m, EventWriter t [AppStatus] m)
  => Maybe PasswordRaw
  -> Dynamic t Bool
  -> Dynamic t (Maybe AesKeyRaw)
  -> Dynamic t [TokenCacheV3]
  -> m (Event t [TokenCacheV3])
saveTokensOnIpfs mPass dIpfsOn dmKey dTokenCache = mdo
  let dIpfsConditions = zipDynWith (,) dIpfsOn dmKey
  let dmValidTokens = getValidTokens <$> dTokenCache
  let dFullConditions = combinePinConditions dIpfsConditions dmValidTokens

  -- logDyn "saveTokensOnIpfs: valid tokens before ipfs pin" dmValidTokens
  eTokenPinAttemptOne <- pinEncryptedTokens dFullConditions
  -- logEvent "saveTokensOnIpfs: tokens after ipfs pin" $ showTokens <$> eTokenPinAttemptOne
  -- logEvent "saveTokensOnIpfs: token left unpinned" emValidTokens

  -- BEGIN
  -- retry to pin tokens extra 5 times
  (eAttemptExcess, eTokenPinComplete) <-
    retryPinEncryptedTokens dIpfsConditions eTokenPinAttemptOne
  -- END

  -- The results of the first pinning we save in anyway
  let eTokensToSave = leftmost [eTokenPinAttemptOne, eTokenPinComplete]
  eSaved <- saveAppDataId mPass encoinsV3 eTokensToSave
  logEvent "saveTokensOnIpfs: eSaved 2" eSaved

  let emValidTokens = leftmost [getValidTokens <$> eTokensToSave, updated dmValidTokens]
  tellIpfsStatus $ leftmost
    [ PinnedAll <$ ffilter isNothing emValidTokens
    , Pinning <$ ffilter isJust emValidTokens
    , AttemptExcess <$ eAttemptExcess
    ]

  dTokenIpfsPinned <- holdDyn [] eTokensToSave
  pure $ tagPromptlyDyn dTokenIpfsPinned eSaved

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

-- if some Tokens are unpinned try to pin them 5 times with 10 seconds delay.
retryPinEncryptedTokens :: MonadWidget t m
  => Dynamic t (Bool, Maybe AesKeyRaw)
  -> Event t [TokenCacheV3]
  -> m (Event t (), Event t [TokenCacheV3])
retryPinEncryptedTokens dIpfsConditions eTokenIpfsPinAttempt = mdo
  let eTokenIpfsUnpinned = fmapMaybe id $ getValidTokens <$> eTokenIpfsPinAttempt
  -- logEvent "retryPinEncryptedTokens: eTokenIpfsUnpinned" $ () <$ eTokenIpfsUnpinned

  dmValidTokens2 <- holdDyn Nothing $ Just <$> eValidTokens3
  let dFullConditions = combinePinConditions dIpfsConditions dmValidTokens2
  eTokenIpfsPinAttemptNext <- pinEncryptedTokens dFullConditions
  dTokenIpfsPinAttemptNext <- holdDyn [] eTokenIpfsPinAttemptNext
  eDelayTokens <- delay 10
    $ leftmost [Just <$> eTokenIpfsUnpinned, getValidTokens <$> eTokenIpfsPinAttemptNext]
  -- logEvent "retryPinEncryptedTokens: eDelayTokens" $ () <$ eDelayTokens
  let (eTokenIpfsCachedComplete, eTryPinAgain) =
        eventMaybeDynDef dTokenIpfsPinAttemptNext eDelayTokens
  -- logEvent "retryPinEncryptedTokens: token left unpinned 2" eTryPinAgain
  -- logEvent "retryPinEncryptedTokens: eTokenIpfsCachedComplete" eTokenIpfsCachedComplete
  dAttemptCounter :: Dynamic t Int <- count eTryPinAgain
  let (eAttemptExcess, eValidTokens3) = fanEither $ attachPromptlyDynWith
        (\n ts -> if n > 5 then Left () else Right ts)
        dAttemptCounter
        eTryPinAgain
  -- logDyn "retryPinEncryptedTokens: dAttemptCounter" dAttemptCounter
  -- logEvent "retryPinEncryptedTokens: eAttemptExcess" eAttemptExcess
  -- logEvent "retryPinEncryptedTokens: eValidTokens3" eValidTokens3
  pure $ (eAttemptExcess, eTokenIpfsCachedComplete)

combinePinConditions :: Reflex t => Dynamic t (Bool, Maybe AesKeyRaw)
  -> Dynamic  t (Maybe (NonEmpty TokenCacheV3))
  -> Dynamic t (Bool, Maybe AesKeyRaw, Maybe (NonEmpty TokenCacheV3))
combinePinConditions = zipDynWith (\(isOn, mKey) ts -> (isOn, mKey, ts))

unpinStatusDebug :: [TokenCacheV3] -> [TokenCacheV3]
unpinStatusDebug = map (\t -> t{tcIpfsStatus = Unpinned})

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
  -> Dynamic t IpfsSaveStatus
  -> Event t ()
  -> m (Dynamic t Bool, Dynamic t (Maybe AesKeyRaw))
ipfsSettingsWindow mPass ipfsCacheFlag dIpfsSaveStatus eOpen = do
  dialogWindow
    True
    eOpen
    never
    "app-Ipfs_Window"
    "Encoins Cloud Backup" $ do
      dIsIpfsOn <- ipfsCheckbox ipfsCacheFlag
      ipfsSaveStatus dIpfsSaveStatus dIsIpfsOn
      emKey <- switchHoldDyn dIsIpfsOn $ \case
        False -> pure never
        True -> do
          emKey <- getAesKey mPass
          dmKey <- holdDyn Nothing emKey
          divClass "app-Ipfs_AesKey_Title" $
            text "Your AES key for restoring encoins. Save it to a file and keep it secure!"
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

ipfsSaveStatus :: MonadWidget t m
  => Dynamic t IpfsSaveStatus
  -> Dynamic t Bool
  -> m ()
ipfsSaveStatus dIpfsSaveStatus dIsIpfs = do
  divClass "app-Ipfs_Status_Title" $
    text "IPFS synchronization status"
  divClass "app-Ipfs_StatusText" $
    dynText $ zipDynWith selectIpfsStatusNote dIpfsSaveStatus dIsIpfs

selectIpfsStatusNote :: IpfsSaveStatus -> Bool -> Text
selectIpfsStatusNote status isIpfs =
  let t = case (status, isIpfs) of
        (_, False)         -> "is turned off"
        (TurnOff, _)       -> "is turned off"
        (Pinning, _)       -> "is in progress..."
        (PinnedAll, _)     -> "is completed successfully."
        (AttemptExcess, _) -> "failed"
  in "The synchronization" <> space <> t

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
    divClass "app-Ipfs_CheckboxDescription" $ text "Save encoins on IPFS"
    return $ _inputElement_checked inp

showKeyWidget :: MonadWidget t m
  => Dynamic t (Maybe AesKeyRaw)
  -> m ()
showKeyWidget dmKey = do
  let dKey = maybe "Ipfs key is not generated" getAesKeyRaw <$> dmKey
  let keyIcon = do
        e <- image "Key.svg" "inverted" "22px"
        void $ copyEvent e
        let eKey = tagPromptlyDyn dKey e
        performEvent_ (liftIO . copyText <$> eKey)
  divClass "app-Ipfs_KeyContainer" $ do
    divClass "key-div" $ withTooltip keyIcon "app-IpfsWindow_KeyTip" 0 0 $ do
      text "Tip: store it offline and protect with a password / encryption. Enable password protection in the Encoins app."
    dynText dKey

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
