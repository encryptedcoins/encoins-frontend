{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.IPFS where

import           Backend.Protocol.Types
import           Backend.Servant.Requests        (ipfsPinRequest,
                                                  ipfsPingRequest,
                                                  restoreRequest)
import           Backend.Status                  (AppStatus,
                                                  IpfsIconStatus (..))
import           Backend.Utility                 (eventEither, eventMaybeDynDef,
                                                  eventTuple, space,
                                                  switchHoldDyn, toText)
import           ENCOINS.App.Widgets.Basic       (elementResultJS, loadAppData,
                                                  saveAppData, saveAppData_,
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
import           Data.Align                      (align)
import           Data.Either                     (partitionEithers)
import           Data.List                       (find, foldl')
import           Data.List.NonEmpty              (NonEmpty)
import qualified Data.List.NonEmpty              as NE
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Maybe                      (catMaybes, isJust, isNothing)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Text.Encoding              (decodeUtf8, encodeUtf8)
import           Data.These                      (These)
import           Reflex.Dom


-- IPFS saving launches
-- when first page load for unpinned token on the left
-- and
-- when new token(s) appear on the left
saveTokensOnIpfs :: (MonadWidget t m, EventWriter t [AppStatus] m)
  => Dynamic t Bool
  -> Dynamic t (Maybe AesKeyRaw)
  -> Dynamic t [TokenCacheV3]
  -> m (Event t [TokenCacheV3])
saveTokensOnIpfs dIpfsOn dmKey dTokenInWallet = mdo
  let dIpfsConditions = zipDynWith (,) dIpfsOn dmKey
  -- Select unpinned tokens in wallet
  let dmUnpinnedTokens = selectUnpinnedTokens <$> dTokenInWallet
  -- Wait until tokens in wallet/ledger are loaded
  eInTokensFired <- delay 1 $ updated dTokenInWallet
  let emValidTokensFired = tagPromptlyDyn dmUnpinnedTokens eInTokensFired
  dmValidTokensInWalletFired <- holdDyn Nothing emValidTokensFired
  -- Combine all condition in a tuple
  let dFullConditions = combineConditions dIpfsConditions dmValidTokensInWalletFired

  (ePinError, eTokenPinAttemptOne) <- fanThese <$> pinEncryptedTokens dFullConditions

  -- BEGIN
  -- retry to pin tokens extra 5 times
  (eAttemptExcess, eTokenPinComplete) <- fanThese <$>
    retryPinEncryptedTokens dIpfsConditions eTokenPinAttemptOne
  -- END

  -- The results of the first pinning is saved anyway
  let eTokensToSave = leftmost [eTokenPinAttemptOne, eTokenPinComplete]

  let emValidTokens = leftmost [selectUnpinnedTokens <$> eTokensToSave, updated dmUnpinnedTokens]
  tellIpfsStatus $ leftmost
    [ PinnedAll <$ ffilter isNothing emValidTokens
    , Pinning <$ ffilter isJust emValidTokens
    , PinError <$ leftmost [ePinError, eAttemptExcess]
    ]

  pure eTokensToSave

pinEncryptedTokens :: MonadWidget t m
  => Dynamic t (Bool, Maybe AesKeyRaw, Maybe (NonEmpty TokenCacheV3))
  -> m (Event t (These () [TokenCacheV3]))
pinEncryptedTokens dConditions = do
  switchHoldDyn dConditions $ \case
    (True, Just key, Just (NE.toList -> tokenCache)) -> do
        dCloudTokens <- encryptTokens key tokenCache
        let eFireCaching = ffilter (not . null) $ updated dCloudTokens
        eePing <- ipfsPingRequest $ () <$ eFireCaching
        let (ePingError, ePingResponse) = eventEither eePing

        let clientId = mkClientId key
        let dReq = (clientId,) <$> dCloudTokens
        eeStatusResponse <- ipfsPinRequest dReq $ () <$ ePingResponse
        let (eCacheError, eStatusResponse) = eventEither eeStatusResponse

        dStatusResponse <- holdDyn Map.empty eStatusResponse
        let eUpdatedTokens = updated $ updateCacheStatus tokenCache <$> dStatusResponse
        let ePinError = () <$ leftmost [ePingError, eCacheError]
        pure $ align ePinError eUpdatedTokens
    _ -> pure never

-- if some Tokens are unpinned try to pin them 5 times with 10 seconds delay.
retryPinEncryptedTokens :: MonadWidget t m
  => Dynamic t (Bool, Maybe AesKeyRaw)
  -> Event t [TokenCacheV3]
  -> m (Event t (These () [TokenCacheV3]))
retryPinEncryptedTokens dIpfsConditions eTokenIpfsPinAttempt = mdo
  let eTokenIpfsUnpinned = fmapMaybe id $ selectUnpinnedTokens <$> eTokenIpfsPinAttempt
  dmValidTokens2 <- holdDyn Nothing $ Just <$> eValidTokens3
  let dFullConditions = combineConditions dIpfsConditions dmValidTokens2
  (ePinError, eTokenIpfsPinAttemptNext) <- fanThese <$> pinEncryptedTokens dFullConditions
  dTokenIpfsPinAttemptNext <- holdDyn [] eTokenIpfsPinAttemptNext
  eDelayTokens <- delay 10
    $ leftmost [Just <$> eTokenIpfsUnpinned, selectUnpinnedTokens <$> eTokenIpfsPinAttemptNext]
  let (eTokenIpfsCachedComplete, eTryPinAgain) =
        eventMaybeDynDef dTokenIpfsPinAttemptNext eDelayTokens
  dAttemptCounter :: Dynamic t Int <- count eTryPinAgain
  let (eAttemptExcess, eValidTokens3) = fanEither $ attachPromptlyDynWith
        (\n ts -> if n > 5 then Left () else Right ts)
        dAttemptCounter
        eTryPinAgain
  let eError = leftmost [ePinError, eAttemptExcess]
  pure $ align eError eTokenIpfsCachedComplete

combineConditions :: Reflex t => Dynamic t (Bool, Maybe AesKeyRaw)
  -> Dynamic t (Maybe (NonEmpty TokenCacheV3))
  -> Dynamic t (Bool, Maybe AesKeyRaw, Maybe (NonEmpty TokenCacheV3))
combineConditions = zipDynWith (\(isOn, mKey) ts -> (isOn, mKey, ts))

-- Encrypt valid tokens only and make request
encryptTokens :: MonadWidget t m
  => AesKeyRaw
  -> [TokenCacheV3]
  -> m (Dynamic t [PinRequest])
encryptTokens key validTokens = do
  let validTokenNumber = length validTokens
  emClouds <- fmap (updated . sequence) $ flip traverse validTokens $ \t -> do
      ev <- newEvent
      dEncryptedSecret <- encryptToken key ev $ tcSecret t
      let dmCloud = fmap (mkPinRequest t) <$> dEncryptedSecret
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
  -> Map AssetName StatusResponse
  -> [TokenCacheV3]
updateCacheStatus oldToken tokensFromClouds = map updateCloud oldToken
  where
    updateCloud :: TokenCacheV3 -> TokenCacheV3
    updateCloud t =
      let name = tcAssetName t
          mStatus = Map.lookup name tokensFromClouds
      in case mStatus of
        Nothing -> t
        -- If IPFS server returns a status with Nothing,
        -- then it uses old value.
        Just (MkStatusResponse ipfsStatus) ->
           t{ tcIpfsStatus = ipfsStatus
            }

-- Select valid tokens for pinning
-- Just created tokens with IpfsUndefined and MintUndefined statuses are selected to IPFS
-- Minted tokens that have Unpinned or IpfsError statuses are selected to IPFS again
-- Tokens with other statuses are not selected to IPFS
selectTokenToPin :: TokenCacheV3 -> Maybe TokenCacheV3
selectTokenToPin t = case tcIpfsStatus t  of
  (IpfsUndefined) -> Just t
  (Unpinned)      -> Just t
  (IpfsError)     -> Just t
  _               -> Nothing

selectUnpinnedTokens :: [TokenCacheV3] -> Maybe (NonEmpty TokenCacheV3)
selectUnpinnedTokens = NE.nonEmpty . catMaybes . map selectTokenToPin

mkPinRequest :: TokenCacheV3 -> EncryptedSecret -> PinRequest
mkPinRequest cache encryptedSecret = MkPinRequest
    { ppAssetName  = tcAssetName cache
    , ppSecretKey  = encryptedSecret
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
  -> Event t ()
  -> m (Event t (Maybe AesKeyRaw))
getAesKey mPass ev1 = do
  -- ev1 <- newEventWithDelay 0.1
  logEvent "getAesKey: ev1" ev1
  dLoadedKey <- fetchAesKey mPass "first-load-of-aes-key" ev1
  let ev2 = () <$ updated dLoadedKey
  ev3 <- delay 0.1 ev2
  switchHoldDyn dLoadedKey $ \case
    Nothing -> do
        let genElId = "genAESKeyId"
        performEvent_ $ JS.generateAESKey genElId <$ ev3
        eAesKeyText <- updated <$> elementResultJS genElId id
        let eAesKey = MkAesKeyRaw <$> eAesKeyText
        ev4 <- saveAppData mPass ipfsCacheKey eAesKey
        -- ev5 <- resetIpfsStatus mPass ev4
        eLoadedKey <- updated <$> fetchAesKey mPass "second-load-of-aes-key" ev4
        pure eLoadedKey
    Just loadedKey -> pure $ (Just loadedKey) <$ ev3

fetchAesKey :: MonadWidget t m
  => Maybe PasswordRaw
  -> Text
  -> Event t ()
  -> m (Dynamic t (Maybe AesKeyRaw))
fetchAesKey mPass resId ev = loadAppData mPass ipfsCacheKey resId ev id Nothing

resetIpfsStatus :: MonadWidget t m
  => Maybe PasswordRaw
  -> Event t ()
  -> m (Event t ())
resetIpfsStatus mPass ev = do
  dTokens <- loadAppData mPass encoinsV3 "resetIpfsStatus_load_tokens" ev id []
  let dResetTokens = map (\t -> t{ tcIpfsStatus = IpfsUndefined }) <$> dTokens
  saveAppData mPass encoinsV3 $ updated dResetTokens

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
            }
          else Left "Invalid amount in the Secret from ipfs"
  where
    eSecret = eitherDecodeStrict' $ encodeUtf8 decrypted

ipfsSettingsWindow :: MonadWidget t m
  => Maybe PasswordRaw
  -> Dynamic t Bool
  -> Dynamic t IpfsIconStatus
  -> Event t ()
  -> m (Dynamic t Bool, Dynamic t (Maybe AesKeyRaw))
ipfsSettingsWindow mPass ipfsCacheFlag dIpfsSaveStatus eOpen = do
  dialogWindow
    True
    eOpen
    never
    "app-Ipfs_Window"
    "Encoins Cloud Backup" $ do
      (dIsIpfsOn, eIpfsChange) <- ipfsCheckbox ipfsCacheFlag
      let eIpfsChangeVal = ffilter id $ tagPromptlyDyn dIsIpfsOn eIpfsChange
      eIpfsChangeValDelayed <- delay 0.1 eIpfsChangeVal
      ipfsIconStatus dIpfsSaveStatus dIsIpfsOn
      emKey <- switchHoldDyn dIsIpfsOn $ \case
        False -> pure never
        True -> do
          emKey <- getAesKey mPass $ leftmost [() <$ eIpfsChangeValDelayed, eOpen]
          dmKey <- holdDyn Nothing emKey
          divClass "app-Ipfs_AesKey_Title" $
            text "Your AES key for restoring encoins. Save it to a file and keep it secure!"
          showKeyWidget dmKey
          pure emKey
      dmKey <- holdDyn Nothing emKey
      pure (dIsIpfsOn, dmKey)

ipfsCheckbox :: MonadWidget t m
  => Dynamic t Bool
  -> m (Dynamic t Bool, Event t Bool)
ipfsCheckbox ipfsCacheFlag = do
  (dIsChecked, eIpfsChange) <- checkboxWidget (updated ipfsCacheFlag) "app-Ipfs_CheckboxToggle"
  saveAppData_ Nothing isIpfsOn $ updated dIsChecked
  pure (dIsChecked, eIpfsChange)

ipfsIconStatus :: MonadWidget t m
  => Dynamic t IpfsIconStatus
  -> Dynamic t Bool
  -> m ()
ipfsIconStatus dIpfsSaveStatus dIsIpfs = do
  divClass "app-Ipfs_Status_Title" $
    text "IPFS synchronization status"
  divClass "app-Ipfs_StatusText" $
    dynText $ zipDynWith selectIpfsStatusNote dIpfsSaveStatus dIsIpfs

selectIpfsStatusNote :: IpfsIconStatus -> Bool -> Text
selectIpfsStatusNote status isIpfs =
  let t = case (status, isIpfs) of
        (_, False)         -> "is turned off"
        (NoTokens, _)      -> "is impossible. There are not tokens in the local cache"
        (Pinning, _)       -> "is in progress..."
        (PinnedAll, _)     -> "is completed successfully."
        (PinError, _) -> "failed"
  in "The synchronization" <> space <> t

checkboxWidget :: MonadWidget t m
  => Event t Bool
  -> Text
  -> m (Dynamic t Bool, Event t Bool)
checkboxWidget initial checkBoxClass = divClass "w-row app-Ipfs_CheckboxContainer" $ do
    inp <- inputElement $ def
      & initialAttributes .~
          ( "class" =: checkBoxClass <>
            "type" =: "checkbox"
          )
      & inputElementConfig_setChecked .~ initial
    divClass "app-Ipfs_CheckboxDescription" $ text "Save encoins on IPFS"
    pure (_inputElement_checked inp, _inputElement_checkedChange inp)

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
updateMintedTokens :: [TokenCacheV3] -> [TokenCacheV3] -> [TokenCacheV3]
updateMintedTokens old new = foldl' (update new) [] old
  where
    update ns acc o =
      case find (\n -> tcAssetName o == tcAssetName n) ns of
        Nothing -> o : acc
        Just n' -> n' : acc
