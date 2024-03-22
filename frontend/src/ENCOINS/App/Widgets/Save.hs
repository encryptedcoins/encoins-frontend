{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.Save where

import           Backend.Protocol.Types
import           Backend.Servant.Requests        (restoreRequest,
                                                  savePingRequest, saveRequest)
import           Backend.Status                  (AppStatus,
                                                  SaveIconStatus (..))
import           Backend.Utility                 (eventEither, eventMaybeDynDef,
                                                  eventTuple, space,
                                                  switchHoldDyn, toText)
import           ENCOINS.App.Widgets.Basic       (elementResultJS, loadAppData,
                                                  saveAppData, saveAppData_,
                                                  tellSaveStatus)
import           ENCOINS.Bulletproofs            (Secret (..))
import           ENCOINS.Common.Cache            (aesKey, isSaveOn)
import           ENCOINS.Common.Events
import           ENCOINS.Common.Utils            (toJsonStrict)
import           ENCOINS.Common.Widgets.Advanced (copyEvent, dialogWindow,
                                                  withTooltip)
import           ENCOINS.Common.Widgets.Basic    (image)
import           ENCOINS.Crypto.Field            (Field (F))
import qualified JS.App                          as JS
import           JS.Website                      (copyText)

import           Control.Monad                   (void, (<=<))
import           Control.Monad.IO.Class          (MonadIO (..))
import qualified Crypto.Hash                     as Hash
import           Data.Aeson                      (eitherDecodeStrict')
import           Data.Align                      (align)
import           Data.Either                     (partitionEithers)
import           Data.Functor                    ((<&>))
import           Data.List                       (find, foldl', union)
import           Data.List.NonEmpty              (NonEmpty)
import qualified Data.List.NonEmpty              as NE
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Maybe                      (catMaybes)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Text.Encoding              (decodeUtf8, encodeUtf8)
import           Data.These                      (These)
import           Reflex.Dom


handleUnsavedTokens :: (MonadWidget t m, EventWriter t [AppStatus] m)
  => Dynamic t Bool
  -> Dynamic t (Maybe AesKeyRaw)
  -> Dynamic t [TokenCacheV3]
  -> Dynamic t [TokenCacheV3]
  -> m (Dynamic t [TokenCacheV3])
handleUnsavedTokens dSaveOn dmKey dTokenCache dTokensInWallet = mdo
  logDyn "handleUnsavedTokens: dSaveOn" dSaveOn
  logDyn "handleUnsavedTokens: dmKey" dmKey
  dTokenCacheUniq <- holdUniqDyn dTokenCache
  dTokensInWalletUniq <- holdUniqDyn dTokensInWallet
  logDyn "handleUnsavedTokens: left" $ showTokens <$> dTokensInWalletUniq
  -- create event when 2 conditions are true
  -- 1. Left column is fired
  -- 2. Left column has tokens that are unsaved
  let eTokenCacheUniqFired = attachPromptlyDynWithMaybe
        (\tokens tokensInWallet -> tokens <$ selectUnsavedTokens tokensInWallet)
        dTokenCacheUpdated
        (updated dTokensInWalletUniq)

  dTokenCacheUniqFired <- holdDyn [] eTokenCacheUniqFired
  logDyn "handleUnsavedTokens: in" $ showTokens <$> dTokenCacheUniqFired
  eTokenWithNewState <- saveTokens dSaveOn dmKey dTokenCacheUniqFired
  -- TODO: consider better union method for eliminating duplicates.
  dTokenSaveSynched <- foldDyn union [] eTokenWithNewState
  logDyn "handleUnsavedTokens: out" $ showTokens <$> dTokenSaveSynched
  -- Update statuses of dTokenCache before saving on server first one.
  let dTokenCacheUpdated = zipDynWith
        updateMintedTokens
        dTokenCacheUniq
        dTokenSaveSynched
  logDyn "handleUnsavedTokens: final cache" $ showTokens <$> dTokenCacheUpdated
  let eUnsavedTokens = updated $ selectUnsavedTokens <$> dTokenCacheUpdated
  tellSaveStatus $ leftmost
    [ AllSaved <$ ffilter null eUnsavedTokens
    , FailedSave <$ ffilter (not . null) eUnsavedTokens
    ]
  pure dTokenCacheUpdated

-- Saving launches
-- when first page loaded
-- and
-- when new token(s) appear on the left
saveTokens :: (MonadWidget t m, EventWriter t [AppStatus] m)
  => Dynamic t Bool
  -> Dynamic t (Maybe AesKeyRaw)
  -> Dynamic t [TokenCacheV3]
  -> m (Event t [TokenCacheV3])
saveTokens dSaveOn dmKey dTokens = mdo
  let dSaveConditions = zipDynWith (,) dSaveOn dmKey
  -- Select unpinned tokens in wallet
  let dmUnpinnedTokens = selectUnsavedTokens <$> dTokens
  -- Combine all condition in a tuple
  let dFullConditions = combineConditions dSaveConditions dmUnpinnedTokens

  (eSaveError, eTokenSaveAttemptOne) <- fanThese <$> saveEncryptedTokens dFullConditions
  logEvent "save error" eSaveError

  -- BEGIN
  -- retry to pin tokens extra 5 times
  (eAttemptExcess, eTokenPinComplete) <- fanThese <$>
    retrySaveEncryptedTokens dSaveConditions eTokenSaveAttemptOne
  -- END
  logEvent "excess of save attempts" eAttemptExcess

  -- The results of the first pinning is saved anyway
  let eTokensToSave = leftmost [eTokenSaveAttemptOne, eTokenPinComplete]

  pure eTokensToSave

switchHoldDynWriter :: (MonadWidget t m, EventWriter t [AppStatus] m)
  => Dynamic t a
  -> (a -> m (Event t b))
  -> m (Event t b)
switchHoldDynWriter da f = switchHold never <=< dyn $ da <&> f

saveEncryptedTokens :: (MonadWidget t m, EventWriter t [AppStatus] m)
  => Dynamic t (Bool, Maybe AesKeyRaw, Maybe (NonEmpty TokenCacheV3))
  -> m (Event t (These () [TokenCacheV3]))
saveEncryptedTokens dConditions = do
  switchHoldDynWriter dConditions $ \case
    (True, Just key, Just (NE.toList -> tokenCache)) -> do
        dCloudTokens <- encryptTokens key tokenCache
        let eFireCaching = ffilter (not . null) $ updated dCloudTokens
        tellSaveStatus $ Saving <$ eFireCaching
        eePing <- savePingRequest $ () <$ eFireCaching
        let (ePingError, ePingResponse) = eventEither eePing

        eeStatusResponse <- saveRequest dCloudTokens $ () <$ ePingResponse
        let (eCacheError, eStatusResponse) = eventEither eeStatusResponse

        dStatusResponse <- holdDyn Map.empty eStatusResponse
        let eUpdatedTokens = updated $ updateCacheStatus tokenCache <$> dStatusResponse
        let eSaveError = () <$ leftmost [ePingError, eCacheError]
        pure $ align eSaveError eUpdatedTokens
    _ -> pure never

-- if some Tokens are unpinned try to pin them 5 times with 10 seconds delay.
retrySaveEncryptedTokens :: (MonadWidget t m, EventWriter t [AppStatus] m)
  => Dynamic t (Bool, Maybe AesKeyRaw)
  -> Event t [TokenCacheV3]
  -> m (Event t (These () [TokenCacheV3]))
retrySaveEncryptedTokens dSaveConditions eTokenSaveAttempt = mdo
  let eTokenUnsaved = fmapMaybe id $ selectUnsavedTokens <$> eTokenSaveAttempt
  dmValidTokens2 <- holdDyn Nothing $ Just <$> eValidTokens3
  let dFullConditions = combineConditions dSaveConditions dmValidTokens2
  (eSaveError, eTokenSaveAttemptNext) <- fanThese <$> saveEncryptedTokens dFullConditions
  dTokenSaveAttemptNext <- holdDyn [] eTokenSaveAttemptNext
  eDelayTokens <- delay 10
    $ leftmost [Just <$> eTokenUnsaved, selectUnsavedTokens <$> eTokenSaveAttemptNext]
  let (eTokenCachedComplete, eTrySaveAgain) =
        eventMaybeDynDef dTokenSaveAttemptNext eDelayTokens
  dAttemptCounter :: Dynamic t Int <- count eTrySaveAgain
  let (eAttemptExcess, eValidTokens3) = fanEither $ attachPromptlyDynWith
        (\n ts -> if n > 5 then Left () else Right ts)
        dAttemptCounter
        eTrySaveAgain
  let eError = leftmost [eSaveError, eAttemptExcess]
  pure $ align eError eTokenCachedComplete

combineConditions :: Reflex t => Dynamic t (Bool, Maybe AesKeyRaw)
  -> Dynamic t (Maybe (NonEmpty TokenCacheV3))
  -> Dynamic t (Bool, Maybe AesKeyRaw, Maybe (NonEmpty TokenCacheV3))
combineConditions = zipDynWith (\(isOn, mKey) ts -> (isOn, mKey, ts))

-- Encrypt valid tokens only and make request
encryptTokens :: MonadWidget t m
  => AesKeyRaw
  -> [TokenCacheV3]
  -> m (Dynamic t [SaveRequest])
encryptTokens key validTokens = do
  let validTokenNumber = length validTokens
  emClouds <- fmap (updated . sequence) $ flip traverse validTokens $ \t -> do
      ev <- newEvent
      dEncryptedSecret <- encryptToken key ev $ tcSecret t
      let dmCloud = fmap (mkSaveRequest t) <$> dEncryptedSecret
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

-- Update save status of tokens with response from save server
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
        -- If Save server returns a status with Nothing,
        -- then it uses old value.
        Just (MkStatusResponse saveStatus) ->
           t{ tcSaveStatus = saveStatus
            }

-- Select valid tokens for saving
-- Just created tokens with 'SaveUndefined' status are selected for saving
-- Minted tokens that have 'SaveUndefined' or 'SaveError' statuses are selected for saving again
-- Tokens with other statuses are not selected for saving
selectTokenToSave :: TokenCacheV3 -> Maybe TokenCacheV3
selectTokenToSave t = case tcSaveStatus t  of
  (SaveUndefined) -> Just t
  (SaveError)     -> Just t
  _               -> Nothing

selectUnsavedTokens :: [TokenCacheV3] -> Maybe (NonEmpty TokenCacheV3)
selectUnsavedTokens = NE.nonEmpty . catMaybes . map selectTokenToSave

mkSaveRequest :: TokenCacheV3 -> EncryptedSecret -> SaveRequest
mkSaveRequest cache encryptedSecret = MkSaveRequest
    { srAssetName  = tcAssetName cache
    , srSecretKey  = encryptedSecret
    }

-- Generate aes 256 length key with function builtin any browser
-- And save it to local cache
genAesKey :: MonadWidget t m
  => Maybe PasswordRaw
  -> Dynamic t (Maybe AesKeyRaw)
  -> Event t ()
  -> m (Dynamic t (Maybe AesKeyRaw))
genAesKey mPass dmCachedKey ev1 = do
  ev2 <- delay 0.1 ev1
  eNewLoadedKey <- switchHoldDyn dmCachedKey $ \case
    Nothing -> do
        let genElId = "genAESKeyId"
        performEvent_ $ JS.generateAESKey genElId <$ ev2
        eAesKeyText <- updated <$> elementResultJS genElId id
        let eAesKey = MkAesKeyRaw <$> eAesKeyText
        ev3 <- saveAppData mPass aesKey eAesKey
        eLoadedKey <- updated <$> fetchAesKey mPass "second-load-of-aes-key" ev3
        pure eLoadedKey
    Just loadedKey -> pure $ (Just loadedKey) <$ ev2
  dNewLoadedKey <- holdDyn Nothing eNewLoadedKey
  pure dNewLoadedKey

fetchAesKey :: MonadWidget t m
  => Maybe PasswordRaw
  -> Text
  -> Event t ()
  -> m (Dynamic t (Maybe AesKeyRaw))
fetchAesKey mPass resId ev = loadAppData mPass aesKey resId ev id Nothing

-- restore tokens from save server
-- that are minted and saved only
restoreValidTokens :: MonadWidget t m
  => Dynamic t (Maybe AesKeyRaw)
  -> m (Dynamic t [TokenCacheV3])
restoreValidTokens dmKey = do
  eTokens <- switchHoldDyn dmKey $ \case
    Nothing -> pure never
    Just key -> do
      ev <- newEvent
      eeResotres <- restoreRequest ev
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
            , tcSaveStatus = Saved
            }
          else Left "Invalid amount in the Secret from save server"
  where
    eSecret = eitherDecodeStrict' $ encodeUtf8 decrypted

saveSettingsWindow :: MonadWidget t m
  => Maybe PasswordRaw
  -> Dynamic t Bool
  -> Dynamic t SaveIconStatus
  -> Event t ()
  -> m (Dynamic t Bool, Dynamic t (Maybe AesKeyRaw), Dynamic t (Maybe AesKeyRaw))
saveSettingsWindow mPass saveCacheFlag dSaveStatus eOpen = do
  dialogWindow
    True
    eOpen
    never
    "app-Save_Window"
    "Encoins Cloud Backup" $ do
      (dIsSaveOn, eSaveChange) <- saveCheckbox saveCacheFlag
      saveIconStatus dSaveStatus dIsSaveOn

      let eSaveChangeVal = ffilter id $ tagPromptlyDyn dIsSaveOn eSaveChange
      eSaveChangeValDelayed <- delay 0.1 eSaveChangeVal

      dmOldKey <- fetchAesKey mPass "first-load-of-aes-key" $ leftmost [() <$ eSaveChangeValDelayed, eOpen]
      -- logDyn "saveSettingsWindow: dmOldKey" dmOldKey
      emKey <- switchHoldDyn dIsSaveOn $ \case
        False -> pure never
        True -> do
          dmNewKey <- genAesKey mPass dmOldKey $ leftmost [() <$ eSaveChangeValDelayed, eOpen]
          divClass "app-Save_AesKey_Title" $
            text "Your AES key for restoring encoins. Save it to a file and keep it secure!"
          showKeyWidget dmNewKey
          pure $ updated dmNewKey
      dmNewKey <- holdDyn Nothing emKey
      pure (dIsSaveOn, dmNewKey, dmOldKey)

resetTokens :: Reflex t => Dynamic t (Maybe [TokenCacheV3]) -> Dynamic t Bool -> Dynamic t (Maybe [TokenCacheV3])
resetTokens dmTokens dKeyWasReset =
  let resetToken t = t{ tcSaveStatus = SaveUndefined }
  in zipDynWith
    (\mTokens wasReset -> if wasReset then map resetToken <$> mTokens else mTokens)
    dmTokens
    dKeyWasReset

saveCheckbox :: MonadWidget t m
  => Dynamic t Bool
  -> m (Dynamic t Bool, Event t Bool)
saveCheckbox saveCacheFlag = do
  (dIsChecked, eSaveChange) <- checkboxWidget (updated saveCacheFlag) "app-Save_CheckboxToggle"
  saveAppData_ Nothing isSaveOn $ updated dIsChecked
  pure (dIsChecked, eSaveChange)

saveIconStatus :: MonadWidget t m
  => Dynamic t SaveIconStatus
  -> Dynamic t Bool
  -> m ()
saveIconStatus dSaveStatus dIsSave = do
  divClass "app-Save_Status_Title" $
    text "Save synchronization status"
  divClass "app-Save_StatusText" $
    dynText $ zipDynWith selectSaveStatusNote dSaveStatus dIsSave

selectSaveStatusNote :: SaveIconStatus -> Bool -> Text
selectSaveStatusNote status isSave =
  let t = case (status, isSave) of
        (_, False)         -> "is turned off"
        (NoTokens, _)      -> "is impossible. There are not tokens in the local cache"
        (Saving, _)       -> "is in progress..."
        (AllSaved, _)     -> "is completed successfully."
        (FailedSave, _) -> "failed"
  in "The synchronization" <> space <> t

checkboxWidget :: MonadWidget t m
  => Event t Bool
  -> Text
  -> m (Dynamic t Bool, Event t Bool)
checkboxWidget initial checkBoxClass = divClass "w-row app-Save_CheckboxContainer" $ do
    inp <- inputElement $ def
      & initialAttributes .~
          ( "class" =: checkBoxClass <>
            "type" =: "checkbox"
          )
      & inputElementConfig_setChecked .~ initial
    divClass "app-Save_CheckboxDescription" $ text "Save encoins on cloud"
    pure (_inputElement_checked inp, _inputElement_checkedChange inp)

showKeyWidget :: MonadWidget t m
  => Dynamic t (Maybe AesKeyRaw)
  -> m ()
showKeyWidget dmKey = do
  let dKey = maybe "Save key is not generated" getAesKeyRaw <$> dmKey
  let keyIcon = do
        e <- image "Key.svg" "inverted" "22px"
        void $ copyEvent e
        let eKey = tagPromptlyDyn dKey e
        performEvent_ (liftIO . copyText <$> eKey)
  divClass "app-Save_KeyContainer" $ do
    divClass "key-div" $ withTooltip keyIcon "app-SaveWindow_KeyTip" 0 0 $ do
      text "Tip: store it offline and protect with a password / encryption. Enable password protection in the Encoins app."
    dynText dKey

-- With saving wallet and ledger modes have two dynamics for token
-- and save each of them separately
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
