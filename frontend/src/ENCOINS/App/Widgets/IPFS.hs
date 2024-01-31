{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.IPFS where

import           Backend.Protocol.Types
import           Backend.Servant.Requests           (cacheRequest,
                                                     restoreRequest)
import           Backend.Utility                    (eventEither, eventTuple,
                                                     switchHoldDyn)
import           ENCOINS.App.Widgets.Basic          (elementResultJS,
                                                     loadAppDataId,
                                                     saveAppDataId)
import           ENCOINS.App.Widgets.PasswordWindow (PasswordRaw, getPassRaw)
import           ENCOINS.Bulletproofs               (Secret (..))
import           ENCOINS.Common.Cache               (ipfsCacheKey, isIpfsOn)
import           ENCOINS.Common.Events
import           ENCOINS.Common.Utils               (toText)
import           ENCOINS.Common.Widgets.Advanced    (dialogWindow)
import           ENCOINS.Crypto.Field               (Field (F))
import qualified JS.App                             as JS
import           JS.Website                         (saveJSON)

import           Control.Monad                      (void)
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

pinEncryptedTokens :: MonadWidget t m
  => Dynamic t Text
  -> Maybe PasswordRaw
  -> Dynamic t (Maybe Text)
  -> Dynamic t Bool
  -> Dynamic t [TokenCacheV3]
  -> m (Dynamic t [TokenCacheV3])
pinEncryptedTokens dWalletAddress mPass dmKey dIpfsOn dTokenCache = do
  case mPass of
    Nothing -> pure dTokenCache
    Just _ -> do
      eTokens <- switchHoldDyn dIpfsOn $ \case
        False -> do
          logDyn "pinEncryptedTokens: dIpfsOn" dIpfsOn
          pure $ updated dTokenCache
        True -> do
          switchHoldDyn dmKey $ \case
            Nothing -> do
              logDyn "pinEncryptedTokens: dmKey" dmKey
              pure $ updated dTokenCache
            Just key -> do
              dCloudTokens <- encryptTokens key dTokenCache
              logDyn "pinEncryptedTokens: dCloudTokens" dCloudTokens
              let eFireCaching = ffilter (not . null) $ updated dCloudTokens
              logEvent "pinEncryptedTokens: eFireCaching" eFireCaching
              let dClientId = mkClientId <$> dWalletAddress
              let dReq = zipDynWith (,) dClientId dCloudTokens
              logDyn "pinEncryptedTokens: dClientId" dClientId
              eeCloudResponse <- cacheRequest dReq $ () <$ eFireCaching
              logEvent "pinEncryptedTokens: cache response:" eeCloudResponse
              let (eCacheError, eCloudResponse) = eventEither eeCloudResponse
              dCloudResponse <- holdDyn Map.empty eCloudResponse
              pure $ updated $ ffor2 dCloudResponse dTokenCache updateCacheStatus
      holdDyn [] eTokens

encryptTokens :: MonadWidget t m
  => Text
  -> Dynamic t [TokenCacheV3]
  -> m (Dynamic t [CloudRequest])
encryptTokens key dTokens = do
  let dValidTokens = catMaybes . map getValidTokens <$> dTokens
  let dValidTokenNumber = length <$> dValidTokens
  eClouds <- switchHoldDyn dValidTokens $ \case
      [] -> pure never
      tokens -> do
        fmap (updated . sequence) $ flip traverse tokens $ \t -> do
            ev <- newEvent
            dEncryptedSecret <- encryptToken key ev $ tcSecret t
            let dmCloud = mkCloudRequest t <$> dEncryptedSecret
            pure dmCloud
  -- logDyn "encryptTokens: dValidTokens" dValidTokens
  -- logDyn "encryptTokens: dValidTokenNumber" dValidTokenNumber
  -- logEvent "encryptTokens: eClouds" eClouds
    -- Returns tokens when they all are decrypted
  dRes <- foldDynMaybe
    (\(ac,ar) _ -> if length ar == ac then Just (ac,ar) else Nothing) (0,[]) $
    attachPromptlyDyn dValidTokenNumber $ catMaybes <$> eClouds
  -- logDyn "encryptTokens: dRes" dRes
  pure $ snd <$> dRes

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

getValidTokens :: TokenCacheV3 -> Maybe TokenCacheV3
getValidTokens t = case (tcIpfsStatus t, tcCoinStatus t)  of
  (Unpinned, Minted) -> Just t
  _                  -> Nothing

mkCloudRequest :: TokenCacheV3 -> Text -> Maybe CloudRequest
mkCloudRequest cache encryptedSecret = if not (T.null encryptedSecret)
  then Just $ MkCloudRequest
    { reqAssetName  = tcAssetName cache
    , reqSecretKey  = encryptedSecret
    }
  else Nothing

mkClientId :: Text -> Text
mkClientId address = toText $ Hash.hash @Hash.SHA256 $ encodeUtf8 address

-- TODO: remove after debug
tokenSample :: CloudRequest
tokenSample = MkCloudRequest
  { reqAssetName = "b47f55bdc1d7615409cf8cc714e3885c42d6cb48629d44ff5a9265c88aa30cdc"
  , reqSecretKey = "super secret key"
  }

mkAesKey :: MonadWidget t m
  => Maybe PasswordRaw
  -> m (Event t Text)
mkAesKey mPass = do
  ev1 <- newEventWithDelay 0.1
  dLoadedKey <- loadAppDataId
    mPass ipfsCacheKey "first-load-of-aes-key" ev1 id Nothing
  let ev2 = () <$ updated dLoadedKey
  ev3 <- delay 0.1 ev2
  switchHoldDyn dLoadedKey $ \case
    Nothing -> do
        let genElId = "genAESKeyId"
        performEvent_ $ JS.generateAESKey genElId <$ ev3
        eAesKey <- updated <$> elementResultJS genElId id
        logEvent "mkAesKey: eAesKey" eAesKey
        ev4 <- performEvent $ saveJSON (getPassRaw <$> mPass) ipfsCacheKey
          . decodeUtf8
          . toStrict
          . encode <$> eAesKey
        eLoadedKey <- updated <$> loadAppDataId
          mPass ipfsCacheKey  "second-load-of-aes-key"  ev4 id ""
        pure eLoadedKey
    Just loadedKey -> pure $ loadedKey <$ ev3

fetchAesKey :: MonadWidget t m
  => Maybe PasswordRaw
  -> Event t ()
  -> m (Dynamic t (Maybe Text))
fetchAesKey mPass ev = loadAppDataId
    mPass ipfsCacheKey "fetchAesKey-load-of-aes-key" ev id Nothing

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
    (\(ac,ar) _ -> if length ar == ac then Just (ac,ar) else Nothing) (0,[]) $
    attachPromptlyDyn dValidLength eResponses
  -- logDyn "decryptTokens: dRes" dRes
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

ipfsSettingsWindow :: MonadWidget t m
  => Maybe PasswordRaw
  -> Event t ()
  -> m (Dynamic t Bool)
ipfsSettingsWindow mPass eOpen = do
  logEvent "ipfsSettingsWindow: eOpen" eOpen
  dialogWindow
    True
    eOpen
    never
    ipfsWindowStyle
    "Save encoins on IPFS" $ mdo
        case mPass of
          Nothing -> do
            divClass "app-Ipfs_Trigger-NeedPassword"
              $ text "Saving encoins on IPFS does not work without local password"
            pure $ constDyn False
          Just _ -> do
            dIsIpfsOn <- ipfsCheckbox eOpen
            void $ switchHoldDyn dIsIpfsOn $ \case
              False -> pure never
              True -> do
                eKey <- mkAesKey mPass
                dKey <- holdDyn "Ipfs key not found" eKey
                divClass "" $ text "Following AES key used to encrypt encoins before saving them on IPFS"
                showKeyWidget dKey
                pure never
            pure dIsIpfsOn

ipfsWindowStyle :: Text
ipfsWindowStyle = "width: min(90%, 950px); padding-left: min(5%, 70px); padding-right: min(5%, 70px); padding-top: min(5%, 30px); padding-bottom: min(5%, 30px);"

ipfsCheckbox :: MonadWidget t m
  => Event t ()
  -> m (Dynamic t Bool)
ipfsCheckbox eOpen = do
  dIpfsCached <- loadAppDataId Nothing isIpfsOn "load-is-ipfs-on-key" eOpen id False
  dIsChecked <- checkboxWidget (updated dIpfsCached) "app-Ipfs_Trigger"
  saveAppDataId Nothing isIpfsOn $ updated dIsChecked
  divClass "" $ text "Check box in order to save your coins on IPFS"
  pure dIsChecked

checkboxWidget :: MonadWidget t m
  => Event t Bool
  -> Text
  -> m (Dynamic t Bool)
checkboxWidget initial checkBoxClass = divClass "w-row" $ do
    inp <- inputElement $ def
      & initialAttributes .~
          ( "class" =: checkBoxClass <>
            "type" =: "checkbox"
          )
      & inputElementConfig_setChecked .~ initial
    return $ _inputElement_checked inp

showKeyWidget :: MonadWidget t m
  => Dynamic t Text
  -> m ()
showKeyWidget dKey = do
  divClass "app-Ipfs_Key" $ dynText dKey
