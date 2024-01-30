{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.IPFS where

import           Backend.Protocol.Types
import           Backend.Servant.Requests           (cacheRequest,
                                                     restoreRequest)
import           Backend.Utility                    (eventEither, eventTuple,
                                                     switchHoldDyn)
import           ENCOINS.App.Widgets.Basic          (elementResultJS, genUid,
                                                     loadAppDataId)
import           ENCOINS.App.Widgets.PasswordWindow (PasswordRaw, getPassRaw)
import           ENCOINS.Bulletproofs               (Secret (..))
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
  -> Dynamic t [TokenCacheV3]
  -> m (Dynamic t [TokenCacheV3])
pinEncryptedTokens dWalletAddress mPass dmKey dTokenCache = do
  case mPass of
    Nothing -> pure dTokenCache
    Just _ -> do
      eTokens <- switchHoldDyn dmKey $ \case
        Nothing -> do
          ev <- newEvent
          pure $ [] <$ ev
        Just key -> do
          dCloudTokens <- encryptTokens key dTokenCache
          -- logDyn "dCloudTokens" dCloudTokens
          let eFireCaching = ffilter (not . null) $ updated dCloudTokens
          -- logEvent "eFireCaching" eFireCaching
          let dClientId = mkClientId <$> dWalletAddress
          let dReq = zipDynWith (,) dClientId dCloudTokens
          -- logDyn "dClientId" dClientId
          eeCloudResponse <- cacheRequest dReq $ () <$ eFireCaching
          -- logEvent "walletTab: cache response:" eeCloudResponse
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

ipfsCacheKey :: Text
ipfsCacheKey = "encoins-aes-key"

isIpfsOn :: Text
isIpfsOn = "encoins-ipfs-switcher"

mkAesKey :: MonadWidget t m
  => Maybe PasswordRaw
  -> m (Event t Text)
mkAesKey mPass = do
  ev1 <- newEventWithDelay 0.1
  dLoadedKey <- loadAppDataId
    (getPassRaw <$> mPass) ipfsCacheKey "first-load-of-aes-key" ev1 id Nothing
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
          (getPassRaw <$> mPass)  ipfsCacheKey  "second-load-of-aes-key"  ev4 id ""
        pure eLoadedKey
    Just loadedKey -> pure $ loadedKey <$ ev3

fetchAesKey :: MonadWidget t m
  => Maybe PasswordRaw
  -> Event t ()
  -> m (Dynamic t (Maybe Text))
fetchAesKey mPass ev = loadAppDataId
    (getPassRaw <$> mPass) ipfsCacheKey "fetchAesKey-load-of-aes-key" ev id Nothing

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
            divClass "app-Ipfs_Trigger-NeedPassword" $ text "You want to password local cache before using IPFS"
            pure $ constDyn False
          Just _ -> do
            dIsIpfsOn <- ipfsTrigger mPass eOpen
            void $ switchHoldDyn dIsIpfsOn $ \case
              False -> pure never
              True -> do
                eKey <- mkAesKey mPass
                dKey <- holdDyn "Ipfs key not found" eKey
                showKeyWidget dKey
                pure never
            pure dIsIpfsOn

ipfsWindowStyle :: Text
ipfsWindowStyle = "width: min(90%, 950px); padding-left: min(5%, 70px); padding-right: min(5%, 70px); padding-top: min(5%, 30px); padding-bottom: min(5%, 30px);"

ipfsTrigger :: MonadWidget t m
  => Maybe PasswordRaw
  -> Event t ()
  -> m (Dynamic t Bool)
ipfsTrigger mPass eOpen = do
  dIsIpfsOn <- loadAppDataId (getPassRaw <$> mPass) isIpfsOn "ipfs-is-on" eOpen id False

  checkboxWidget (updated dIsIpfsOn) "app-Ipfs_Trigger" eOpen

checkboxWidget :: MonadWidget t m
  => Event t Bool
  -> Text
  -> Event t ()
  -> m (Dynamic t Bool)
checkboxWidget initial checkBoxClass eOpen = divClass "w-row" $ do
    inp <- inputElement $ def
      & initialAttributes .~
          ( "class" =: checkBoxClass )
      & inputElementConfig_initialChecked .~ False
      & inputElementConfig_setChecked .~ initial
    -- setFocusDelayOnEvent inp eOpen
    return $ _inputElement_checked inp

showKeyWidget :: MonadWidget t m
  => Dynamic t Text
  -> m ()
showKeyWidget dKey = do
  divClass "app-Ipfs_Key" $ dynText dKey
