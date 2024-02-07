{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.IPFS where

import           Backend.Protocol.Types
import           Backend.Servant.Requests        (cacheRequest, restoreRequest)
import           Backend.Utility                 (eventEither, eventTuple,
                                                  switchHoldDyn)
import           ENCOINS.App.Widgets.Basic       (elementResultJS,
                                                  loadAppDataId, saveAppDataId,
                                                  saveAppDataId_)
import           ENCOINS.Bulletproofs            (Secret (..))
import           ENCOINS.Common.Cache            (encoinsV3, ipfsCacheKey,
                                                  isIpfsOn)
import           ENCOINS.Common.Events
import           ENCOINS.Common.Utils            (toJsonStrict, toText)
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

saveTokensOnIpfs :: MonadWidget t m
  => Dynamic t Text
  -> Maybe PasswordRaw
  -> Event t [TokenCacheV3]
  -> m (Event t [TokenCacheV3])
saveTokensOnIpfs dWalletAddress mPass eTokenCache = do
  dmKey <- fetchAesKey mPass $ () <$ eTokenCache
  dIpfsOn <- fetchIPFSKey $ () <$ eTokenCache
  eTokenCacheDelayed <- delay 0.2 eTokenCache
  logEvent "saveTokensOnIpfs: tokens before ipfs save" $ showTokens <$> eTokenCacheDelayed
  eTokenIpfsCached <- pinEncryptedTokens
    dWalletAddress
    dmKey
    dIpfsOn
    eTokenCacheDelayed
  logEvent "saveTokensOnIpfs: tokens after ipfs save" $ showTokens <$> eTokenIpfsCached
  eSaved <- saveAppDataId mPass encoinsV3 eTokenIpfsCached
  logEvent "saveTokensOnIpfs: eSaved 2" eSaved
  dTokenIpfsCached <- holdDyn [] eTokenIpfsCached
  pure $ tagPromptlyDyn dTokenIpfsCached eSaved

pinEncryptedTokens :: MonadWidget t m
  => Dynamic t Text
  -> Dynamic t (Maybe Text)
  -> Dynamic t Bool
  -> Event t [TokenCacheV3]
  -> m (Event t [TokenCacheV3])
pinEncryptedTokens dWalletAddress dmKey dIpfsOn eTokenCache = do
  switchHoldDyn dIpfsOn $ \case
    False -> do
      pure never
    True -> do
      switchHoldDyn dmKey $ \case
        Nothing -> do
          pure never
        Just key -> do
          dTokenCache <- holdDyn [] eTokenCache
          switchHoldDyn dTokenCache $ \case
            [] -> do
              pure never
            tokenCache -> do
              dCloudTokens <- encryptTokens key tokenCache
              logDyn "pinEncryptedTokens: dCloudTokens" $ map reqAssetName <$> dCloudTokens
              let eFireCaching = ffilter (not . null) $ updated dCloudTokens
              let dClientId = mkClientId <$> dWalletAddress
              let dReq = zipDynWith (,) dClientId dCloudTokens
              eeCloudResponse <- cacheRequest dReq $ () <$ eFireCaching
              let (eCacheError, eCloudResponse) = eventEither eeCloudResponse
              dCloudResponse <- holdDyn Map.empty eCloudResponse
              pure $ updated $ ffor2 dCloudResponse dTokenCache updateCacheStatus

-- Encrypt valid tokens only and make request
encryptTokens :: MonadWidget t m
  => Text
  -> [TokenCacheV3]
  -> m (Dynamic t [CloudRequest])
encryptTokens key tokens = do
  let validTokens = catMaybes $ map getValidTokens tokens
  let validTokenNumber = length validTokens
  eClouds <- fmap (updated . sequence) $ flip traverse validTokens $ \t -> do
      ev <- newEvent
      dEncryptedSecret <- encryptToken key ev $ tcSecret t
      let dmCloud = mkCloudRequest t <$> dEncryptedSecret
      pure dmCloud
    -- Wait until all tokens are encrypted and return them
  dRes <- foldDynMaybe
    (\(ac,ar) _ -> if length ar == ac then Just (ac,ar) else Nothing) (0,[]) $
    attachPromptlyDyn (constDyn validTokenNumber) $ catMaybes <$> eClouds
  pure $ snd <$> dRes

encryptToken :: MonadWidget t m
  => Text
  -> Event t ()
  -> Secret
  -> m (Dynamic t Text)
encryptToken key ev secret = do
  let secretByte = toJsonStrict secret
  let elementId = toText $ Hash.hash @Hash.SHA256 secretByte
  performEvent_ $ JS.encryptAES (key, elementId, decodeUtf8 secretByte) <$ ev
  dEncryptedToken <- elementResultJS elementId id
  pure dEncryptedToken

-- Update ipfs metadata of tokens with response from ipfs
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

-- Valid tokens are ones that Minted and not Pinned
getValidTokens :: TokenCacheV3 -> Maybe TokenCacheV3
getValidTokens t = case (tcIpfsStatus t, tcCoinStatus t)  of
  (Pinned, _) -> Nothing
  (_, Minted) -> Just t
  _           -> Nothing

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

-- Generate aes 256 length key with function builtin any browser
-- And save it to local cache
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
        ev4 <- saveAppDataId mPass ipfsCacheKey eAesKey
        eLoadedKey <- updated <$> loadAppDataId
            mPass ipfsCacheKey "second-load-of-aes-key"  ev4 id ""
        pure eLoadedKey
    Just loadedKey -> pure $ loadedKey <$ ev3

fetchAesKey :: MonadWidget t m
  => Maybe PasswordRaw
  -> Event t ()
  -> m (Dynamic t (Maybe Text))
fetchAesKey mPass ev = loadAppDataId
    mPass ipfsCacheKey "fetchAesKey-load-of-aes-key" ev id Nothing

fetchIPFSKey :: MonadWidget t m
  => Event t ()
  -> m (Dynamic t Bool)
fetchIPFSKey ev = loadAppDataId
    Nothing isIpfsOn "fetchIPFSKey-is-ipfs-on-key" ev id False

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
  -- Wait until all tokens are decrypted and return them
  dRes <- foldDynMaybe
    (\(ac,ar) _ -> if length ar == ac then Just (ac,ar) else Nothing) (0,[]) $
    attachPromptlyDyn dValidLength eResponses
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
    "Save encoins on IPFS" $ do
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
  saveAppDataId_ Nothing isIpfsOn $ updated dIsChecked
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

-- With ipfs wallet and ledger modes have two dynamics for token and save each of them separately
-- This function synchronize them.
-- Otherwise dynamic with old tokens rewrite new one.
-- TODO: consider use MVar or TWar to manage state.
updateTokenState :: [TokenCacheV3] -> [TokenCacheV3] -> [TokenCacheV3]
updateTokenState old new = foldl' (update new) [] old
  where
    update ns acc o =
      case find (\n -> tcAssetName o == tcAssetName n) ns of
        Nothing -> o : acc
        Just n' -> n' : acc
