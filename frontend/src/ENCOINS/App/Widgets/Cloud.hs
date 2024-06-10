{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.Cloud where

import Backend.Protocol.Types
import Backend.Wallet(WalletName, toJS)
import Backend.Servant.Requests (restoreRequest, savePingRequest, saveRequest)
import Backend.Status
    ( AppStatus (..)
    , CloudIconStatus (..)
    , CloudRestoreStatus (..)
    )
import Backend.Utility (eventMaybeDynDef, switchHoldDyn, toText, unionWith, hashKeccak256)
import ENCOINS.App.Widgets.Basic
    ( elementResultJS
    , loadAppData
    , saveAppData
    , tellAppStatus
    )
import ENCOINS.Bulletproofs (Secret (..))
import ENCOINS.Common.Cache (aesKey)
import ENCOINS.Common.Events
import ENCOINS.Common.Utils (toJsonStrict)
import ENCOINS.Crypto.Field (Field (F))
import qualified JS.App as JS

import Control.Monad ((<=<))
import qualified Crypto.Hash as Hash
import Data.Aeson (decodeStrict)
import Data.Align (align)
import Data.Functor ((<&>))
import Data.List (find)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.These (These)
import Reflex.Dom

handleUnsavedTokens ::
    (MonadWidget t m, EventWriter t [AppStatus] m) =>
    Dynamic t Bool
    -> Dynamic t (Maybe AesKeyRaw)
    -> Dynamic t [TokenCacheV3]
    -> Event t ()
    -> m (Dynamic t [TokenCacheV3])
handleUnsavedTokens dCloudOn dmKey dTokenCache eWasMigration = mdo
    dTokenCacheUniq <- holdUniqDyn dTokenCache

    -- create save trigger when cache contains un-saved tokens
    let eTokenCacheUniqFired =
            attachPromptlyDynWithMaybe
                ( \updatedTokens tokensInCache -> updatedTokens <$ selectUnsavedTokens tokensInCache
                )
                dTokenCacheUpdated
                (updated dTokenCacheUniq)

    -- or create trigger of saving when migration was occurred
    let eTokenCacheMigrated = tagPromptlyDyn dTokenCacheUpdated eWasMigration

    let eSaveTrigger = leftmost [eTokenCacheMigrated, eTokenCacheUniqFired]
    dTokenCacheUniqFired <- holdDyn [] eSaveTrigger

    eTokenWithNewState <- saveTokens dCloudOn dmKey dTokenCacheUniqFired
    dTokenSaveSynched <- foldDyn (unionWith tcAssetName) [] eTokenWithNewState
    -- Update statuses of dTokenCache before saving on server first one.
    let dTokenCacheUpdated =
            zipDynWith
                syncOldWithUpdatedTokens
                dTokenCacheUniq
                dTokenSaveSynched
    let eUnsavedTokens = updated $ selectUnsavedTokens <$> dTokenCacheUpdated
    tellAppStatus $
        leftmost
            [ CloudIcon AllSaved <$ ffilter null eUnsavedTokens
            , CloudIcon FailedSave <$ ffilter (not . null) eUnsavedTokens
            ]
    pure dTokenCacheUpdated

-- Saving launches
-- when there are unsaved tokens in cache appear
saveTokens ::
    (MonadWidget t m, EventWriter t [AppStatus] m) =>
    Dynamic t Bool
    -> Dynamic t (Maybe AesKeyRaw)
    -> Dynamic t [TokenCacheV3]
    -> m (Event t [TokenCacheV3])
saveTokens dCloudOn dmKey dTokens = mdo
    let dSaveConditions = zipDynWith (,) dCloudOn dmKey
    -- Select unpinned tokens in cache
    let dmUnpinnedTokens = selectUnsavedTokens <$> dTokens
    -- Combine all condition to the tuple
    let dFullConditions = combineConditions dSaveConditions dmUnpinnedTokens

    (eSaveError, eTokenSaveAttemptOne) <-
        fanThese <$> saveEncryptedTokens dFullConditions
    logEvent "save error" eSaveError

    -- BEGIN
    -- retry to pin tokens extra 5 times
    (eAttemptExcess, eTokenPinComplete) <-
        fanThese
            <$> retrySaveEncryptedTokens dSaveConditions eTokenSaveAttemptOne
    -- END
    logEvent "excess of save attempts" eAttemptExcess

    -- The results of the first pinning is saved anyway
    let eTokensToSave = leftmost [eTokenSaveAttemptOne, eTokenPinComplete]

    pure eTokensToSave

switchHoldDynWriter ::
    (MonadWidget t m, EventWriter t [AppStatus] m) =>
    Dynamic t a
    -> (a -> m (Event t b))
    -> m (Event t b)
switchHoldDynWriter da f = switchHold never <=< dyn $ da <&> f

saveEncryptedTokens ::
    (MonadWidget t m, EventWriter t [AppStatus] m) =>
    Dynamic t (Bool, Maybe AesKeyRaw, Maybe (NonEmpty TokenCacheV3))
    -> m (Event t (These () [TokenCacheV3]))
saveEncryptedTokens dConditions = do
    switchHoldDynWriter dConditions $ \case
        (True, Just key, Just (NE.toList -> tokenCache)) -> do
            dCloudTokens <- encryptTokens key tokenCache
            let eFireCaching = ffilter (not . null) $ updated dCloudTokens
            tellAppStatus $ CloudIcon Saving <$ eFireCaching
            eePing <- savePingRequest $ () <$ eFireCaching
            let (ePingError, ePingResponse) = fanEither eePing

            eeStatusResponse <- saveRequest dCloudTokens $ () <$ ePingResponse
            let (eCacheError, eStatusResponse) = fanEither eeStatusResponse

            dStatusResponse <- holdDyn Map.empty eStatusResponse
            let eUpdatedTokens = updated $ updateCacheStatus tokenCache <$> dStatusResponse
            let eSaveError = () <$ leftmost [ePingError, eCacheError]
            pure $ align eSaveError eUpdatedTokens
        _ -> pure never

-- if some Tokens are unpinned try to pin them 5 times with 10 seconds delay.
retrySaveEncryptedTokens ::
    (MonadWidget t m, EventWriter t [AppStatus] m) =>
    Dynamic t (Bool, Maybe AesKeyRaw)
    -> Event t [TokenCacheV3]
    -> m (Event t (These () [TokenCacheV3]))
retrySaveEncryptedTokens dSaveConditions eTokenSaveAttempt = mdo
    let eTokenUnsaved = fmapMaybe id $ selectUnsavedTokens <$> eTokenSaveAttempt
    dmValidTokens2 <- holdDyn Nothing $ Just <$> eValidTokens3
    let dFullConditions = combineConditions dSaveConditions dmValidTokens2
    (eSaveError, eTokenSaveAttemptNext) <-
        fanThese <$> saveEncryptedTokens dFullConditions
    dTokenSaveAttemptNext <- holdDyn [] eTokenSaveAttemptNext
    eDelayTokens <-
        delay 10 $
            leftmost [Just <$> eTokenUnsaved, selectUnsavedTokens <$> eTokenSaveAttemptNext]
    let (eTokenCachedComplete, eTrySaveAgain) =
            eventMaybeDynDef dTokenSaveAttemptNext eDelayTokens
    dAttemptCounter :: Dynamic t Int <- count eTrySaveAgain
    let (eAttemptExcess, eValidTokens3) =
            fanEither $
                attachPromptlyDynWith
                    (\n ts -> if n > 5 then Left () else Right ts)
                    dAttemptCounter
                    eTrySaveAgain
    let eError = leftmost [eSaveError, eAttemptExcess]
    pure $ align eError eTokenCachedComplete

combineConditions ::
    (Reflex t) =>
    Dynamic t (Bool, Maybe AesKeyRaw)
    -> Dynamic t (Maybe (NonEmpty TokenCacheV3))
    -> Dynamic t (Bool, Maybe AesKeyRaw, Maybe (NonEmpty TokenCacheV3))
combineConditions = zipDynWith (\(isOn, mKey) ts -> (isOn, mKey, ts))

-- Encrypt valid tokens only and make request
encryptTokens ::
    (MonadWidget t m) =>
    AesKeyRaw
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
    dRes <-
        foldDynMaybe
            (\(ac, ar) _ -> if length ar == ac then Just (ac, ar) else Nothing)
            (0, [])
            $ attachPromptlyDyn (constDyn validTokenNumber)
            $ catMaybes <$> emClouds
    pure $ snd <$> dRes

encryptToken ::
    (MonadWidget t m) =>
    AesKeyRaw
    -> Event t ()
    -> Secret
    -> m (Dynamic t (Maybe EncryptedSecret))
encryptToken (MkAesKeyRaw keyText) ev secret = do
    let secretByte = toJsonStrict secret
    let elementId = toText $ Hash.hash @Hash.SHA1 secretByte
    performEvent_ $
        JS.encryptSecret (keyText, elementId, decodeUtf8 secretByte) <$ ev
    dEncryptedSecretT <- elementResultJS elementId id
    let checkEmpty t = if T.null t then Nothing else Just t
    pure $ fmap MkEncryptedSecret . checkEmpty <$> dEncryptedSecretT

-- Update save status of tokens with response from save server
updateCacheStatus ::
    [TokenCacheV3]
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
                        t
                            { tcSaveStatus = saveStatus
                            }

-- Select valid tokens for saving
-- Just created tokens with 'SaveUndefined' status are selected for saving
-- Minted tokens that have 'SaveUndefined' or 'SaveError' statuses are selected for saving again
-- Tokens with other statuses are not selected for saving
selectTokenToSave :: TokenCacheV3 -> Maybe TokenCacheV3
selectTokenToSave t = case tcSaveStatus t of
    (SaveUndefined) -> Just t
    (SaveError) -> Just t
    _ -> Nothing

selectUnsavedTokens :: [TokenCacheV3] -> Maybe (NonEmpty TokenCacheV3)
selectUnsavedTokens = NE.nonEmpty . catMaybes . map selectTokenToSave

mkSaveRequest :: TokenCacheV3 -> EncryptedSecret -> SaveRequest
mkSaveRequest cache encryptedSecret =
    MkSaveRequest
        { srAssetName = tcAssetName cache
        , srSecretKey = encryptedSecret
        }

-- Generate aes 256 length key with function builtin any browser
-- And save it to local cache
genAesKey ::
    (MonadWidget t m) =>
    Maybe PasswordRaw
    -> Dynamic t (Maybe AesKeyRaw)
    -> Event t ()
    -> m (Event t ())
genAesKey mPass dmCachedKey ev1 = do
    ev2 <- delay 0.1 ev1
    eNewKeySaved <- switchHoldDyn dmCachedKey $ \case
        Nothing -> do
            let genElId = "genAESKeyId"
            performEvent_ $ JS.generateCloudKey genElId <$ ev2
            eAesKeyText <- updated <$> elementResultJS genElId id
            let eAesKey = MkAesKeyRaw <$> eAesKeyText
            eKeySaved <- saveAppData mPass aesKey eAesKey
            pure eKeySaved
        Just _ -> pure never
    pure eNewKeySaved

-- Make cloud key basing on wallet signature.
-- Then save it to local cache
makeSignedKey ::
    (MonadWidget t m) =>
    Maybe PasswordRaw
    -> Dynamic t WalletName
    -> Event t ()
    -> m (Event t ())
makeSignedKey mPass dWalletName ev = do
    let getKeyElId = "getKeyFromSign2"
    ev2 <- delay 0.1 ev
    performEvent_ $ JS.getSignedKey getKeyElId <$> tagPromptlyDyn (toJS <$> dWalletName) ev2
    eSignedKey <- updated <$> elementResultJS
      getKeyElId
      id
    let eHashedSign = hashKeccak256 <$> eSignedKey
    let eAesKey = MkAesKeyRaw <$> eHashedSign
    eKeySaved <- saveAppData mPass aesKey eAesKey
    pure eKeySaved

fetchAesKey ::
    (MonadWidget t m) =>
    Maybe PasswordRaw
    -> Text
    -> Event t ()
    -> m (Dynamic t (Maybe AesKeyRaw))
fetchAesKey mPass resId ev = loadAppData mPass aesKey resId ev id Nothing

resetTokens ::
    (Reflex t) =>
    Dynamic t (Maybe [TokenCacheV3])
    -> Dynamic t Bool
    -> Dynamic t (Maybe [TokenCacheV3])
resetTokens dmTokens dKeyWasReset =
    let resetToken t = t{tcSaveStatus = SaveUndefined}
     in zipDynWith
            (\mTokens wasReset -> if wasReset then map resetToken <$> mTokens else mTokens)
            dmTokens
            dKeyWasReset

-- With saving wallet and ledger modes have two dynamics for token
-- and save each of them separately
-- This function synchronize them.
-- Otherwise dynamic with old tokens rewrite new one.
-- O(1) when new token list is empty,
-- O(n*m) otherwise
syncOldWithUpdatedTokens :: [TokenCacheV3] -> [TokenCacheV3] -> [TokenCacheV3]
syncOldWithUpdatedTokens old [] = old
syncOldWithUpdatedTokens old new = map (update new) old
    where
        update ns o = case find (\n -> tcAssetName o == tcAssetName n) ns of
            Nothing -> o
            Just n' -> n'

-- RESTORE

-- restore tokens saved on cloud server
restoreValidTokens ::
    (MonadWidget t m, EventWriter t [AppStatus] m) =>
    Dynamic t (Maybe AesKeyRaw)
    -> Event t ()
    -> m (Dynamic t [TokenCacheV3])
restoreValidTokens dmKey eRestore = do
    eTokens <- switchHoldDyn dmKey $ \case
        Nothing -> pure never
        Just key -> do
            eeResotres <- restoreRequest eRestore
            let (eRestoreError, eRestoreResponses) = fanEither eeResotres
            dRestoreResponses <- holdDyn [] eRestoreResponses
            eRes <- updated <$> decryptTokens key dRestoreResponses
            let restoredNumber = fromMaybe 0 . fmap length
            tellAppStatus $
                leftmost
                    [ CloudRestore RestoreFail <$ eRestoreError
                    , CloudRestore . RestoreSuccess . restoredNumber <$> eRes
                    ]
            pure eRes
    holdDyn [] $ fromMaybe [] <$> eTokens

-- Return the tokens that the aes key is able to decrypt
decryptTokens ::
    (MonadWidget t m) =>
    AesKeyRaw
    -> Dynamic t [(Text, Text)]
    -> m (Dynamic t (Maybe [TokenCacheV3]))
decryptTokens key dRestores = do
    let dValidLength = length <$> dRestores
    logDyn "decryptTokens: number of received tokens" dValidLength
    emlmTokens <- switchHoldDyn dRestores $ \case
        [] -> pure never
        restores -> do
            ev <- newEventWithDelay 1
            let resId = "decryptTokens-resId"
            performEvent_ $ JS.decryptSecretList (getAesKeyRaw key, resId, restores) <$ ev
            let f =
                    fmap
                        ( mapMaybe
                            (\(n, s) -> mkTokenCacheV3 n =<< (decodeStrict $ encodeUtf8 s :: Maybe Secret))
                        )
                        . (decodeStrict . encodeUtf8 :: Text -> Maybe [(AssetName, Text)])
            dmDecryptedTokens <- elementResultJS resId f
            pure $ updated dmDecryptedTokens

    dRes <- holdDyn Nothing emlmTokens
    pure dRes

mkTokenCacheV3 :: AssetName -> Secret -> Maybe TokenCacheV3
mkTokenCacheV3 name secret =
    if F 0 <= v && v < F (2 ^ (20 :: Int))
        then
            Just $
                MkTokenCacheV3
                    { tcAssetName = name
                    , tcSecret = secret
                    , tcSaveStatus = Saved
                    }
        else Nothing
    where
        v = secretV secret
