{-# LANGUAGE RecursiveDo #-}

module Backend.Servant.Requests where

import           Control.Lens           (view)
import           Control.Monad          (forM)
import           Control.Monad.IO.Class (MonadIO (..))
import           CSL                    (TransactionInputs)
import           Data.Aeson             (Value)
import           Data.Array.IO
import           Data.Functor           (($>))
import           Data.List              (delete)
import           Data.Map               (Map)
import           Data.Maybe             (isNothing)
import           Data.Text              (Text)
import           Reflex.Dom             hiding (Request, Value)
import           Servant.Reflex         (BaseUrl (..), ReqResult (..), QParam(..))
import           System.Random          (randomRIO)
import           Witherable             (catMaybes)

import           Backend.Protocol.Types
import           Backend.Servant.Client
import           Backend.Utility        (normalizeCurrentUrl, normalizePingUrl)
import           Config.Config          (jwtToken)
import           ENCOINS.Common.Events
import           JS.App                 (pingServer)

newTxRequestWrapper :: MonadWidget t m
  => BaseUrl
  -> Dynamic t (InputOfEncoinsApi, TransactionInputs)
  -> Event t ()
  -> m (Event t (Either Text (Text, Text)))
newTxRequestWrapper baseUrl dReqBody e = do
  let ApiClient{..} = mkApiClient baseUrl
  eResp <- newTxRequest (Right <$> dReqBody) e
  let eRespUnwrapped = mkTextOrResponse <$> eResp
  logEvent "newTx request" $ () <$ eRespUnwrapped
  pure eRespUnwrapped

submitTxRequestWrapper :: MonadWidget t m
  => BaseUrl
  -> Dynamic t SubmitTxReqBody
  -> Event t ()
  -> m (Event t (Either Text ()))
submitTxRequestWrapper baseUrl dReqBody e = do
  let ApiClient{..} = mkApiClient baseUrl
  eResp <- submitTxRequest (Right <$> dReqBody) e
  let eRespUnwrapped = (() <$) . mkTextOrResponse <$> eResp
  logEvent "submitTx request" eRespUnwrapped
  return eRespUnwrapped

pingRequestWrapper :: MonadWidget t m
  => BaseUrl
  -> Event t ()
  -> m (Event t (Maybe BaseUrl))
pingRequestWrapper baseUrl e = do
  let ApiClient{..} = mkApiClient baseUrl
  delayed <- delay 0.2 e
  ePingRes <- fmap makeResponse <$> pingRequest delayed
  logEvent "pingRequestWrapper: ping response" ePingRes
  let eRes = (\p -> baseUrl <$ p) <$> ePingRes
  pure eRes

serverTxRequestWrapper :: MonadWidget t m
  => BaseUrl
  -> Dynamic t (InputOfEncoinsApi, TransactionInputs)
  -> Event t ()
  -> m (Event t (Either Text ()))
serverTxRequestWrapper baseUrl dReqBody e = do
  let ApiClient{..} = mkApiClient baseUrl
  eResp <- serverTxRequest (Right <$> dReqBody) e
  let eRespUnwrapped = (() <$) . mkTextOrResponse <$> eResp
  logEvent "serverTx request" eRespUnwrapped
  return eRespUnwrapped

statusRequestWrapper :: MonadWidget t m
  => BaseUrl
  -> Dynamic t EncoinsStatusReqBody
  -> Event t ()
  -> m (Event t (Either Text EncoinsStatusResult))
statusRequestWrapper baseUrl dReqBody e = do
  let ApiClient{..} = mkApiClient baseUrl
  eResp <- statusRequest (Right <$> dReqBody) e
  let eRespUnwrapped = mkTextOrResponse <$> eResp
  logEvent "status request" $ fmap showStatus <$> eRespUnwrapped
  return eRespUnwrapped

versionRequestWrapper :: MonadWidget t m
  => BaseUrl
  -> Event t ()
  -> m (Event t (Maybe ServerVersion))
versionRequestWrapper baseUrl e = do
  let ApiClient{..} = mkApiClient baseUrl
  eVersionRes <- fmap makeResponse <$> versionRequest e
  logEvent "Version" eVersionRes
  return eVersionRes

-- Fetch servers that are selected for delegation
serversRequestWrapper :: MonadWidget t m
  => BaseUrl
  -> Event t ()
  -> m (Event t (Either Text (Map Text Integer)))
serversRequestWrapper baseUrl e = do
  let ApiClient{..} = mkApiClient baseUrl
  eResp <- serversRequest e
  let eRespUnwrapped = mkTextOrResponse <$> eResp
  logEvent "servers request" eRespUnwrapped
  return eRespUnwrapped

-- Fetch available servers for app
currentRequestWrapper :: MonadWidget t m
  => BaseUrl
  -> Event t ()
  -> m (Event t (Either Text [Text]))
currentRequestWrapper baseUrl e = do
  let ApiClient{..} = mkApiClient baseUrl
  eResp <- currentRequest e
  let eRespUnwrapped = mkTextOrResponse <$> eResp
  logEvent "current servers request" eRespUnwrapped
  return $ fmap (fmap normalizeCurrentUrl) <$> eRespUnwrapped

-- Fetch how much encoins and which relay the particular wallet delegated.
infoRequestWrapper :: MonadWidget t m
  => BaseUrl
  -> Dynamic t Address
  -> Event t ()
  -> m (Event t (Either Text (Text, Integer)))
infoRequestWrapper baseUrl addr e = do
  let ApiClient{..} = mkApiClient baseUrl
  eResp <- infoRequest (Right <$> addr) e
  let eRespUnwrapped = mkTextOrResponse <$> eResp
  logEvent "info request" eRespUnwrapped
  return eRespUnwrapped

---------------------------------------------- Utilities ----------------------------------------

getRelayUrlE :: MonadWidget t m
  => Dynamic t [Text]
  -> Event t ()
  -> m (Event t (Maybe Text))
getRelayUrlE dUrls ev = do
  let eUrls = tagPromptlyDyn dUrls ev
  performEvent $ go <$> eUrls
  where
    go [] = pure Nothing
    go l = do
      idx <- randomRIO (0, length l - 1)
      let url = l !! idx
      pingOk <- pingServer $ normalizePingUrl url
      if pingOk
        then return $ Just url
        else go (delete url l)

makeResponse :: ReqResult tag a -> Maybe a
makeResponse (ResponseSuccess _ a _) = Just a
makeResponse _                       = Nothing

mkTextOrResponse :: ReqResult tag a -> Either Text a
mkTextOrResponse (ResponseSuccess _ a _)   = Right a
mkTextOrResponse (ResponseFailure _ txt _) = Left $ "ResponseFailure: " <> txt
mkTextOrResponse (RequestFailure _ txt)    = Left $ "RequestFailure: " <> txt

mkStatusOrResponse :: ReqResult tag a -> Either Int a
mkStatusOrResponse (ResponseSuccess _ a _) = Right a
mkStatusOrResponse (ResponseFailure _ _ xhr) = Left $ fromIntegral $ view xhrResponse_status xhr
mkStatusOrResponse (RequestFailure _ _) = Left $ -1

eventMaybe :: Reflex t => e -> Event t (Maybe a) -> (Event t e, Event t a)
eventMaybe errValue ev = (errValue <$ ffilter isNothing ev, catMaybes ev)

eventEither :: Reflex t => Event t (Either e a) -> (Event t e, Event t a)
eventEither ev = (filterLeft ev, filterRight ev)

hasStatusZero :: Int -> Either Int Int
hasStatusZero = \case
  0 -> Left 0
  n -> Right n

fromRelayResponse :: Reflex t
  => Event t (Either Int a)
  -> (Event t Int, Event t Int, Event t a) -- Relay disconnected, failed or responded
fromRelayResponse eeResp =
  let (eStatus, eRespOk) = eventEither eeResp
      eeStatus = hasStatusZero <$> eStatus
      (eRelayDown, eRelayError) = (filterLeft eeStatus, filterRight eeStatus)
  in (eRelayDown, eRelayError, eRespOk)

-- | Randomly shuffle a list on event fires
--   /O(N)/
shuffle :: MonadWidget t m => Event t () -> [a] -> m (Event t [a])
shuffle ev xs = performEvent $ ev $> (liftIO $ do
  ar <- newArr xsLength xs
  forM [1..xsLength] $ \i -> do
      j <- randomRIO (i,xsLength)
      vi <- readArray ar i
      vj <- readArray ar j
      writeArray ar j vi
      return vj)
  where
    xsLength = length xs
    newArr :: Int -> [a] -> IO (IOArray Int a)
    newArr n ls =  newListArray (1,n) ls

-- IPFS

-- Used only to fetch value (files) itself through dedicated gateway
fetchUrl :: BaseUrl
fetchUrl = BasePath "https://coral-holy-gibbon-767.mypinata.cloud"

-- Used for everything
pinUrl :: BaseUrl
pinUrl = BasePath "https://api.pinata.cloud"

pinJsonWrapper :: MonadWidget t m
  => Dynamic t Person
  -> Event t ()
  -> m (Event t (Either Text Value))
pinJsonWrapper dReqBody e = do
  let MkIpfsApiClient{..} = mkIpfsApiClient pinUrl $ Just jwtToken
  eResp <- pinJson (Right <$> dReqBody) e
  let eRespUnwrapped = mkTextOrResponse <$> eResp
  logEvent "pinJson request" eRespUnwrapped
  pure eRespUnwrapped

fetchByCipWrapper :: MonadWidget t m
  => Dynamic t (Either Text Text)
  -> Event t ()
  -> m (Event t (Either Text Value))
fetchByCipWrapper dCip e = do
  let MkIpfsApiClient{..} = mkIpfsApiClient fetchUrl Nothing
  eResp <- fetchByCip dCip e
  let eRespUnwrapped = mkTextOrResponse <$> eResp
  logEvent "fetchByCip request" eRespUnwrapped
  pure eRespUnwrapped

fetchMetaAllWrapper :: MonadWidget t m
  => Event t ()
  -> m (Event t (Either Text Value))
fetchMetaAllWrapper e = do
  let MkIpfsApiClient{..} = mkIpfsApiClient pinUrl $ Just jwtToken
  eResp <- fetchMetaAll e
  let eRespUnwrapped = mkTextOrResponse <$> eResp
  logEvent "fetchMetaAll request" eRespUnwrapped
  pure eRespUnwrapped

unpinByCipWrapper :: MonadWidget t m
  => Dynamic t (Either Text Text)
  -> Event t ()
  -> m (Event t (Either Text Text))
unpinByCipWrapper dCip e = do
  let MkIpfsApiClient{..} = mkIpfsApiClient pinUrl $ Just jwtToken
  eResp <- unpinByCip dCip e
  let eRespUnwrapped = mkTextOrResponse <$> eResp
  logEvent "unpinByCip request" eRespUnwrapped
  pure eRespUnwrapped

fetchMetaPinnedWrapper :: MonadWidget t m
  => Event t ()
  -> m (Event t (Either Text Value))
fetchMetaPinnedWrapper e = do
  let MkIpfsApiClient{..} = mkIpfsApiClient pinUrl $ Just jwtToken
  eResp <- fetchMetaPinned (pure $ QParamSome "pinned") e
  let eRespUnwrapped = mkTextOrResponse <$> eResp
  logEvent "fetchMetaPinned request" eRespUnwrapped
  pure eRespUnwrapped

fetchMetaPinnedIdWrapper :: MonadWidget t m
  => Dynamic t Text
  -> Event t ()
  -> m (Event t (Either Text Value))
fetchMetaPinnedIdWrapper dName e = do
  let MkIpfsApiClient{..} = mkIpfsApiClient pinUrl $ Just jwtToken
  eResp <- fetchMetaPinnedId
    (pure $ QParamSome "pinned")
    (QParamSome <$> dName)
    e
  let eRespUnwrapped = mkTextOrResponse <$> eResp
  logEvent "fetchMetaPinnedId request" eRespUnwrapped
  pure eRespUnwrapped
