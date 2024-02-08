{-# LANGUAGE RecursiveDo #-}

module Backend.Servant.Requests where

import           Backend.Protocol.Types
import           Backend.Servant.Client
import           Backend.Utility        (eventEither, normalizeCurrentUrl,
                                         normalizePingUrl)
import           ENCOINS.Common.Events
import           JS.App                 (pingServer)

import           Control.Lens           (view)
import           Control.Monad          (forM)
import           Control.Monad.IO.Class (MonadIO (..))
import           CSL                    (TransactionInputs)
import           Data.Array.IO
import           Data.Functor           (($>))
import           Data.List              (delete)
import           Data.Map               (Map)
import           Data.Text              (Text)
import           Reflex.Dom             hiding (Request, Value)
import           Servant.Reflex         (BaseUrl (..), ReqResult (..))
import           System.Random          (randomRIO)


newTxRequestWrapper :: MonadWidget t m
  => BaseUrl
  -> Dynamic t (InputOfEncoinsApi, TransactionInputs)
  -> Event t ()
  -> m (Event t (Either Text (Text, Text)))
newTxRequestWrapper baseUrl dReqBody e = do
  let ApiClient{..} = mkApiClient baseUrl
  eResp <- newTxRequest (Right <$> dReqBody) e
  let eRespUnwrapped = mkTextOrResponse <$> eResp
  logEvent "newTx response" $ () <$ eRespUnwrapped
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
  logEvent "submitTx response" eRespUnwrapped
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
  logEvent "serverTx response" eRespUnwrapped
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
  logEvent "status response" $ fmap showStatus <$> eRespUnwrapped
  return eRespUnwrapped

versionRequestWrapper :: MonadWidget t m
  => BaseUrl
  -> Event t ()
  -> m (Event t (Maybe ServerVersion))
versionRequestWrapper baseUrl e = do
  let ApiClient{..} = mkApiClient baseUrl
  eVersionRes <- fmap makeResponse <$> versionRequest e
  logEvent "version response" eVersionRes
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
  logEvent "servers response" eRespUnwrapped
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
  logEvent "current response" eRespUnwrapped
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
  logEvent "info response" eRespUnwrapped
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

-- Ipfs request to backend

upfsBackendUrl :: BaseUrl
upfsBackendUrl = BasePath "http://localhost:7000"

ipfsCacheRequest :: MonadWidget t m
  => Dynamic t (Text, [CloudRequest])
  -> Event t ()
  -> m (Event t (Either Text (Map Text CloudResponse)))
ipfsCacheRequest dToken e = do
  let MkBackIpfsApiClient{..} = mkBackIpfsApiClient upfsBackendUrl
  eResp <- cache (Right <$> dToken) e
  let eRespUnwrapped = mkTextOrResponse <$> eResp
  logEvent "ipfsCache response" eRespUnwrapped
  pure eRespUnwrapped

restoreRequest :: MonadWidget t m
  => Dynamic t Text
  -> Event t ()
  -> m (Event t (Either Text [RestoreResponse]))
restoreRequest dClientId e = do
  let MkBackIpfsApiClient{..} = mkBackIpfsApiClient upfsBackendUrl
  eResp <- restore (Right <$> dClientId) e
  let eRespUnwrapped = mkTextOrResponse <$> eResp
  logEvent "restore response" eRespUnwrapped
  pure eRespUnwrapped
