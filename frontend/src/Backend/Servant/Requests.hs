module Backend.Servant.Requests where

import           Config.Config          (urlsBS)
import           Control.Monad          (void)
import           Control.Monad.IO.Class (MonadIO (..))
import           CSL                    (TransactionInputs)
import           Data.Aeson             (decode)
import           Data.ByteString.Lazy   (fromStrict)
import           Data.List              (delete)
import           Data.Maybe             (fromJust)
import           Data.Text              (Text)
import           Reflex.Dom             hiding (Value)
import           Servant.Reflex         (BaseUrl (..))
import           System.Random          (randomRIO)
import           Servant.API            (NoContent)

import           Backend.Protocol.Types
import           Backend.Servant.Client
import           Backend.Status         (Status (..), relayError)
import           ENCOINS.Common.Events  (logEvent, logDyn)
import           JS.App                 (pingServer)

newTxRequestWrapper :: MonadWidget t m
  => Dynamic t BaseUrl
  -> Dynamic t (InputOfEncoinsApi, TransactionInputs)
  -> Event t ()
  -> m (Event t (Text, Text), Event t Status)
newTxRequestWrapper dBaseUrl dReqBody e = do
  let ApiClient{..} = mkApiClient dBaseUrl
  eResp <- newTxRequest (Right <$> dReqBody) e
  let eRespUnwrapped = makeResponse <$> eResp
  logEvent "newTxRequestWrapper: eRespUnwrapped:" eRespUnwrapped
  return $ eventMaybe (BackendError relayError) eRespUnwrapped

submitTxRequestWrapper :: MonadWidget t m
  => Dynamic t BaseUrl
  -> Dynamic t SubmitTxReqBody
  -> Event t ()
  -> m (Event t (), Event t Status)
submitTxRequestWrapper dBaseUrl dReqBody e = do
  let ApiClient{..} = mkApiClient dBaseUrl
  eResp <- fmap (void . makeResponse) <$> submitTxRequest (Right <$> dReqBody) e
  return $ eventMaybe (BackendError relayError) eResp

pingRequestWrapper :: MonadWidget t m
  => Dynamic t BaseUrl
  -> Event t ()
  -> m (Event t (Maybe NoContent))
pingRequestWrapper dBaseUrl e = do
  let ApiClient{..} = mkApiClient dBaseUrl
  logDyn "Ping url" dBaseUrl
  ePingRes <- fmap makeResponse <$> pingRequest e
  logEvent "Ping response" ePingRes
  return ePingRes

serverTxRequestWrapper :: MonadWidget t m
  => Dynamic t BaseUrl
  -> Dynamic t (InputOfEncoinsApi, TransactionInputs)
  -> Event t ()
  -> m (Event t (), Event t Status)
serverTxRequestWrapper dBaseUrl dReqBody e = do
  let ApiClient{..} = mkApiClient dBaseUrl
  eResp <- serverTxRequest (Right <$> dReqBody) e
  let eRespUnwrapped = (() <$) . makeResponse <$> eResp
  logEvent "serverTxRequestWrapper: eRespUnwrapped:" eRespUnwrapped
  return $ eventMaybe (BackendError relayError) eRespUnwrapped

statusRequestWrapper :: MonadWidget t m
  => Dynamic t BaseUrl
  -> Dynamic t EncoinsStatusReqBody
  -> Event t ()
  -> m (Event t EncoinsStatusResult, Event t Status)
statusRequestWrapper dBaseUrl dReqBody e = do
  let ApiClient{..} = mkApiClient dBaseUrl
  eResp <- statusRequest (Right <$> dReqBody) e
  let eRespUnwrapped = makeResponse <$> eResp
  logEvent "statusRequestWrapper: eRespUnwrapped:" eRespUnwrapped
  return $ eventMaybe (BackendError relayError) eRespUnwrapped

urls :: [Text]
urls = fromJust $ decode $ fromStrict urlsBS

getRelayUrl :: MonadIO m => m (Maybe BaseUrl)
getRelayUrl = go urls
  where
    go [] = pure Nothing
    go l = do
      idx <- randomRIO (0, length l - 1)
      let url = l !! idx
      pingOk <- pingServer url
      if pingOk
        then return $ Just $ BasePath url
        else go (delete url l)