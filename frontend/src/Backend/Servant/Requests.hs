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
import           Servant.API            (NoContent)
import           Servant.Reflex         (BaseUrl (..))
import           System.Random          (randomRIO)
import qualified Data.IntMap            as IMap
import           Data.IntMap            (IntMap)

import           Backend.Protocol.Types
import           Backend.Servant.Client
import           Backend.Status         (Status (..), relayError)
import           ENCOINS.Common.Events  (logEvent)
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
  return $ eventOrError (BackendError relayError) eRespUnwrapped

submitTxRequestWrapper :: MonadWidget t m
  => Dynamic t BaseUrl
  -> Dynamic t SubmitTxReqBody
  -> Event t ()
  -> m (Event t (), Event t Status)
submitTxRequestWrapper dBaseUrl dReqBody e = do
  let ApiClient{..} = mkApiClient dBaseUrl
  eResp <- fmap (void . makeResponse) <$> submitTxRequest (Right <$> dReqBody) e
  return $ eventOrError (BackendError relayError) eResp

pingRequestWrapper :: MonadWidget t m
  => Dynamic t BaseUrl
  -> Event t ()
  -> m (Event t NoContent, Event t Status)
pingRequestWrapper dBaseUrl e = do
  let ApiClient{..} = mkApiClient dBaseUrl
  delayed <- delay 0.2 e
  ePingRes <- fmap makeResponse <$> pingRequest delayed
  logEvent "Ping response" ePingRes
  pure $ eventOrError (BackendError relayError) ePingRes

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
  return $ eventOrError (BackendError relayError) eRespUnwrapped

statusRequestWrapper :: MonadWidget t m
  => Dynamic t BaseUrl
  -> Dynamic t EncoinsStatusReqBody
  -> Event t ()
  -> m (Event t EncoinsStatusResult, Event t Status)
statusRequestWrapper dBaseUrl dReqBody e = do
  let ApiClient{..} = mkApiClient dBaseUrl
  eResp <- statusRequest (Right <$> dReqBody) e
  let eRespUnwrapped = makeResponse <$> eResp
  logEvent "statusRequestWrapper: eRespUnwrapped:" $ fmap showStatus <$> eRespUnwrapped
  return $ eventOrError (BackendError relayError) eRespUnwrapped

versionRequestWrapper :: MonadWidget t m
  => Dynamic t BaseUrl
  -> Event t ()
  -> m (Event t (Maybe ServerVersion))
versionRequestWrapper dBaseUrl e = do
  let ApiClient{..} = mkApiClient dBaseUrl
  eVersionRes <- fmap makeResponse <$> versionRequest e
  logEvent "Version" eVersionRes
  return eVersionRes

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

urlMap :: IntMap BaseUrl
urlMap = IMap.fromList $ zip [1..] $ map BasePath urls

getCheckedRelay :: MonadWidget t m => Event t () -> m (Dynamic t BaseUrl)
getCheckedRelay ev = mdo
  dUrls <- foldDyn IMap.delete urlMap eInvalidKey
  let dKeys = IMap.keys <$> dUrls
  let dKeyLength = length <$> dKeys
  eRandomIndex <- dyn $ (\l -> liftIO $ randomRIO (0, l-1)) <$> dKeyLength
  let eKey = attachPromptlyDynWith (\ks i -> ks !! i) dKeys eRandomIndex
  let eUrl = attachPromptlyDynWith (\urls' key -> urls' IMap.! key) dUrls eKey
  -- expect that dUrl never be empty txt
  dUrl <- foldDyn const (BasePath "") eUrl

  (ePingOk, ePingFail) <- pingRequestWrapper dUrl $ () <$ eUrl

  -- expect that dKey never be -1
  dKey <- foldDyn const (-1) eKey
  let eInvalidKey = tagPromptlyDyn dKey ePingFail

  -- Return url when it is ok, don't return default ever.
  foldDyn const (BasePath "") $ tagPromptlyDyn dUrl ePingOk