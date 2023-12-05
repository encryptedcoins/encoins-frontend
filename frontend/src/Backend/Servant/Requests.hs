{-# LANGUAGE RecursiveDo #-}

module Backend.Servant.Requests where

import           Config.Config          (urlsBS)
import           Control.Lens           (view)
import           Control.Monad          (forM)
import           Control.Monad.IO.Class (MonadIO (..))
import           CSL                    (TransactionInputs)
import           Data.Aeson             (decode)
import           Data.Array.IO
import           Data.ByteString.Lazy   (fromStrict)
import           Data.Functor           (($>))
import           Data.List              (delete)
import           Data.Map               (Map)
import           Data.Maybe             (fromJust, isNothing)
import           Data.Text              (Text)
import           Reflex.Dom             hiding (Value)
import           Servant.Reflex         (BaseUrl (..), ReqResult (..))
import           System.Random          (randomRIO)
import           Witherable             (catMaybes)

import           Backend.Protocol.Types
import           Backend.Servant.Client
import           Backend.Utility        (normalizePingUrl)
import           ENCOINS.Common.Events
import           JS.App                 (pingServer)

newTxRequestWrapper :: MonadWidget t m
  => BaseUrl
  -> Dynamic t (InputOfEncoinsApi, TransactionInputs)
  -> Event t ()
  -> m (Event t (Either Int (Text, Text)))
newTxRequestWrapper baseUrl dReqBody e = do
  let ApiClient{..} = mkApiClient baseUrl
  eResp <- newTxRequest (Right <$> dReqBody) e
  let eRespUnwrapped = mkStatusOrResponse <$> eResp
  logEvent "newTx request" eRespUnwrapped
  pure eRespUnwrapped

submitTxRequestWrapper :: MonadWidget t m
  => BaseUrl
  -> Dynamic t SubmitTxReqBody
  -> Event t ()
  -> m (Event t (Either Int ()))
submitTxRequestWrapper baseUrl dReqBody e = do
  let ApiClient{..} = mkApiClient baseUrl
  eResp <- submitTxRequest (Right <$> dReqBody) e
  let eRespUnwrapped = (() <$) . mkStatusOrResponse <$> eResp
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
  -> m (Event t (Either Int ()))
serverTxRequestWrapper baseUrl dReqBody e = do
  let ApiClient{..} = mkApiClient baseUrl
  eResp <- serverTxRequest (Right <$> dReqBody) e
  let eRespUnwrapped = (() <$) . mkStatusOrResponse <$> eResp
  logEvent "serverTx request" eRespUnwrapped
  return eRespUnwrapped

statusRequestWrapper :: MonadWidget t m
  => BaseUrl
  -> Dynamic t EncoinsStatusReqBody
  -> Event t ()
  -> m (Event t (Either Int EncoinsStatusResult))
statusRequestWrapper baseUrl dReqBody e = do
  let ApiClient{..} = mkApiClient baseUrl
  eResp <- statusRequest (Right <$> dReqBody) e
  let eRespUnwrapped = mkStatusOrResponse <$> eResp
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
  -> m (Event t (Either Int (Map Text Integer)))
serversRequestWrapper baseUrl e = do
  let ApiClient{..} = mkApiClient baseUrl
  eResp <- serversRequest e
  let eRespUnwrapped = mkStatusOrResponse <$> eResp
  logEvent "servers request" eRespUnwrapped
  return eRespUnwrapped

-- Fetch available servers for app
currentRequestWrapper :: MonadWidget t m
  => BaseUrl
  -> Event t ()
  -> m (Event t (Either Int [Text]))
currentRequestWrapper baseUrl e = do
  let ApiClient{..} = mkApiClient baseUrl
  eResp <- currentRequest e
  let eRespUnwrapped = mkStatusOrResponse <$> eResp
  logEvent "current servers request" eRespUnwrapped
  return eRespUnwrapped

---------------------------------------------- Utilities ----------------------------------------

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

getRelayUrlE :: MonadWidget t m
  => Dynamic t [Text]
  -> Event t ()
  -> m (Event t (Maybe BaseUrl))
getRelayUrlE dUrls ev = do
  let eUrls = tagPromptlyDyn dUrls ev
  performEvent $ go <$> eUrls
  where
    go [] = pure Nothing
    go l = do
      idx <- randomRIO (0, length l - 1)
      let url = normalizePingUrl $ l !! idx
      pingOk <- pingServer url
      if pingOk
        then return $ Just $ BasePath url
        else go (delete url l)

makeResponse :: ReqResult tag a -> Maybe a
makeResponse (ResponseSuccess _ a _) = Just a
makeResponse _                       = Nothing

makeResponseEither :: ReqResult tag a -> Either Text a
makeResponseEither (ResponseSuccess _ a _)   = Right a
makeResponseEither (ResponseFailure _ txt _) = Left $ "ResponseFailure: " <> txt
makeResponseEither (RequestFailure _ txt)    = Left $ "RequestFailure: " <> txt

mkStatusOrResponse :: ReqResult tag a -> Either Int a
mkStatusOrResponse (ResponseSuccess _ a _) = Right a
mkStatusOrResponse (ResponseFailure _ _ xhr) = Left $ fromIntegral $ view xhrResponse_status xhr
mkStatusOrResponse (RequestFailure _ _) = Left $ -1

eventMaybe :: Reflex t => b -> Event t (Maybe a) -> (Event t a, Event t b)
eventMaybe errValue ev = (catMaybes ev, errValue <$ ffilter isNothing ev)

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
