module Backend.Servant.Requests where

import           Control.Monad                (void)
import           Data.Text                    (Text)
import           Reflex.Dom                   hiding (Value)
import           Servant.Reflex               (BaseUrl)
import           Witherable                   (catMaybes)

import           Backend.Protocol.Types
import           Backend.Servant.Client
import           Backend.Status               (Status(..))
import           CSL                          (TransactionInputs, Value)
import           ENCOINS.Common.Events        (logEvent)

newTxRequestWrapper :: MonadWidget t m
  => BaseUrl
  -> Dynamic t (Either (Address, Value, Address) (EncoinsRedeemer, EncoinsMode), TransactionInputs)
  -> Event t ()
  -> m (Event t (Text, Text), Event t Status)
newTxRequestWrapper baseUrl dReqBody e = do
  let ApiClient{..} = mkApiClient baseUrl
  eResp <- newTxRequest (Right <$> dReqBody) e
  let eRespUnwrapped = makeResponse <$> eResp
  logEvent "newTxRequestWrapper: eRespUnwrapped:" eRespUnwrapped
  return $ eventMaybe (BackendError "The current relay is down. Please, select another one.") eRespUnwrapped

submitTxRequestWrapper :: MonadWidget t m
  => BaseUrl
  -> Dynamic t SubmitTxReqBody
  -> Event t ()
  -> m (Event t (), Event t Status)
submitTxRequestWrapper baseUrl dReqBody e = do
  let ApiClient{..} = mkApiClient baseUrl
  eResp <- fmap (void . makeResponse) <$> submitTxRequest (Right <$> dReqBody) e
  return $ eventMaybe (BackendError "The current relay is down. Please, select another one.") eResp

pingRequestWrapper :: MonadWidget t m => BaseUrl -> Event t () -> m (Event t ())
pingRequestWrapper baseUrl e = do
  let ApiClient{..} = mkApiClient baseUrl
  e' <- fmap makeResponse <$> pingRequest e
  return $ void $ catMaybes e'

serverTxRequestWrapper :: MonadWidget t m
  => BaseUrl
  -> Dynamic t (Either (Address, Value, Address) (EncoinsRedeemer, EncoinsMode), TransactionInputs)
  -> Event t ()
  -> m (Event t (), Event t Status)
serverTxRequestWrapper baseUrl dReqBody e = do
  let ApiClient{..} = mkApiClient baseUrl
  eResp <- serverTxRequest (Right <$> dReqBody) e
  let eRespUnwrapped = (() <$) . makeResponse <$> eResp
  logEvent "serverTxRequestWrapper: eRespUnwrapped:" eRespUnwrapped
  return $ eventMaybe (BackendError "The current relay is down. Please, select another one.") eRespUnwrapped

statusRequestWrapper :: MonadWidget t m
  => BaseUrl
  -> Dynamic t EncoinsStatusReqBody
  -> Event t ()
  -> m (Event t EncoinsStatusResult, Event t Status)
statusRequestWrapper baseUrl dReqBody e = do
  let ApiClient{..} = mkApiClient baseUrl
  eResp <- statusRequest (Right <$> dReqBody) e
  let eRespUnwrapped = makeResponse <$> eResp
  logEvent "statusRequestWrapper: eRespUnwrapped:" eRespUnwrapped
  return $ eventMaybe (BackendError "The current relay is down. Please, select another one.") eRespUnwrapped
