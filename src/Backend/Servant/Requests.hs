module Backend.Servant.Requests where

import           Control.Monad                (void)
import           Control.Monad.IO.Class       (liftIO)
import           Data.Text                    (Text, pack)
import           Reflex.Dom                   hiding (Value)
import           Servant.Checked.Exceptions   (fromEnvelope)
import           Witherable                   (catMaybes)

import           Backend.Servant.Client
import           Backend.Status               (Status(..))
import           Backend.Types
import           CSL                          (TransactionUnspentOutputs)
import           JS.Website                   (logInfo)

newTxRequestWrapper :: MonadWidget t m => Dynamic t (EncoinsRedeemerWithData, TransactionUnspentOutputs) ->
  Event t () -> m (Event t Text, Event t Status)
newTxRequestWrapper dReqBody e = do
  let ApiClient{..} = mkApiClient pabIP
  eResp <- newTxRequest (Right <$> dReqBody) e
  let eRespUnwrapped = fmap (fmap (fromEnvelope (const "")) . makeResponse) eResp
  performEvent_ $ liftIO . logInfo . pack . show <$> eRespUnwrapped
  return $ eventMaybe (BackendError "The current relay is down. Please, select another one.") eRespUnwrapped

submitTxRequestWrapper :: MonadWidget t m => Dynamic t SubmitTxReqBody ->
  Event t () -> m (Event t (), Event t Status)
submitTxRequestWrapper dReqBody e = do
  let ApiClient{..} = mkApiClient pabIP
  eResp <- fmap (void . makeResponse) <$> submitTxRequest (Right <$> dReqBody) e
  return $ eventMaybe (BackendError "The current relay is down. Please, select another one.") eResp

pingRequestWrapper :: MonadWidget t m => Event t () -> m (Event t ())
pingRequestWrapper e = do
  let ApiClient{..} = mkApiClient pabIP
  e' <- fmap makeResponse <$> pingRequest e
  return $ catMaybes e'