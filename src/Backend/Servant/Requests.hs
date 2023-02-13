module Backend.Servant.Requests where

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
  performEvent_ $ liftIO . logInfo . pack <$> ("request!" <$ e)
  eResp <- newTxRequest (Right <$> dReqBody) e
  performEvent_ $ liftIO . logInfo . pack <$> ("response!" <$ eResp)
  let eRespUnwrapped = fmap (fmap (fromEnvelope (const "")) . makeResponse) eResp
  performEvent_ $ liftIO . logInfo . pack . show <$> eRespUnwrapped
  -- return $ eventMaybe (StatusError "The current relay is down. Please, select another one.") eRespUnwrapped
  return $ eventMaybe (StatusError "The relay is down.") eRespUnwrapped

submitTxRequestWrapper :: MonadWidget t m => Dynamic t SubmitTxReqBody ->
  Event t () -> m (Event t (), Event t Status)
submitTxRequestWrapper dReqBody e = do
  let ApiClient{..} = mkApiClient pabIP
  e' <- fmap makeResponse <$> submitTxRequest (Right <$> dReqBody) e
  -- return $ eventMaybe (StatusError "The current relay is down. Please, select another one.") e'
  return $ eventMaybe (StatusError "The relay is down.") e'

pingRequestWrapper :: MonadWidget t m => Event t () -> m (Event t ())
pingRequestWrapper e = do
  let ApiClient{..} = mkApiClient pabIP
  e' <- fmap makeResponse <$> pingRequest e
  return $ catMaybes e'