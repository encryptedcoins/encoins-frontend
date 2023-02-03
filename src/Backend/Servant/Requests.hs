module Backend.Servant.Requests where

import           Data.Text                    (Text)
import           Reflex.Dom                   hiding (Value)
import           Servant.Checked.Exceptions   (fromEnvelope)
import           Witherable                   (catMaybes)

import           Backend.Servant.Client
import           Backend.Types
import           CSL                          (TransactionUnspentOutputs)

newTxRequestWrapper :: MonadWidget t m => Dynamic t (EncoinsRedeemerWithData, TransactionUnspentOutputs) ->
  Event t () -> m (Event t Text)
newTxRequestWrapper dReqBody e = do
  let ApiClient{..} = mkApiClient pabIP
  eResp <- newTxRequest (Right <$> dReqBody) e
  let eRespUnwrapped = fmap (fmap (fromEnvelope (const "")) . makeResponse) eResp
  return $ catMaybes eRespUnwrapped

submitTxRequestWrapper :: MonadWidget t m => Dynamic t SubmitTxReqBody ->
  Event t () -> m (Event t ())
submitTxRequestWrapper dReqBody e = do
  let ApiClient{..} = mkApiClient pabIP
  e' <- fmap makeResponse <$> submitTxRequest (Right <$> dReqBody) e
  return $ catMaybes e'

pingRequestWrapper :: MonadWidget t m => Event t () -> m (Event t ())
pingRequestWrapper e = do
  let ApiClient{..} = mkApiClient pabIP
  e' <- fmap makeResponse <$> pingRequest e
  return $ catMaybes e'