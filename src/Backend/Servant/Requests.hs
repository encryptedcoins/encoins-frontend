module Backend.Servant.Requests where

import           Data.Text                  (Text)
import           Reflex.Dom                 hiding (Value)
import           Servant.Reflex             (BaseUrl(..))
import           Witherable                 (catMaybes)

import           Backend.Servant.Client
import           Backend.Types
import           CSL                        (TransactionUnspentOutputs)

pabIP :: BaseUrl
pabIP = BasePath "http://localhost:3000"

newTxRequestWrapper :: MonadWidget t m => Dynamic t EncoinsRedeemerWithData -> Dynamic t TransactionUnspentOutputs ->
  Event t () -> m (Event t Text)
newTxRequestWrapper dRed dUTXOs e = do
  let ApiClient{..} = mkApiClient pabIP
  e' <- fmap makeResponse <$> newTxRequest (Right <$> zipDyn dRed dUTXOs) e
  return $ catMaybes e'

submitTxRequestWrapper :: MonadWidget t m => Dynamic t EncoinsRedeemerWithData -> Dynamic t TransactionUnspentOutputs ->
  Event t () -> m (Event t ())
submitTxRequestWrapper dRed dUTXOs e = do
  let ApiClient{..} = mkApiClient pabIP
  e' <- fmap makeResponse <$> submitTxRequest (Right <$> zipDyn dRed dUTXOs) e
  return $ catMaybes e'

addSignatureRequestWrapper :: MonadWidget t m => Dynamic t Text -> Dynamic t Text ->
  Event t () -> m (Event t ())
addSignatureRequestWrapper dTx dSig e = do
  let ApiClient{..} = mkApiClient pabIP
  e' <- fmap makeResponse <$> addSignatureRequest (Right <$> zipDyn dTx dSig) e
  return $ catMaybes e'

pingRequestWrapper :: MonadWidget t m => Event t () -> m (Event t ())
pingRequestWrapper e = do
  let ApiClient{..} = mkApiClient pabIP
  e' <- fmap makeResponse <$> pingRequest e
  return $ catMaybes e'