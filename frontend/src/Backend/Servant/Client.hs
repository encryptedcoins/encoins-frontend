module Backend.Servant.Client where

import           Control.Monad.IO.Class       (MonadIO(..))
import           Data.Aeson                   (decode)
import           Data.ByteString.Lazy         (fromStrict)
import           Data.List                    (delete)
import           Data.Maybe                   (isNothing, fromJust)
import           Data.Proxy                   (Proxy(..))
import           Data.Text                    (Text)
import           Reflex.Dom                   hiding (Value)
import           Servant.API
import           Servant.Reflex
import           System.Random                (randomRIO)
import           Witherable                   (catMaybes)

import           Backend.Protocol.Types
import           CSL                          (TransactionInputs)
import           JS.App                       (pingServer)
import           Config.Config                (urlsBS)

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

type API =   "newTx"        :> ReqBody '[JSON] (InputOfEncoinsApi, TransactionInputs)
                            :> Post '[JSON] (Text, Text)
        :<|> "submitTx"     :> ReqBody '[JSON] SubmitTxReqBody
                            :> Post '[JSON] NoContent
        :<|> "ping"         :> Get '[JSON] NoContent
        :<|> "serverTx"     :> ReqBody '[JSON] (InputOfEncoinsApi, TransactionInputs)
                            :> Post '[JSON] NoContent
        :<|> "status"       :> ReqBody '[JSON] EncoinsStatusReqBody
                            :> Post '[JSON] EncoinsStatusResult

type RespEvent t a      = Event t (ReqResult () a)
type Res t m res        = Event t () -> m (RespEvent t res)
type DynReqBody t a     = Dynamic t (Either Text a)
type ReqRes t m req res = DynReqBody t req -> Res t m res

data ApiClient t m = ApiClient
  { newTxRequest        :: ReqRes t m (InputOfEncoinsApi, TransactionInputs) (Text, Text)
  , submitTxRequest     :: ReqRes t m SubmitTxReqBody NoContent
  , pingRequest         :: Res t m NoContent
  , serverTxRequest     :: ReqRes t m (InputOfEncoinsApi, TransactionInputs) NoContent
  , statusRequest       :: ReqRes t m EncoinsStatusReqBody EncoinsStatusResult
  }

mkApiClient :: forall t m . MonadWidget t m => BaseUrl -> ApiClient t m
mkApiClient host = ApiClient{..}
  where
    (newTxRequest :<|> submitTxRequest :<|> pingRequest :<|> serverTxRequest
      :<|> statusRequest) = client (Proxy @API) (Proxy @m) (Proxy @()) (pure host)

---------------------------------------------- Utilities ----------------------------------------

makeResponse :: ReqResult tag a -> Maybe a
makeResponse (ResponseSuccess _ a _) = Just a
makeResponse _ = Nothing

eventMaybe :: Reflex t => b -> Event t (Maybe a) -> (Event t a, Event t b)
eventMaybe errValue e = (catMaybes e, errValue <$ ffilter isNothing e)
