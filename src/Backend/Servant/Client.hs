module Backend.Servant.Client where

import           Data.Aeson                   (decode)
import           Data.ByteString.Lazy         (fromStrict)
import           Data.FileEmbed               (embedFile)
import           Data.Maybe                   (isNothing, fromJust)
import           Data.Proxy                   (Proxy(..))
import           Data.Text                    (Text)
import           Reflex.Dom                   hiding (Value)
import           Servant.API
import           Servant.Checked.Exceptions   (Envelope)
import           Servant.Reflex
import           Witherable                   (catMaybes)

import           Backend.Types
import           CSL                          (TransactionUnspentOutputs)

pabIP :: BaseUrl
pabIP = BasePath $ fromJust $ decode $ fromStrict $(embedFile "config/backend_url.json")

type API =   "newTx"        :> ReqBody '[JSON] (EncoinsRedeemer, TransactionUnspentOutputs) :> Post '[JSON] (Envelope '[] (Text, Text))
        :<|> "submitTx"     :> ReqBody '[JSON] SubmitTxReqBody :> Post '[JSON] NoContent
        :<|> "ping"         :> Get '[JSON] ()

type RespEvent t a      = Event t (ReqResult () a)
type Res t m res        = Event t () -> m (RespEvent t res)
type DynReqBody t a     = Dynamic t (Either Text a)
type ReqRes t m req res = DynReqBody t req -> Res t m res

data ApiClient t m = ApiClient
  {
    newTxRequest        :: ReqRes t m (EncoinsRedeemer, TransactionUnspentOutputs) (Envelope '[] (Text, Text)),
    submitTxRequest     :: ReqRes t m SubmitTxReqBody NoContent,
    pingRequest         :: Res t m ()
  }

mkApiClient :: forall t m . MonadWidget t m => BaseUrl -> ApiClient t m
mkApiClient host = ApiClient{..}
  where
    (newTxRequest :<|> submitTxRequest :<|> pingRequest) = client (Proxy @API) (Proxy @m) (Proxy @()) (pure host)

---------------------------------------------- Utilities ----------------------------------------

makeResponse :: ReqResult tag a -> Maybe a
makeResponse (ResponseSuccess _ a _) = Just a
makeResponse _ = Nothing

eventMaybe :: Reflex t => b -> Event t (Maybe a) -> (Event t a, Event t b)
eventMaybe errValue e = (catMaybes e, errValue <$ ffilter isNothing e)

