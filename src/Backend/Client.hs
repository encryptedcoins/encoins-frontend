module Backend.Client where

import           Data.Proxy                   (Proxy(..))
import           Data.Text                    (Text)
import           Reflex.Dom                   hiding (Value)
import           Servant.API
import           Servant.Reflex

import           CSL                          (TransactionUnspentOutputs)
import           JS.Types

type MapUTXO = TransactionUnspentOutputs

type API = "relayRequestNewTx"  :> ReqBody '[JSON] (EncoinsRedeemer, MapUTXO) :> Post '[JSON] Text
        :<|> "relayRequestPing" :> Get '[JSON] ()

type RespEvent t a      = Event t (ReqResult () a)
type Res t m res        = Event t () -> m (RespEvent t res)
type DynReqBody t a     = Dynamic t (Either Text a)
type ReqRes t m req res = DynReqBody t req -> Res t m res

data ApiClient t m = ApiClient
  {
    relayRequest :: ReqRes t m (EncoinsRedeemer, MapUTXO) Text,
    pingRequest  :: Res t m ()
  }

mkApiClient :: forall t m . MonadWidget t m => BaseUrl -> ApiClient t m
mkApiClient host = ApiClient{..}
  where
    (relayRequest :<|> pingRequest) = client (Proxy @API) (Proxy @m) (Proxy @()) (pure host)

makeResponse :: ReqResult tag a -> Maybe a
makeResponse (ResponseSuccess _ a _) = Just a
makeResponse _ = Nothing
