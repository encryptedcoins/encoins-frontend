module Backend.Servant.Client where

import Data.Map (Map)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Reflex.Dom hiding (Value)
import Servant.API
import Servant.Reflex (BaseUrl, ReqResult (..), client)

import Backend.Protocol.Types
import CSL (TransactionInputs)

type API =
    "newTx"
        :> ReqBody '[JSON] (InputOfEncoinsApi, TransactionInputs)
        :> Post '[JSON] (Text, Text)
        :<|> "submitTx"
            :> ReqBody '[JSON] SubmitTxReqBody
            :> Post '[JSON] NoContent
        :<|> "ping" :> Get '[JSON] NoContent
        :<|> "serverTx"
            :> ReqBody '[JSON] (InputOfEncoinsApi, TransactionInputs)
            :> Post '[JSON] NoContent
        :<|> "status"
            :> ReqBody '[JSON] EncoinsStatusReqBody
            :> Post '[JSON] EncoinsStatusResult
        :<|> "version" :> Get '[JSON] ServerVersion
        :<|> "servers" :> Get '[JSON] (Map Text Integer)
        :<|> "current" :> Get '[JSON] [Text]
        :<|> "info"
            :> ReqBody '[JSON] Address
            :> Post '[JSON] (Text, Integer)

type RespEvent t a = Event t (ReqResult () a)
type Res t m res = Event t () -> m (RespEvent t res)
type DynReqBody t a = Dynamic t (Either Text a)
type ReqRes t m req res = DynReqBody t req -> Res t m res

data ApiClient t m = ApiClient
    { newTxRequest ::
        ReqRes t m (InputOfEncoinsApi, TransactionInputs) (Text, Text)
    , submitTxRequest :: ReqRes t m SubmitTxReqBody NoContent
    , pingRequest :: Res t m NoContent
    , serverTxRequest ::
        ReqRes t m (InputOfEncoinsApi, TransactionInputs) NoContent
    , statusRequest :: ReqRes t m EncoinsStatusReqBody EncoinsStatusResult
    , versionRequest :: Res t m ServerVersion
    , serversRequest :: Res t m (Map Text Integer)
    , currentRequest :: Res t m [Text]
    , infoRequest :: ReqRes t m Address (Text, Integer)
    }

mkApiClient :: forall t m. (MonadWidget t m) => BaseUrl -> ApiClient t m
mkApiClient host = ApiClient{..}
    where
        ( newTxRequest
                :<|> submitTxRequest
                :<|> pingRequest
                :<|> serverTxRequest
                :<|> statusRequest
                :<|> versionRequest
                :<|> serversRequest
                :<|> currentRequest
                :<|> infoRequest
            ) = client (Proxy @API) (Proxy @m) (Proxy @()) (pure host)

-- Save request to  delegate backend

type SaveApi =
    "ping" :> Get '[JSON] NoContent
        :<|> "save"
            :> ReqBody '[JSON] [SaveRequest]
            :> Post '[JSON] (Map AssetName StatusResponse)
        :<|> "restore" :> Get '[JSON] [(Text, Text)]

data SaveApiClient t m = MkSaveApiClient
    { sacPing :: Res t m NoContent
    , sacSave :: ReqRes t m [SaveRequest] (Map AssetName StatusResponse)
    , sacRestore :: Res t m [(Text, Text)]
    }

mkSaveApiClient ::
    forall t m.
    (MonadWidget t m) =>
    BaseUrl
    -> SaveApiClient t m
mkSaveApiClient host = MkSaveApiClient{..}
    where
        ( sacPing
                :<|> sacSave
                :<|> sacRestore
            ) = client (Proxy @SaveApi) (Proxy @m) (Proxy @()) (pure host)
