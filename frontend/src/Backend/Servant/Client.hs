module Backend.Servant.Client where

import           Data.Map               (Map)
import           Data.Proxy             (Proxy (..))
import           Data.Text              (Text)
import           Reflex.Dom             hiding (Value)
import Control.Lens
import           Servant.API
import           Servant.Reflex         (clientWithOpts, ClientOptions(..), BaseUrl, ReqResult (..), client)
import           Data.Aeson             (Value)

import           Backend.Protocol.Types
import           CSL                    (TransactionInputs)

type API =   "newTx"        :> ReqBody '[JSON] (InputOfEncoinsApi, TransactionInputs)
                            :> Post '[JSON] (Text, Text)
        :<|> "submitTx"     :> ReqBody '[JSON] SubmitTxReqBody
                            :> Post '[JSON] NoContent
        :<|> "ping"         :> Get '[JSON] NoContent
        :<|> "serverTx"     :> ReqBody '[JSON] (InputOfEncoinsApi, TransactionInputs)
                            :> Post '[JSON] NoContent
        :<|> "status"       :> ReqBody '[JSON] EncoinsStatusReqBody
                            :> Post '[JSON] EncoinsStatusResult
        :<|> "version"      :> Get '[JSON] ServerVersion
        :<|> "servers"      :> Get '[JSON] (Map Text Integer)
        :<|> "current"      :> Get '[JSON] [Text]
        :<|> "info"         :> ReqBody '[JSON] Address
                            :> Post '[JSON] (Text, Integer)

type RespEvent t a      = Event t (ReqResult () a)
type Res t m res        = Event t () -> m (RespEvent t res)
type DynReqBody t a     = Dynamic t (Either Text a)
type ReqRes t m req res = DynReqBody t req -> Res t m res

data ApiClient t m = ApiClient
  { newTxRequest           :: ReqRes t m (InputOfEncoinsApi, TransactionInputs) (Text, Text)
  , submitTxRequest        :: ReqRes t m SubmitTxReqBody NoContent
  , pingRequest            :: Res t m NoContent
  , serverTxRequest        :: ReqRes t m (InputOfEncoinsApi, TransactionInputs) NoContent
  , statusRequest          :: ReqRes t m EncoinsStatusReqBody EncoinsStatusResult
  , versionRequest         :: Res t m ServerVersion
  , serversRequest         :: Res t m (Map Text Integer)
  , currentRequest         :: Res t m [Text]
  , infoRequest            :: ReqRes t m Address (Text, Integer)
  }

mkApiClient :: forall t m . MonadWidget t m => BaseUrl -> ApiClient t m
mkApiClient host = ApiClient{..}
  where
    ( newTxRequest :<|>
      submitTxRequest :<|>
      pingRequest :<|>
      serverTxRequest :<|>
      statusRequest :<|>
      versionRequest :<|>
      serversRequest :<|>
      currentRequest :<|>
      infoRequest) = client (Proxy @API) (Proxy @m) (Proxy @()) (pure host)


-- IPFS

jwtToken :: Text
jwtToken = "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VySW5mb3JtYXRpb24iOnsiaWQiOiJjMmMzNTNjMC04YzIwLTQ0YmQtODc5NC1mY2YxZDczYjMxOWQiLCJlbWFpbCI6ImNvbnRpbmdlbnRhbEBnbWFpbC5jb20iLCJlbWFpbF92ZXJpZmllZCI6dHJ1ZSwicGluX3BvbGljeSI6eyJyZWdpb25zIjpbeyJpZCI6IkZSQTEiLCJkZXNpcmVkUmVwbGljYXRpb25Db3VudCI6MX0seyJpZCI6Ik5ZQzEiLCJkZXNpcmVkUmVwbGljYXRpb25Db3VudCI6MX1dLCJ2ZXJzaW9uIjoxfSwibWZhX2VuYWJsZWQiOmZhbHNlLCJzdGF0dXMiOiJBQ1RJVkUifSwiYXV0aGVudGljYXRpb25UeXBlIjoic2NvcGVkS2V5Iiwic2NvcGVkS2V5S2V5IjoiNjlmZGNhZDY1ZjgzNzYzZTJlMzMiLCJzY29wZWRLZXlTZWNyZXQiOiJlNjU4Yjg3ZWQ2YzAzNzNjYzk0MGYzZDJmMmE1OWE5NjkzOTk4NjY4MTYxMzhkMWQ1ZWM3YjA2ZTI5NzE3MzdiIiwiaWF0IjoxNzAzNzY0Njg1fQ.ANQVAtuW0xAKVNXu57zktHMF2_TwbAxi4ciIceLc2so"

type IpfsAPI =
    "pinning"
    :> "pinJSONToIPFS"
    :> ReqBody '[JSON] Person
    :> Post '[JSON] Value

data IpfsApiClient t m = MkIpfsApiClient
  { ipfsAddRequest :: ReqRes t m Person Value
  }

clientOpts :: ClientOptions
clientOpts = ClientOptions tweakReq
 where
  tweakReq r = do
    return $ r & headerMod "authorization" .~ (Just jwtToken)
  headerMod d = xhrRequest_config . xhrRequestConfig_headers . at d

mkIpfsApiClient :: forall t m . MonadWidget t m
  => BaseUrl
  -> IpfsApiClient t m
mkIpfsApiClient host = MkIpfsApiClient{..}
  where
    (ipfsAddRequest) = clientWithOpts
        (Proxy @IpfsAPI)
        (Proxy @m)
        (Proxy @())
        (pure host)
        clientOpts