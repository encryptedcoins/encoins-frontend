module Backend.Servant.Client where

import           Control.Lens
import           Data.Aeson             (Value)
import           Data.Map               (Map)
import           Data.Proxy             (Proxy (..))
import           Data.Text              (Text)
import           Reflex.Dom             hiding (Value)
import           Servant.API
import           Servant.Reflex         (BaseUrl, ClientOptions (..),
                                         QParam (..), ReqResult (..), client,
                                         clientWithOpts)

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

type PinataIpfsAPI =
       "pinning" :> "pinJSONToIPFS"
                 :> ReqBody '[JSON] Person
                 :> Post '[JSON] PinJsonResponse
  :<|> "ipfs" :> Capture "cip" Text :> Get '[JSON] Value
  :<|> "data" :> "pinList" :> Get '[JSON] Files
  :<|> "pinning" :> "unpin" :> Capture "cip" Text :> Delete '[PlainText] Text
  :<|> "data" :> "pinList" :> QueryParam "status" Text :> Get '[JSON] Files
  :<|> "data" :> "pinList"
              :> QueryParam "status" Text
              :> QueryParam "metadata[name]" Text
              :> Get '[JSON] Files

data IpfsApiClient t m = MkIpfsApiClient
  { pinJson           :: ReqRes t m Person PinJsonResponse
  , fetchByCip        :: Dynamic t (Either Text Text) -> Res t m Value
  , fetchMetaAll      :: Res t m Files
  , unpinByCip        :: Dynamic t (Either Text Text) -> Res t m Text
  , fetchMetaPinned   :: Dynamic t (QParam Text) -> Res t m Files
  , fetchMetaPinnedId :: Dynamic t (QParam Text)
                      -> Dynamic t (QParam Text)
                      -> Res t m Files
  }

mkIpfsApiClient :: forall t m . MonadWidget t m
  => BaseUrl
  -> Maybe Text
  -> IpfsApiClient t m
mkIpfsApiClient host token = MkIpfsApiClient{..}
  where
    ( pinJson :<|>
      fetchByCip :<|>
      fetchMetaAll :<|>
      unpinByCip :<|>
      fetchMetaPinned :<|>
      fetchMetaPinnedId) = clientWithOpts
        (Proxy @PinataIpfsAPI)
        (Proxy @m)
        (Proxy @())
        (pure host)
        (clientOpts token)

clientOpts :: Maybe Text -> ClientOptions
clientOpts mToken = ClientOptions tweakReq
 where
  tweakReq :: XhrRequest a -> IO (XhrRequest a)
  tweakReq r = case mToken of
    Nothing -> return r
    Just token ->
      pure $ r & headerMod "authorization" .~ (Just $ "Bearer " <> token)
  headerMod d = xhrRequest_config . xhrRequestConfig_headers . at d


-- Ipfs request to  delegate backend

type BackIpfsApi =
         "minted" :> ReqBody '[JSON] Token :> Post '[JSON] Text
    :<|> "burned" :> ReqBody '[JSON] Token :> Post '[JSON] Text

data BackIpfsApiClient t m = MkBackIpfsApiClient
  { minted :: ReqRes t m Token Text
  , burned :: ReqRes t m Token Text
  }

mkBackIpfsApiClient :: forall t m . MonadWidget t m
  => BaseUrl
  -> BackIpfsApiClient t m
mkBackIpfsApiClient host = MkBackIpfsApiClient{..}
  where
    ( minted :<|>
      burned) = client
        (Proxy @BackIpfsApi)
        (Proxy @m)
        (Proxy @())
        (pure host)