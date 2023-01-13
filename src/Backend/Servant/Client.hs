module Backend.Servant.Client where

import           Data.Maybe                   (isNothing)
import           Data.Proxy                   (Proxy(..))
import           Data.Text                    (Text)
import           Reflex.Dom                   hiding (Value)
import           Servant.API
import           Servant.Reflex
import           Witherable                   (catMaybes)

import           CSL                          (TransactionUnspentOutputs)
import           JS.Types

type AddSignatureReqBody = (Text, Text)

type API =   "newTx"        :> ReqBody '[JSON] (EncoinsRedeemerWithData, TransactionUnspentOutputs) :> Post '[JSON] Text
        :<|> "submitTx"     :> ReqBody '[JSON] (EncoinsRedeemerWithData, TransactionUnspentOutputs) :> Post '[JSON] ()
        :<|> "addSignature" :> ReqBody '[JSON] AddSignatureReqBody :> Post '[JSON] ()
        :<|> "ping"         :> Get '[JSON] ()

type RespEvent t a      = Event t (ReqResult () a)
type Res t m res        = Event t () -> m (RespEvent t res)
type DynReqBody t a     = Dynamic t (Either Text a)
type ReqRes t m req res = DynReqBody t req -> Res t m res

data ApiClient t m = ApiClient
  {
    newTxRequest        :: ReqRes t m (EncoinsRedeemerWithData, TransactionUnspentOutputs) Text,
    submitTxRequest     :: ReqRes t m (EncoinsRedeemerWithData, TransactionUnspentOutputs) (),
    addSignatureRequest :: ReqRes t m AddSignatureReqBody (),
    pingRequest         :: Res t m ()
  }

mkApiClient :: forall t m . MonadWidget t m => BaseUrl -> ApiClient t m
mkApiClient host = ApiClient{..}
  where
    (newTxRequest :<|> submitTxRequest :<|> addSignatureRequest :<|> pingRequest) = client (Proxy @API) (Proxy @m) (Proxy @()) (pure host)

---------------------------------------------- Utilities -------------------------------------

makeResponse :: ReqResult tag a -> Maybe a
makeResponse (ResponseSuccess _ a _) = Just a
makeResponse _ = Nothing

eventMaybe :: Reflex t => b -> Event t (Maybe a) -> (Event t a, Event t b)
eventMaybe errValue e = (catMaybes e, errValue <$ ffilter isNothing e)