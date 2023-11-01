{-# LANGUAGE RecursiveDo #-}

module Backend.Servant.Requests where

import           Config.Config          (urlsBS)
import           Control.Monad          (void)
import           Control.Monad.IO.Class (MonadIO (..))
import           CSL                    (TransactionInputs)
import           Data.Aeson             (decode)
import           Data.Bool              (bool)
import           Data.ByteString.Lazy   (fromStrict)
import           Data.Either.Extra
import           Data.IntMap            (IntMap)
import qualified Data.IntMap            as IMap
import           Data.List              (delete)
import           Data.Maybe             (fromJust, isNothing)
import           Data.Text              (Text)
import           Reflex.Dom             hiding (Value)
import           Servant.API            (NoContent)
import           Servant.Reflex         (BaseUrl (..))
import           System.Random          (randomRIO)
import           Witherable             (catMaybes)

import           Backend.Protocol.Types
import           Backend.Servant.Client
import           Backend.Status         (Status (..), relayError)
import           Backend.Utility        (normalizePingUrl)
import           ENCOINS.Common.Events  (logDyn, logEvent, newEvent, postDelay)
import           JS.App                 (pingServer)

import           Debug.Trace
-- import System.Random
import           Control.Monad
import           Data.Array.IO

newTxRequestWrapper :: MonadWidget t m
  => BaseUrl
  -> Dynamic t (InputOfEncoinsApi, TransactionInputs)
  -> Event t ()
  -> m (Event t (Text, Text), Event t Status)
newTxRequestWrapper baseUrl dReqBody e = do
  let ApiClient{..} = mkApiClient baseUrl
  eResp <- newTxRequest (Right <$> dReqBody) e
  let eRespUnwrapped = makeResponse <$> eResp
  logEvent "newTxRequestWrapper: eRespUnwrapped:" eRespUnwrapped
  return $ eventMaybe (BackendError relayError) eRespUnwrapped
--   -> m (Event t (Either Text (Text, Text)))
-- newTxRequestWrapper baseUrl dReqBody e = do
--   let ApiClient{..} = mkApiClient baseUrl
--   eResp <- newTxRequest (Right <$> dReqBody) e
--   let eRespUnwrapped = makeResponseEither <$> eResp
--   logEvent "newTxRequestWrapper: eRespUnwrapped:" eRespUnwrapped
--   pure eRespUnwrapped

submitTxRequestWrapper :: MonadWidget t m
  => BaseUrl
  -> Dynamic t SubmitTxReqBody
  -> Event t ()
  -> m (Event t (), Event t Status)
submitTxRequestWrapper baseUrl dReqBody e = do
  let ApiClient{..} = mkApiClient baseUrl
  eResp <- fmap (void . makeResponse) <$> submitTxRequest (Right <$> dReqBody) e
  return $ eventMaybe (BackendError relayError) eResp
--   -> m (Event t (Either Text ()))
-- submitTxRequestWrapper baseUrl dReqBody e = do
--   let ApiClient{..} = mkApiClient baseUrl
--   eResp <- fmap (void . makeResponseEither) <$> submitTxRequest (Right <$> dReqBody) e
--   return eResp

pingRequestWrapper :: MonadWidget t m
  => BaseUrl
  -> Event t ()
  -> m (Event t (Either Text BaseUrl))
pingRequestWrapper baseUrl@(BasePath url) e = do
  let ApiClient{..} = mkApiClient $ BasePath $ normalizePingUrl url
  delayed <- delay 0.2 e
  ePingRes <- fmap makeResponseEither <$> pingRequest delayed
  logEvent "Ping response" ePingRes
  pure $ mapRight (const baseUrl) <$> ePingRes

serverTxRequestWrapper :: MonadWidget t m
  => BaseUrl
  -> Dynamic t (InputOfEncoinsApi, TransactionInputs)
  -> Event t ()
  -> m (Event t (), Event t Status)
serverTxRequestWrapper baseUrl dReqBody e = do
  let ApiClient{..} = mkApiClient baseUrl
  eResp <- serverTxRequest (Right <$> dReqBody) e
  let eRespUnwrapped = (() <$) . makeResponse <$> eResp
  logEvent "serverTxRequestWrapper: eRespUnwrapped:" eRespUnwrapped
  return $ eventMaybe (BackendError relayError) eRespUnwrapped

statusRequestWrapper :: MonadWidget t m
  => BaseUrl
  -> Dynamic t EncoinsStatusReqBody
  -> Event t ()
  -> m (Event t EncoinsStatusResult, Event t Status)
statusRequestWrapper baseUrl dReqBody e = do
  let ApiClient{..} = mkApiClient baseUrl
  eResp <- statusRequest (Right <$> dReqBody) e
  let eRespUnwrapped = makeResponse <$> eResp
  logEvent "statusRequestWrapper: eRespUnwrapped:" $ fmap showStatus <$> eRespUnwrapped
  return $ eventMaybe (BackendError relayError) eRespUnwrapped

versionRequestWrapper :: MonadWidget t m
  => BaseUrl
  -> Event t ()
  -> m (Event t (Maybe ServerVersion))
versionRequestWrapper baseUrl e = do
  let ApiClient{..} = mkApiClient baseUrl
  eVersionRes <- fmap makeResponse <$> versionRequest e
  logEvent "Version" eVersionRes
  return eVersionRes

eventMaybe :: Reflex t => b -> Event t (Maybe a) -> (Event t a, Event t b)
eventMaybe errValue e = (catMaybes e, errValue <$ ffilter isNothing e)

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

mkUrlMap :: [Text] -> IntMap BaseUrl
mkUrlMap = IMap.fromList . zip [1..] . map BasePath

getValidRelay :: MonadWidget t m
  => Event t ()
  -> m (Event t (Maybe BaseUrl))
getValidRelay ev = do
  urlsShuffled <- liftIO $ shuffle $ map BasePath urls
  eList <- traverse (\u -> checkUrl u ev) urlsShuffled
  let eUrl = leftmost eList
  logEvent "getValidRelay: eUrl" eUrl
  eEmptyList <- bool (pure never) newEvent $ null eList
  logEvent "getValidRelay: eEmptyList" eEmptyList
  pure $ leftmost [Just <$> eUrl, Nothing <$ eEmptyList]


checkUrl :: MonadWidget t m
  => BaseUrl
  -> Event t ()
  -> m (Event t BaseUrl)
checkUrl url ev = mdo
  ePing <- pingRequestWrapper url ev
  let (ePingFail, ePingOk) = fanEither ePing
  pure ePingOk

-- foldUrlChecker :: MonadWidget t m
--   => Event t ()
--   -> (Bool, Maybe BaseUrl)
--   -> BaseUrl
--   -> m (Bool, Maybe BaseUrl)
-- foldUrlChecker ev acc@(isLastPingOk, _) url = do
--   if isLastPingOk
--     then pure acc
--     else do
--       ePing <- pingRequestWrapper url ev
--       logEvent "foldUrlChecker: ePing" ePing
--       dUrl <- foldDyn const Nothing ePing
--       logDyn "foldUrlChecker: dUrl" dUrl
--       mUrl <- sample $ current dUrl
--       case mUrl of
--         Nothing -> pure (False, Nothing)
--         Just _ -> pure (True, Just url)

-- foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b


-- getValidRelay :: MonadWidget t m
--   => Event t ()
--   -> m (Event t (Maybe BaseUrl))
-- getValidRelay ev = mdo

--   let urlMap = map BasePath urls

--   dUrls <- trace ("urlMap: " <> show urlMap) $ holdDyn urlMap $ coincidence $ eUrls <$ ePingFail
--   -- dUrls <- holdDyn urlMap $ tagPromptlyDyn (constDyn urlMap) ev
--   logDyn "getValidRelay: dUrls" dUrls

--   let (dmUrl, dmUrls) = splitDynPure $ ffor dUrls $ \us -> trace ("urls: " <> show us) case us of
--         [] -> (Left "empty head", Left "empty tail")
--         [x] -> (Right x, Left "empty tail")
--         x:xs -> (Right x, Right xs)
--   logDyn "getValidRelay: dmUrl" dmUrl
--   logDyn "getValidRelay: dmUrls" dmUrls

--   let (eEmptyHead, eUrl) = fanEither $ updated dmUrl
--   let (eEmptyTail, eUrls) = fanEither $ updated dmUrls
--   logEvent "getValidRelay: eEmptyHead" eEmptyHead
--   logEvent "getValidRelay: eEmptyTail" eEmptyTail
--   logEvent "getValidRelay: eUrl" eUrl
--   logEvent "getValidRelay: eUrls" eUrls

--   dUrl <- holdDyn (BasePath "http://localhost:1000") eUrl
--   logDyn "getValidRelay: dUrl" dUrl

--   let eShouldPing = leftmost [ev, () <$ eUrl]

--   eePing <- dyn $ (\url -> trace ("dUrl: " <> show url) $ pingRequestWrapper url eShouldPing) <$> dUrl
--   let (ePingFail, ePingOk) = fanEither $ coincidence eePing
--   logEvent "getValidRelay: ePingOk" ePingOk
--   logEvent "getValidRelay: ePingFail" ePingFail

--   -- postDelay 1

--   pure $ leftmost [tagPromptlyDyn (Just <$> dUrl) ePingOk, tagPromptlyDyn (constDyn Nothing) eEmptyHead]


-- -- | Randomly shuffle a list
-- --   /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArr xsLength xs
        forM [1..xsLength] $ \i -> do
            j <- randomRIO (i,xsLength)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    xsLength = length xs
    newArr :: Int -> [a] -> IO (IOArray Int a)
    newArr n ls =  newListArray (1,n) ls

-- getValidRelay :: MonadWidget t m
--   => Event t ()
--   -> m (Dynamic t (Maybe BaseUrl))
-- getValidRelay ev = mdo

--   let urlMap = mkUrlMap urls

--   dUrls <- holdDyn urlMap eRestUrls
--   logDyn "getValidRelay: dUrls" dUrls

--   let (eAllRelayError, eNonEmptyUrls) = fanEither $ (\us -> bool (Right us) (Left "err") $ IMap.null us) <$> updated dUrls
--   logEvent "getValidRelay: eNonEmptyUrls" eNonEmptyUrls
--   logEvent "getValidRelay: eAllRelayError" eAllRelayError

--   dNonEmptyUrls <- foldDyn const urlMap eNonEmptyUrls
--   logDyn "getValidRelay: dNonEmptyUrls" dNonEmptyUrls

--   (eUrl, eInvalidKey) <-
--     getRandomUrl dNonEmptyUrls $ leftmost [ev, () <$ eNonEmptyUrls]

--   postDelay 1

--   let eRestUrls = attachPromptlyDynWith
--         (\urls' k -> IMap.delete k urls')
--         dNonEmptyUrls
--         eInvalidKey
--   logEvent "getValidRelay: eRestUrls" eRestUrls



--   foldDyn const Nothing $ leftmost [Just <$> eUrl, Nothing <$ eAllRelayError]


  -- ev <- newEvent
  -- let eRestUrls = urlMap <$ ev

-- getRandomUrl :: MonadWidget t m
--   => Dynamic t (IntMap BaseUrl)
--   -> Event t ()
--   -> m (Event t BaseUrl, Event t Int)
-- getRandomUrl dUrls ev = mdo
--   let dKeys = IMap.keys <$> dUrls
--   logDyn "getRandomUrl: dKeys" dKeys
--   let dKeyQuantity = length <$> dKeys
--   logDyn "getRandomUrl: dKeyQuantity" dKeyQuantity

--   eRandomIndex <- dyn $ (\l -> liftIO $ randomRIO (0, l - 1)) <$> dKeyQuantity
--   logEvent "getRandomUrl: eRandomIndex" eRandomIndex

--   let eKey = attachPromptlyDynWith (\ks i -> ks !! i) dKeys eRandomIndex
--   logEvent "getRandomUrl: eKey" eKey
--   let eUrl = attachPromptlyDynWith (\urls' key -> urls' IMap.! key) dUrls eKey
--   logEvent "getRandomUrl: eUrl" eUrl

--   -- expect that dUrl never be empty txt
--   dUrl <- foldDyn const (BasePath "") eUrl
--   logDyn "getRandomUrl: dUrl" dUrl

--   eePing <- dyn $ (\url -> pingRequestWrapper url ev) <$> dUrl
--   let (ePingFail, ePingOk) = fanEither $ coincidence eePing
--   logEvent "getRandomUrl: ePingOk" ePingOk
--   logEvent "getRandomUrl: ePingFail" ePingFail

--   -- expect that dKey never be -1
--   -- dKey <- foldDyn const (-1) eKey
--   let eInvalidKey = coincidence $ eKey <$ ePingFail
--   logEvent "getRandomUrl: eInvalidKey" eInvalidKey

--   -- Return url when it is ok, don't return default ever.
--   pure (coincidence $ eUrl <$ ePingOk, eInvalidKey)


-- getRandomUrl :: MonadWidget t m
--   => Dynamic t (IntMap BaseUrl)
--   -> m (Event t BaseUrl, Event t Int)
-- getRandomUrl dUrls = mdo
--   let dKeys = IMap.keys <$> dUrls
--   logDyn "getRandomUrl: dKeys" dKeys
--   let dKeyLength = length <$> dKeys
--   logDyn "getRandomUrl: dKeyLength" dKeyLength
--   eRandomIndex <- dyn $ (\l -> liftIO $ randomRIO (0, l-1)) <$> dKeyLength
--   logEvent "getRandomUrl: eRandomIndex" eRandomIndex
--   let eKey = attachPromptlyDynWith (\ks i -> ks !! i) dKeys eRandomIndex
--   logEvent "getRandomUrl: eKey" eKey
--   let eUrl = attachPromptlyDynWith (\urls' key -> urls' IMap.! key) dUrls eKey
--   logEvent "getRandomUrl: eUrl" eUrl

--   -- expect that dUrl never be empty txt
--   dUrl <- foldDyn const (BasePath "") eUrl
--   logDyn "getRandomUrl: dUrl" dUrl

--   (ePingOk, ePingFail) <- pingRequestWrapper dUrl $ () <$ eUrl
--   logEvent "getRandomUrl: ePingOk" ePingOk
--   logEvent "getRandomUrl: ePingFail" ePingFail

--   -- expect that dKey never be -1
--   dKey <- foldDyn const (-1) eKey
--   let eInvalidKey = tagPromptlyDyn dKey ePingFail
--   logEvent "getRandomUrl: eInvalidKey" eInvalidKey

--   -- Return url when it is ok, don't return default ever.
--   pure (tagPromptlyDyn dUrl ePingOk, eInvalidKey)
