{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module I18n.Reflex.I18n
    ( module I18n.Reflex.I18n
    , module I18n.Common.I18n
    )
where

import Control.Monad.Base (MonadBase (..))
import Control.Monad.Exception (MonadAsyncException, MonadException)
import Control.Monad.Primitive (PrimMonad, PrimState, primitive)
import Control.Monad.Reader
    ( MonadFix
    , MonadIO
    , MonadReader (ask, local)
    , MonadTrans (..)
    , ReaderT (..)
    , mapReaderT
    , void
    )
import Control.Monad.Ref
    ( MonadAtomicRef
    , MonadRef
    , Ref
    , newRef
    , readRef
    , writeRef
    )
import qualified Control.Monad.State.Strict as StrictState
import Control.Monad.Trans.Control
    ( ComposeSt
    , MonadBaseControl (..)
    , MonadTransControl (..)
    , defaultLiftBaseWith
    , defaultRestoreM
    )
import Data.Coerce (coerce)
import Data.Text (Text)
import I18n.Common.I18n
    ( FieldName (..)
    , HasI18n (..)
    , HasI18nFields (..)
    , Locale (..)
    , LocalizeFactor (..)
    , Localized
    , capitalizeConstTextHead
    , constFieldName
    , fieldNameConst
    , localizeENWith
    , standardLocalize
    )
#ifndef ghcjs_HOST_OS
import Language.Javascript.JSaddle.Types (MonadJSM)
#endif
import Reflex
    ( Adjustable (..)
    , EventSelectorInt (EventSelectorInt)
    , EventWriter
    , EventWriterT (EventWriterT)
    , MonadHold
    , MonadQuery (..)
    , MonadSample (..)
    , NotReady
    , PatchDMap (PatchDMap)
    , PerformEvent (..)
    , PostBuild
    , PostBuildT (PostBuildT)
    , QueryT (QueryT)
    , Reflex (Dynamic, current, updated)
    , Requester (..)
    , RequesterT (RequesterT)
    , TriggerEvent
    , TriggerEventT (TriggerEventT)
    , coerceEvent
    , ffor
    , fmapCheap
    )
import Reflex.Dom.Core
    ( DomBuilder
        ( DomBuilderSpace
        , element
        , inputElement
        , placeRawElement
        , selectElement
        , textAreaElement
        , textNode
        , wrapRawElement
        )
    , DomRenderHook
    , DomSpace (RawDocument)
    , HasDocument (..)
    , Prerender (..)
    , TextNodeConfig
        ( TextNodeConfig
        , _textNodeConfig_initialContents
        , _textNodeConfig_setContents
        )
    , WithJSContextSingleton (WithJSContextSingleton)
    )
import Reflex.Host.Class (MonadReflexCreateTrigger)

-- | A type class for monads that have access to some 'Dynamic' @locale@.
class (Reflex t, Monad m) => HasLocale t locale m | m -> locale where
    askLocale :: m (Dynamic t locale)

-- | A simple 'ReaderT' implementation for 'HasLocale'.
newtype LocalizeT t locale m a = LocalizeT {unLocalizeT :: ReaderT (Dynamic t locale) m a}
    deriving newtype
        ( Applicative
        , Functor
        , Monad
        , MonadAsyncException
        , MonadAtomicRef
        , MonadException
        , MonadFix
        , MonadHold t
        , MonadIO
        , MonadSample t
        , MonadTrans
        , DomRenderHook t
        )

deriving newtype instance
    (MonadReflexCreateTrigger t m) =>
    MonadReflexCreateTrigger t (LocalizeT t locale m)

deriving newtype instance (PostBuild t m) => PostBuild t (LocalizeT t locale m)

deriving newtype instance
    (TriggerEvent t m) => TriggerEvent t (LocalizeT t locale m)

instance (Prerender t m) => Prerender t (LocalizeT t locale m) where
    type Client (LocalizeT t locale m) = LocalizeT t locale (Client m)
    prerender server client = LocalizeT $
        ReaderT $ \d ->
            prerender (runLocalize d server) (runLocalize d client)

instance (Monad m, Reflex t) => HasLocale t locale (LocalizeT t locale m) where
    askLocale = LocalizeT ask

instance (Reflex t) => MonadTransControl (LocalizeT t locale) where
    type StT (LocalizeT t locale) a = a
    liftWith f = LocalizeT . ReaderT $ \r -> f $ \t -> runLocalize r t
    restoreT = LocalizeT . ReaderT . const
    {-# INLINEABLE liftWith #-}
    {-# INLINEABLE restoreT #-}

instance (MonadBase b m) => MonadBase b (LocalizeT t locale m) where
    liftBase = lift . liftBase

instance (Reflex t, MonadBaseControl b m) => MonadBaseControl b (LocalizeT t locale m) where
    type StM (LocalizeT t locale m) a = ComposeSt (LocalizeT t locale) m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM
    {-# INLINEABLE liftBaseWith #-}
    {-# INLINEABLE restoreM #-}

instance (MonadRef m) => MonadRef (LocalizeT t locale m) where
    type Ref (LocalizeT t locale m) = Ref m
    {-# INLINEABLE newRef #-}
    newRef = lift . newRef
    {-# INLINEABLE readRef #-}
    readRef = lift . readRef
    {-# INLINEABLE writeRef #-}
    writeRef r = lift . writeRef r

instance (MonadReader r m) => MonadReader r (LocalizeT t locale m) where
    ask = lift ask
    local f (LocalizeT a) = LocalizeT $ mapReaderT (local f) a

instance (HasLocale t locale m) => HasLocale t locale (StrictState.StateT s m) where
    askLocale = lift askLocale

instance (HasLocale t locale m) => HasLocale t locale (ReaderT e m) where
    askLocale = lift askLocale

instance (Monad m, NotReady t m) => NotReady t (LocalizeT t locale m)

instance (Adjustable t m, MonadHold t m) => Adjustable t (LocalizeT t locale m) where
    runWithReplace a0 a' = LocalizeT $ runWithReplace (unLocalizeT a0) (fmapCheap unLocalizeT a')
    traverseDMapWithKeyWithAdjust f dm edm =
        LocalizeT $
            traverseDMapWithKeyWithAdjust
                (\k v -> unLocalizeT $ f k v)
                (coerce dm)
                (coerceEvent edm)
    {-# INLINEABLE traverseDMapWithKeyWithAdjust #-}
    traverseDMapWithKeyWithAdjustWithMove f dm edm =
        LocalizeT $
            traverseDMapWithKeyWithAdjustWithMove
                (\k v -> unLocalizeT $ f k v)
                (coerce dm)
                (coerceEvent edm)
    traverseIntMapWithKeyWithAdjust f im edm =
        LocalizeT $
            traverseIntMapWithKeyWithAdjust
                (\k v -> unLocalizeT $ f k v)
                (coerce im)
                (coerceEvent edm)

instance (PerformEvent t m) => PerformEvent t (LocalizeT t locale m) where
    type Performable (LocalizeT t locale m) = Performable m
    {-# INLINEABLE performEvent_ #-}
    performEvent_ = lift . performEvent_
    {-# INLINEABLE performEvent #-}
    performEvent = lift . performEvent

instance
    (DomBuilder t m, MonadHold t m, MonadFix m) =>
    DomBuilder t (LocalizeT t locale m)
    where
    type DomBuilderSpace (LocalizeT t locale m) = DomBuilderSpace m
    textNode = lift . textNode
    element elementTag cfg (LocalizeT child) = LocalizeT $ element elementTag cfg child
    inputElement = lift . inputElement
    textAreaElement = lift . textAreaElement
    selectElement cfg (LocalizeT child) = LocalizeT $ selectElement cfg child
    placeRawElement = lift . placeRawElement
    wrapRawElement e = lift . wrapRawElement e

instance (Requester t m) => Requester t (LocalizeT t locale m) where
    type Request (LocalizeT t locale m) = Request m
    type Response (LocalizeT t locale m) = Response m
    requesting = lift . requesting
    requesting_ = lift . requesting_

instance (MonadQuery t q m, Monad m) => MonadQuery t q (LocalizeT t locale m) where
    tellQueryIncremental = lift . tellQueryIncremental
    askQueryResult = lift askQueryResult
    queryIncremental = lift . queryIncremental

instance (PrimMonad m) => PrimMonad (LocalizeT t locale m) where
    type PrimState (LocalizeT t locale m) = PrimState m
    primitive = lift . primitive

-- This constraint is because RawDocument is not injective, however, I'm not totally sure it's what we want.
instance
    ( HasDocument m
    , RawDocument
        (DomBuilderSpace (LocalizeT t locale m))
        ~ RawDocument (DomBuilderSpace m)
    ) =>
    HasDocument (LocalizeT t locale m)
    where
    askDocument = lift askDocument

#ifndef ghcjs_HOST_OS
instance MonadJSM m => MonadJSM (LocalizeT t locale m)
#endif

deriving instance
    (HasLocale t locale m) => HasLocale t locale (RequesterT t request response m)

deriving instance
    (HasLocale t locale m) => HasLocale t locale (EventWriterT t w m)

deriving instance (HasLocale t locale m) => HasLocale t locale (QueryT t q m)

deriving instance (EventWriter t w m) => EventWriter t w (LocalizeT t locale m)

deriving instance (HasLocale t locale m) => HasLocale t locale (PostBuildT t m)

deriving instance (HasLocale t locale m) => HasLocale t locale (TriggerEventT t m)

deriving instance
    (HasLocale t locale m) => HasLocale t locale (WithJSContextSingleton x m)

-- | Runs an action that is locale-aware using the given 'Dynamic t locale' as the time-varying locale.
--
-- WARNING: The given 'Dynamic' must be strict as it will be immediately sampled by some widgets.
runLocalize :: Dynamic t locale -> LocalizeT t locale m a -> m a
runLocalize locale (LocalizeT act) = runReaderT act locale

-- | Renders an internationalizable term as simple @Text@ in a known locale.
--
-- The resulting node is dynamic and will change with the locale.
textL ::
    ( DomBuilder t m
    , MonadSample t m
    , HasLocale t locale m
    , HasI18n locale term Text
    ) =>
    term -> m ()
textL term = do
    termTextD <- showLocale term
    termText0 <- sample (current termTextD)

    void $
        textNode
            TextNodeConfig
                { _textNodeConfig_initialContents = termText0
                , _textNodeConfig_setContents = Just (updated termTextD)
                }

-- | Converts an internationalizable term into a @Dynamic t Text@ in a known locale.
--
-- The result is dynamic and will change with the locale.
showLocale ::
    (HasLocale t locale m, HasI18n locale term Text) => term -> m (Dynamic t Text)
showLocale term = do
    localeD <- askLocale
    return $ ffor localeD $ \locale -> localizeWith locale term
