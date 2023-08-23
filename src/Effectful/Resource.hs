{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Resource management via 'R.MonadResource'.
module Effectful.Resource
  ( -- * Effect
    Resource

    -- ** Handlers
  , runResource

    -- * Registering and releasing resources
  , allocateEff
  , allocateEff_
  , registerEff
  , releaseEff
  , unprotectEff
  , R.allocate
  , R.allocate_
  , R.register
  , R.release
  , R.unprotect

    -- * Internal state
  , R.InternalState
  , getInternalState
  , runInternalState
  , R.createInternalState
  , R.closeInternalState

    -- * Re-exports
  , R.ReleaseKey
  , R.ResourceCleanupException(..)
  ) where

import Control.Exception
import qualified Control.Monad.Trans.Resource as R
import qualified Control.Monad.Trans.Resource.Internal as RI

import Effectful
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive

-- | Provide the ability to use the 'R.MonadResource' instance of 'Eff'.
data Resource :: Effect

type instance DispatchOf Resource = Static WithSideEffects
newtype instance StaticRep Resource = Resource R.InternalState

-- | Run the resource effect.
runResource :: IOE :> es => Eff (Resource : es) a -> Eff es a
runResource m = unsafeEff $ \es0 -> do
  istate <- R.createInternalState
  mask $ \unmask -> do
    es <- consEnv (Resource istate) dummyRelinker es0
    a <- unmask (unEff m es) `catch` \e -> do
      unconsEnv es
      RI.stateCleanupChecked (Just e) istate
      throwIO e
    unconsEnv es
    RI.stateCleanupChecked Nothing istate
    pure a

----------------------------------------
-- Registering and releasing resources

-- | A variant of 'R.allocate` adjusted to work in the 'Eff' monad.
--
-- /Note:/ the @release@ action will run a cloned environment, so any changes it
-- makes to thread local data will not be visible outside of it.
allocateEff
  :: Resource :> es
  => Eff es a -- ^ allocate
  -> (a -> Eff es ()) -- ^ free resource
  -> Eff es (R.ReleaseKey, a)
allocateEff acquire release = do
  istate <- getInternalState
  unsafeEff $ \es0 -> mask_ $ do
    a <- unEff acquire es0
    -- we need to clone original env for release action
    -- because it will be called when original env already unconsed
    es1 <- cloneEnv es0
    key <- RI.register' istate $ unEff (release a) es1
    pure (key, a)

-- | A variant of 'R.allocate_' adjusted to work in the 'Eff' monad.
--
-- /Note:/ the @release@ action will run a cloned environment, so any changes it
-- makes to thread local data will not be visible outside of it.
allocateEff_
  :: Resource :> es
  => Eff es a -- ^ allocate
  -> Eff es () -- ^ free resource
  -> Eff es R.ReleaseKey
allocateEff_ a = fmap fst . allocateEff a . const

-- | A variant of 'R.register' adjusted to work in the 'Eff' monad.
--
-- /Note:/ the @release@ action will run a cloned environment, so any changes it
-- makes to thread local data will not be visible outside of it.
registerEff :: Resource :> es => Eff es () -> Eff es R.ReleaseKey
registerEff release = do
  istate <- getInternalState
  unsafeEff $ \es0 -> do
    -- we need to clone original env for release action
    -- because it will be called when original env already unconsed
    es1 <- cloneEnv es0
    RI.register' istate $ unEff release es1

-- | A variant of 'R.release' adjusted to work in the 'Eff' monad.
releaseEff :: Resource :> es => R.ReleaseKey -> Eff es ()
releaseEff = unsafeEff_ . R.release

-- | A variant of 'R.unprotect' adjusted to work in the 'Eff' monad.
--
-- /Note:/ the @release@ action returned will run a clone of the environment it
-- was registered in, so the effect row @es'@ will be ignored.
unprotectEff :: (Resource :> es, Resource :> es') => R.ReleaseKey -> Eff es (Maybe (Eff es' ()))
unprotectEff = unsafeEff_ . fmap (fmap unsafeEff_) . R.unprotect

----------------------------------------
-- Internal state

-- | Get the 'R.InternalState' of the current 'Resource' effect.
getInternalState :: Resource :> es => Eff es R.InternalState
getInternalState = do
  Resource istate <- getStaticRep
  pure istate

-- | Run the 'Resource' effect with existing 'R.InternalState'.
--
-- /Note:/ the 'R.InternalState' will not be closed at the end.
runInternalState :: IOE :> es => R.InternalState -> Eff (Resource : es) a -> Eff es a
runInternalState istate = evalStaticRep (Resource istate)

----------------------------------------
-- Orphan instance

instance (IOE :> es, Resource :> es) => R.MonadResource (Eff es) where
  liftResourceT (RI.ResourceT m) = unsafeEff $ \es -> do
    getEnv es >>= \(Resource istate) -> m istate
