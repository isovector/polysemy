{-# LANGUAGE AllowAmbiguousTypes   #-}

{-# OPTIONS_HADDOCK not-home #-}

module Polysemy.Internal.Tactics
  ( Tactics (..)
  , getInitialStateT
  , getEvacuatorT
  , runT
  , bindT
  , pureT
  , liftT
  , runTactics
  , runEvacuator
  , Tactical
  , WithTactics
  ) where

import Polysemy.Internal
import Polysemy.Internal.Effect
import Polysemy.Internal.Union


newtype Evacuator f = Evacuator
  { runEvacuator :: forall x. f x -> Maybe x
  }


------------------------------------------------------------------------------
-- | 'Tactical' is an environment in which you're capable of explicitly
-- threading higher-order effect states. This is provided by the (internal)
-- effect @Tactics@, which is capable of rewriting monadic actions so they run
-- in the correct stateful environment.
--
-- Inside a 'Tactical', you're capable of running 'pureT', 'runT' and 'bindT'
-- which are the main tools for rewriting monadic stateful environments.
--
-- For example, consider trying to write an interpreter for
-- 'Polysemy.Resource.Resource', whose effect is defined as:
--
-- @
-- data 'Polysemy.Resource.Resource' m a where
--   'Polysemy.Resource.Bracket' :: m a -> (a -> m ()) -> (a -> m b) -> 'Polysemy.Resource.Resource' m b
-- @
--
-- Here we have an @m a@ which clearly needs to be run first, and then
-- subsequently call the @a -> m ()@ and @a -> m b@ arguments. In a 'Tactical'
-- environment, we can write the threading code thusly:
--
-- @
-- 'Polysemy.Resource.Bracket' alloc dealloc use -> do
--   alloc'   <- 'runT'  alloc
--   dealloc' <- 'bindT' dealloc
--   use'     <- 'bindT' use
-- @
--
-- where
--
-- @
-- alloc'   ::         'Polysemy.Sem' ('Polysemy.Resource.Resource' ': r) (f a1)
-- dealloc' :: f a1 -> 'Polysemy.Sem' ('Polysemy.Resource.Resource' ': r) (f ())
-- use'     :: f a1 -> 'Polysemy.Sem' ('Polysemy.Resource.Resource' ': r) (f x)
-- @
--
-- The @f@ type here is existential and corresponds to "whatever
-- state the other effects want to keep track of." @f@ is always
-- a 'Functor'.
--
-- @alloc'@, @dealloc'@ and @use'@ are now in a form that can be
-- easily consumed by your interpreter. At this point, simply bind
-- them in the desired order and continue on your merry way.
--
-- We can see from the types of @dealloc'@ and @use'@ that since they both
-- consume a @f a1@, they must run in the same stateful environment. This
-- means, for illustration, any 'Polysemy.State.put's run inside the @use@
-- block will not be visible inside of the @dealloc@ block.
--
-- Power users may explicitly use 'getInitialStateT' and 'bindT' to construct
-- whatever data flow they'd like; although this is usually unnecessary.
type Tactical e m r x = ∀ f. Functor f
                          => Sem (WithTactics e f m r) (f x)

type WithTactics e f m r = Tactics f m (e ': r) ': r

data Tactics f n r m a where
  GetInitialState     :: Tactics f n r m (f ())
  HoistInterpretation :: (a -> n b) -> Tactics f n r m (f a -> Sem r (f b))
  GetEvacuator        :: Tactics f n r m (Evacuator f)


------------------------------------------------------------------------------
-- | Get the stateful environment of the world at the moment the effect @e@ is
-- to be run. Prefer 'pureT', 'runT' or 'bindT' instead of using this function
-- directly.
getInitialStateT :: forall f m r e. Sem (WithTactics e f m r) (f ())
getInitialStateT = send @(Tactics _ m (e ': r)) GetInitialState


------------------------------------------------------------------------------
-- | Lift a value into 'Tactical'.
pureT :: a -> Tactical e m r a
pureT a = do
  istate <- getInitialStateT
  pure $ a <$ istate


------------------------------------------------------------------------------
-- | Run a monadic action in a 'Tactical' environment. The stateful environment
-- used will be the same one that the effect is initally run in. Use 'bindT' if
-- you'd prefer to explicitly manage your stateful environment.
runT
    :: m a
      -- ^ The monadic action to lift. This is usually a parameter in your
      -- effect.
    -> Sem (WithTactics e f m r)
                (Sem (e ': r) (f a))
runT na = do
  istate <- getInitialStateT
  na'    <- bindT (const na)
  pure $ na' istate
{-# INLINE runT #-}


------------------------------------------------------------------------------
-- | Lift a kleisli action into the stateful environment. You can use
-- 'bindT' to get an effect parameter of the form @a -> m b@ into something
-- that can be used after calling 'runT' on an effect parameter @m a@.
bindT
    :: (a -> m b)
       -- ^ The monadic continuation to lift. This is usually a parameter in
       -- your effect.
       --
       -- Continuations lifted via 'bindT' will run in the same environment
       -- which produced the @a@.
    -> Sem (WithTactics e f m r)
                (f a -> Sem (e ': r) (f b))
bindT f = send $ HoistInterpretation f
{-# INLINE bindT #-}


------------------------------------------------------------------------------
-- | Internal function to create first-order interpreter combinators out of
-- higher-order ones.
liftT
    :: forall m f r e a
     . Functor f
    => Sem r a
    -> Sem (WithTactics e f m r) (f a)
liftT m = do
  a <- raise m
  pureT a
{-# INLINE liftT #-}

getEvacuatorT :: forall e f m r. Sem (WithTactics e f m r) (Evacuator f)
getEvacuatorT = send @(Tactics _ m (e ': r)) GetEvacuator


------------------------------------------------------------------------------
-- | Run the 'Tactics' effect.
runTactics
   :: Functor f
   => f ()
   -> (∀ x. f (m x) -> Sem r2 (f x))
   -> (∀ x. f x -> Maybe x)
   -> Sem (Tactics f m r2 ': r) a
   -> Sem r a
runTactics s d v (Sem m) = m $ \u ->
  case decomp u of
    Left x -> liftSem $ hoist (runTactics_b s d v) x
    Right (Yo GetInitialState s' _ y _) ->
      pure $ y $ s <$ s'
    Right (Yo (HoistInterpretation na) s' _ y _) -> do
      pure $ y $ (d . fmap na) <$ s'
    Right (Yo GetEvacuator s' _ y _) -> do
      pure $ y $ Evacuator v <$ s'
{-# INLINE runTactics #-}


runTactics_b
   :: Functor f
   => f ()
   -> (∀ x. f (m x) -> Sem r2 (f x))
   -> (∀ x. f x -> Maybe x)
   -> Sem (Tactics f m r2 ': r) a
   -> Sem r a
runTactics_b = runTactics
{-# NOINLINE runTactics_b #-}

