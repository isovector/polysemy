{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Ok where

import qualified Control.Exception as X
import           Control.Monad.Trans
import qualified Control.Monad.Trans.Except as E
import           Control.Monad.Trans.State hiding (State (..), runState)
import           Data.Functor.Compose
import           Data.Functor.Coyoneda
import           Data.Kind
import           Data.OpenUnion.Internal
import           Data.Void
import           Unsafe.Coerce

newtype Lift m (n :: * -> *) a = Lift
  { unLift :: m a
  }

newtype Eff r a = Eff
  { runEff :: forall m. Monad m => (Union r (Eff r) ~> m) -> m a
  }

usingEff :: Monad m => (Union r (Eff r) ~> m) -> Eff r ~> m
usingEff k e = runEff e k

instance Functor (Eff f) where
  fmap f (Eff m) = Eff $ \k -> fmap f $ m k
  {-# INLINE fmap #-}


instance Applicative (Eff f) where
  pure a = Eff $ const $ pure a
  {-# INLINE pure #-}

  Eff f <*> Eff a = Eff $ \k -> f k <*> a k
  {-# INLINE (<*>) #-}


instance Monad (Eff f) where
  return = pure
  {-# INLINE return #-}

  Eff ma >>= f = Eff $ \k -> do
    z <- ma k
    runEff (f z) k
  {-# INLINE (>>=) #-}





voidIt :: Union '[] m a -> Void
voidIt = error "it's a void you dip"


-- -- extract :: Functor m => Union '[Lift m] n a -> m a
-- -- extract u = case decomp u of
-- --   Right (FreeYo (Lift e) f nt) -> fmap f e
-- --   Left u -> absurd $ voidIt u


-- -- decomp
-- --     :: Union (e ': r) m a
-- --     -> Either (Union r m a)
-- --               (FreeYo e m a)
-- -- decomp (Union 0 (FreeYo e m a)) = Right $ FreeYo (unsafeCoerce e) m a
-- -- decomp (Union n e) = Left $ Union (n - 1) e


data State s (m :: * -> *) a where
  Get :: State s m s
  Put :: s -> State s m ()


data Error e (m :: * -> *) a where
  Throw :: e -> Error e m a


data Scoped (m :: * -> *) a where
  Scoped :: m () -> m a -> Scoped m a


foo :: (Member (State String) r, Member (Error Bool) r, Member Scoped r, Member (Lift IO) r) => Eff r ()
foo = send $ Scoped (sendM $ putStrLn "DYING") $ do
  send Get >>= sendM . putStrLn
  send $ Put "works"
  send Get >>= sendM . putStrLn
  send $ Put "done"
  send $ Throw False
  sendM $ putStrLn "inside"


sendM :: Member (Lift m) r => m a -> Eff r a
sendM m = send $ Lift m


runState :: s -> Eff (State s ': r) a -> Eff r a
runState s (Eff m) = Eff $ \k -> flip evalStateT s $ m $ \u ->
  case decomp u of
    Left  x -> do
      s' <- get
      lift $ usingEff k $ liftEff $ hoist (runState s') x
    Right (FreeYo Get _ f)      -> fmap f get
    Right (FreeYo (Put s') _ f) -> fmap f $ put s'


-- runError :: Eff (Error e ': r) a -> Eff r (Either e a)
-- runError (Eff m) = Eff $ \k -> E.runExceptT $ m $ \u ->
--   case decomp u of
--     Left  x -> E.ExceptT $ usingEff k $ liftEff $ foist Right runError x
--     Right (FreeYo (Throw e) _ _) -> E.throwE e



-- runScoped :: Member (Lift IO) r => (Eff r ~> IO) -> Eff (Scoped ': r) a -> Eff r a
-- runScoped end (Eff m) = Eff $ \k -> m $ \u ->
--   case decomp u of
--     Left x -> k $ hoist (runScoped end) x
--     Right (FreeYo (Scoped final first) nt f) -> do
--       usingEff k $ sendM $ X.bracket
--         (end $ runScoped end $ nt $ first)
--         (\a -> end $ runScoped end $ nt $ final)
--         undefined
--       -- usingEff k $ runScoped end $ nt $ first <* final


-- foist :: Functor n => (forall x. x -> f x) -> (m a -> n (f a)) -> Union r m a -> Union r n (f a)
-- foist p f (Union w (FreeYo e nt k)) = Union w $ FreeYo e
--   (\s z -> f $ nt s z)
--   (p . k)


runM :: Monad m => Eff '[Lift m] a -> m a
runM (Eff m) = m $ \(extract -> FreeYo (Lift e) _ f) -> fmap f e


-- main :: IO ()
-- main = (print =<<) . runM . runScoped runM . runError @Bool . runState "first" $ foo


liftEff :: Union r (Eff r) a -> Eff r a
liftEff u = Eff $ \k -> k u


send :: Member e r => e (Eff r) a -> Eff r a
send e = Eff $ \k -> k $ inj e


weaken :: Union r m a -> Union (e ': r) m a
weaken (Union w e) = Union (w + 1) e

