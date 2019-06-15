{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE UndecidableInstances  #-}

module Polysemy.Internal.Union
  ( Union (..)
  , Yo (..)
  , liftYo
  , Member
  -- * Building Unions
  , inj
  , weaken
  -- * Using Unions
  , decomp
  , prj
  , extract
  , absurdU
  , decompCoerce
  -- * Witnesses
  , Elem (..)
  ) where

import Control.Monad
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Kind
import Data.Type.Equality
import Polysemy.Internal.Effect

#ifdef ERROR_MESSAGES
import Polysemy.Internal.CustomErrors
#endif

-- TODO: move into 'CustomErrors'
import GHC.TypeLits


------------------------------------------------------------------------------
-- | Kind of effect type constructor.
type Eff = (Type -> Type) -> Type -> Type

------------------------------------------------------------------------------
-- | An extensible, type-safe union. The @r@ type parameter is a type-level
-- list of effects, any one of which may be held within the 'Union'.
data Union :: [Eff] -> (Type -> Type) -> Type -> Type where
  Union :: Elem r e  -- ^ A proof that the effect is actually in @r@.
        -> Yo e m a  -- ^ The effect to wrap. The functions 'prj' and 'decomp'
                     -- can help retrieve this value later.
        -> Union r m a

------------------------------------------------------------------------------
-- | Nat describing position of type in type-level list.
data Elem :: [Eff] -> Eff -> Type where
  Here ::              Elem (e ': es) e
  In   :: Elem es e -> Elem (d ': es) e

instance TestEquality (Elem r) where
  testEquality Here   Here   = Just Refl
  testEquality (In l) (In r) = testEquality l r
  testEquality _      _      = Nothing
  {-# INLINE testEquality #-}

------------------------------------------------------------------------------
data Yo e m a where
  Yo :: Functor f
     => e m a
     -> f ()
     -> (forall x. f (m x) -> n (f x))
     -> (f a -> b)
     -> (forall x. f x -> Maybe x)
     -> Yo e n b

instance Functor (Yo e m) where
  fmap f (Yo e s d f' v) = Yo e s d (f . f') v
  {-# INLINE fmap #-}

instance Effect (Yo e) where
  weave s' d v' (Yo e s nt f v) =
    Yo e (Compose $ s <$ s')
         (fmap Compose . d . fmap nt . getCompose)
         (fmap f . getCompose)
         (v <=< v' . getCompose)
  {-# INLINE weave #-}

  hoist = defaultHoist
  {-# INLINE hoist #-}

------------------------------------------------------------------------------
liftYo :: Functor m => e m a -> Yo e m a
liftYo e = Yo e (Identity ())
                (fmap Identity . runIdentity)
                runIdentity
                (Just . runIdentity)
{-# INLINE liftYo #-}

instance Functor (Union r m) where
  fmap f (Union p a) = Union p $ fmap' f a
  {-# INLINE fmap #-}

instance Effect (Union r) where
  weave s f v (Union p a) = Union p $ weave s f v a
  {-# INLINE weave #-}

  hoist f (Union p a) = Union p $ hoist f a
  {-# INLINE hoist #-}

------------------------------------------------------------------------------
-- | A proof that the effect @e@ is available somewhere inside of the effect
-- stack @r@.
type Member e r = Member' e r  -- TODO: check stuckness and empty union

------------------------------------------------------------------------------
class Member' (e :: Eff) (r :: [Eff]) where
  membership :: Elem r e

instance {-# OVERLAPPING #-}
         Member' e (e ': es) where
  membership = Here
  {-# INLINE membership #-}

instance Member' e es
      => Member' e (d ': es) where
  membership = In membership
  {-# INLINE membership #-}

------------------------------------------------------------------------------
-- | Lift an effect @e@ into a 'Union' capable of holding it.
inj :: (Member' e r, Functor m) => e m a -> Union r m a
inj = Union membership . liftYo
{-# INLINE inj #-}

------------------------------------------------------------------------------
-- | Attempt to take an @e@ effect out of a 'Union'.
prj :: forall e r m a. Member' e r => Union r m a -> Maybe (Yo e m a)
prj (Union p a) = (\Refl -> a) <$> p `testEquality` membership @e @r
{-# INLINE prj #-}

------------------------------------------------------------------------------
-- TODO: move into 'CustomErrors', make more informative
type family NotMember (e :: Eff) (es :: [Eff]) :: k where
  NotMember e '[] = TypeError
    (     'Text "Attempt to make '" ':<>: 'ShowType e ':<>: 'Text "' member"
    ':<>: 'Text " of empty union"
    )
  NotMember e (e ': es) = TypeError
    (     'Text "'" ':<>: 'ShowType e ':<>: 'Text "' is not member of '"
    ':<>: 'ShowType (e ': es) ':<>: 'Text "'"
    )

------------------------------------------------------------------------------
-- | Decompose a 'Union'. Either this union contains an effect @e@---the head
-- of the @r@ list---or it doesn't.
decomp :: Union (e ': r) m a -> Either (Union r m a) (Yo e m a)
decomp (Union Here   a) = Right a
decomp (Union (In p) a) = Left $ Union p a
{-# INLINE decomp #-}


------------------------------------------------------------------------------
-- | Retrieve the last effect in a 'Union'.
extract :: Union '[e] m a -> Yo e m a
extract (Union Here   a) = a
extract (Union (In p) _) = case p of {}
{-# INLINE extract #-}


------------------------------------------------------------------------------
-- | An empty union contains nothing, so this function is uncallable.
absurdU :: Union '[] m a -> b
absurdU (Union p _) = case p of {}

------------------------------------------------------------------------------
-- | Weaken a 'Union' so it is capable of storing a new sort of effect.
weaken :: Union r m a -> Union (e ': r) m a
weaken (Union p a) = Union (In p) a
{-# INLINE weaken #-}

------------------------------------------------------------------------------
-- | Like 'decomp', but allows for a more efficient
-- 'Polysemy.Interpretation.reinterpret' function.
decompCoerce
    :: Union (e ': r) m a
    -> Either (Union (f ': r) m a) (Yo e m a)
decompCoerce (Union Here   a) = Right a
decompCoerce (Union (In p) a) = Left $ Union (In p) a
{-# INLINE decompCoerce #-}
