module Polysemy.Membership
  ( -- * Witnesses
    ElemOf (..)
  , membership
  , sameMember
  -- * Checking membership
  , KnownRow
  , tryMembership
  -- * Using membership
  , subsumeUsing
  , interceptUsing
  , interceptUsingNew
  , interceptUsingH
  ) where

import Polysemy.Internal
import Polysemy.Internal.Combinators
import Polysemy.Internal.Union
