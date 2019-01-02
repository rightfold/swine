module Swinec.Name
  ( -- * Basic names
    Identifier (..)
  , Synthesized (..)
  , Dependency (..)

    -- * Intrinsics
  , IntrinsicType (..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Basic names

newtype Identifier =
  Identifier Text

newtype Synthesized =
  Synthesized Word64

-- |
-- A dependency refers to another translation unit.
--
-- A mapping from dependency identifiers to object filenames is given to the
-- compiler separately from the source code. To keep the AST simple, dependency
-- identifiers are looked up immediately while parsing, resulting in
-- 'Dependency' values instead.
--
-- Determining whether two translation units refer to the same translation unit
-- as a dependency is a tricky problem, but necessary for path-dependent types
-- to work correctly. The solution used in the compiler is the simple one: use
-- the object filename as the dependency. This works, based on two assumptions
-- that hold independently of time:
--
--  1. Two different object files do not have the same filename.
--  2. The same object file always has the same filename.
--
-- It is up to the build system to ensure that these assumptions hold. If you
-- use Nix, and the object filenames are Nix store paths, you'll be fine.
--
-- Indeed, 'Dependency' is just a wrapper around the object filename.
newtype Dependency =
  Dependency Text

--------------------------------------------------------------------------------
-- Intrinsics

data IntrinsicType :: * where

  -- |
  -- The type of functions.
  FunctionIntrinsicType :: IntrinsicType

--------------------------------------------------------------------------------
-- Instances

deriving anyclass instance FromJSON Identifier
deriving anyclass instance ToJSON Identifier
deriving newtype instance Hashable Identifier
deriving stock instance Eq Identifier
deriving stock instance Generic Identifier
deriving stock instance Ord Identifier
deriving stock instance Show Identifier

deriving anyclass instance FromJSON Synthesized
deriving anyclass instance ToJSON Synthesized
deriving newtype instance Hashable Synthesized
deriving stock instance Eq Synthesized
deriving stock instance Generic Synthesized
deriving stock instance Ord Synthesized
deriving stock instance Show Synthesized

deriving anyclass instance FromJSON Dependency
deriving anyclass instance ToJSON Dependency
deriving newtype instance Hashable Dependency
deriving stock instance Eq Dependency
deriving stock instance Generic Dependency
deriving stock instance Ord Dependency
deriving stock instance Show Dependency

deriving anyclass instance FromJSON IntrinsicType
deriving anyclass instance Hashable IntrinsicType
deriving anyclass instance ToJSON IntrinsicType
deriving stock instance Eq IntrinsicType
deriving stock instance Generic IntrinsicType
deriving stock instance Ord IntrinsicType
deriving stock instance Show IntrinsicType
