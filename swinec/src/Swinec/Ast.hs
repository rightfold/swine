module Swinec.Ast
  ( -- * Types
    Constant (..)
  , Expression (..)
  , Path (..)
  , Type (..)

    -- * Properties
  , constantType
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Swinec.Name (Dependency, Identifier, IntrinsicType)

--------------------------------------------------------------------------------
-- Types

data Constant :: * where

  -- |
  -- A UTF-8 constant is a string encoded in UTF-8.
  Utf8Constant :: Text -> Constant

data Expression :: * where

  -- |
  -- A constant expression evaluates to a constant.
  ConstantExpression :: Constant -> Expression

  -- |
  -- A dependency expression refers to another translation unit, and evaluates
  -- to the value of that translation unit. See 'Dependency' for more
  -- information about dependencies.
  DependencyExpression :: Dependency -> Expression

  -- |
  -- A variable expression evaluates to the value bound to its identifier in
  -- the context. The name is bound by a lambda expression.
  VariableExpression :: Identifier -> Expression

  -- |
  -- An apply expression evaluates a function body given a value for the
  -- parameter of the function.
  ApplyExpression :: Expression -> Expression -> Expression

  -- |
  -- A lambda expression evaluates to a function that inherits the context for
  -- the evaluation of its body. The body is evaluated when the function is
  -- applied.
  LambdaExpression :: Identifier -> Type -> Expression -> Expression

  -- |
  -- A let expression evaluates to the value of its body, with the value of its
  -- definition bound to an identifier.
  LetExpression :: Identifier -> Expression -> Expression -> Expression

data Path :: * where

  -- |
  -- A constant path refers to a constant. Particularly useful for singleton
  -- types.
  ConstantPath :: Constant -> Path

  -- |
  -- A dependency path refers to a another translation unit. See
  -- 'Dependency' for more information about dependencies.
  DependencyPath :: Dependency -> Path

data Type :: * where

  -- |
  -- An intrinsic type.
  IntrinsicType :: IntrinsicType -> Type

  -- |
  -- The singleton type of a path is the type that contains only that path as
  -- its inhabitant.
  SingletonType :: Path -> Type

  -- |
  -- Apply a type constructor to a type.
  ApplyType :: Type -> Type -> Type

--------------------------------------------------------------------------------
-- Properties

constantType :: Constant -> Type
constantType = SingletonType . ConstantPath
{-# INLINE constantType #-}

--------------------------------------------------------------------------------
-- Instances

deriving anyclass instance FromJSON Constant
deriving anyclass instance ToJSON Constant
deriving stock instance Eq Constant
deriving stock instance Generic Constant
deriving stock instance Show Constant

deriving stock instance Eq Expression
deriving stock instance Show Expression

deriving anyclass instance FromJSON Path
deriving anyclass instance ToJSON Path
deriving stock instance Eq Path
deriving stock instance Generic Path
deriving stock instance Show Path

deriving anyclass instance FromJSON Type
deriving anyclass instance ToJSON Type
deriving stock instance Eq Type
deriving stock instance Generic Type
deriving stock instance Show Type
