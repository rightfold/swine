module Swinec.Anf
  ( -- * Types
    Program (..)
  , Expression (..)
  , Value (..)

    -- * Properties
  , valueType
  ) where

import Swinec.Ast (Constant, Type, constantType)
import Swinec.Name (Dependency, Synthesized)

--------------------------------------------------------------------------------
-- Types

data Program =
  Program
    { programBindings :: [(Synthesized, Expression)]
    , programResult   :: Value }

data Expression :: * where

  ApplyExpression :: Value -> Value -> Expression

  LambdaExpression :: Synthesized -> Type -> Program -> Expression

data Value :: * where

  ConstantValue :: Constant -> Value

  DependencyValue :: Dependency -> Type -> Value

  VariableValue :: Synthesized -> Type -> Value

--------------------------------------------------------------------------------
-- Properties

valueType :: Value -> Type
valueType (ConstantValue constant) = constantType constant
valueType (DependencyValue _ type_) = type_
valueType (VariableValue _ type_) = type_
{-# INLINABLE valueType #-}
