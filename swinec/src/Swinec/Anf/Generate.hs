{-# LANGUAGE UndecidableInstances #-}

module Swinec.Anf.Generate
  ( -- * Monad class
    MonadGenerate (..)
  , generateConstant

    -- * Monad transformer
  , GenerateT
  , Error (..)
  ) where

import Control.Lens ((<<+=), at, to, view)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Trans.RWS (RWST)
import Data.HashMap.Strict (HashMap)
import Data.Word (Word64)
import Swinec.Ast (Constant, Type)
import Swinec.Name (Dependency, Identifier, Synthesized (..))

import qualified Control.Monad.Error.Class as Error
import qualified Control.Monad.Writer.Class as Writer
import qualified Swinec.Anf as Anf

--------------------------------------------------------------------------------
-- Monad class

class Monad m => MonadGenerate m where

  generateDependency :: Dependency -> m Anf.Value

  generateApply :: Anf.Value -> Anf.Value -> m Anf.Value

  generateLambda :: Type -> (Anf.Value -> m Anf.Value) -> m Anf.Value

generateConstant :: Applicative m => Constant -> m Anf.Value
generateConstant = pure . Anf.ConstantValue
{-# INLINE generateConstant #-}

--------------------------------------------------------------------------------
-- Monad transformer

newtype GenerateT m a =
  GenerateT (RWST R W S m a)

type R = Context
type W = [(Synthesized, Anf.Expression)]
type S = Word64

data Context =
  Context
    { contextDependencies :: HashMap Dependency Type }

data Error
  = UnknownDependency Dependency

fresh :: Monad m => GenerateT m Synthesized
fresh = GenerateT $ Synthesized <$> (id <<+= 1)

bind :: Monad m => Anf.Expression -> Type -> GenerateT m Anf.Value
bind expression type_ = do
  name <- fresh
  GenerateT $ Writer.tell [(name, expression)]
  pure $ Anf.VariableValue name type_

instance (MonadError Error m) => MonadGenerate (GenerateT m) where

  generateDependency dependency = GenerateT $ do
    type_ <- view $ to contextDependencies . at dependency
    maybe (Error.throwError (UnknownDependency dependency))
          (pure . Anf.DependencyValue dependency)
          type_

  generateApply function argument = GenerateT $ do
    let functionType = Anf.valueType function
    let argumentType = Anf.valueType argument
    type_ <- _
    bind (Anf.ApplyExpression function argument) type_

  generateLambda parameterType body = GenerateT $
    _

--------------------------------------------------------------------------------
-- Instances

deriving newtype instance Functor m => Functor (GenerateT m)
deriving newtype instance Monad m => Applicative (GenerateT m)
deriving newtype instance Monad m => Monad (GenerateT m)
