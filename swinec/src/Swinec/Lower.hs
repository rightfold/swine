{-# LANGUAGE TemplateHaskell #-}

module Swinec.Lower
  ( -- * Infrastructure
    MonadLower
  , Context (..)
  , contextVariables

    -- * Lowering
  , lower
  ) where

import Control.Lens ((?~), at, makeLenses, view)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Reader.Class (MonadReader)
import Data.HashMap.Strict (HashMap)
import Swinec.Anf.Generate (MonadGenerate)
import Swinec.Ast (Expression (..))
import Swinec.Name (Identifier)

import qualified Control.Monad.Error.Class as Error
import qualified Control.Monad.Reader.Class as Reader
import qualified Swinec.Anf as Anf
import qualified Swinec.Anf.Generate as Anf

--------------------------------------------------------------------------------
-- Infrastructure

type MonadLower m =
  ( MonadReader Context m
  , MonadError () m
  , MonadGenerate m )

data Context =
  Context
    { _contextVariables :: HashMap Identifier Anf.Value }

$(makeLenses ''Context)

--------------------------------------------------------------------------------
-- Lowering

lower :: MonadLower m => Expression -> m Anf.Value

lower (ConstantExpression constant) =
  Anf.generateConstant constant

lower (DependencyExpression dependency) =
  Anf.generateDependency dependency

lower (VariableExpression variable) =
  view (contextVariables . at variable)
    >>= maybe (Error.throwError ()) pure

lower (ApplyExpression function argument) = do
  function' <- lower function
  argument' <- lower argument
  Anf.generateApply function' argument'

lower (LambdaExpression parameter parameterType body) =
  Anf.generateLambda parameterType $
    \argument ->
      Reader.local (contextVariables . at parameter ?~ argument) $
        lower body

lower (LetExpression variable definition body) = do
  definition' <- lower definition
  Reader.local (contextVariables . at variable ?~ definition') $
    lower body
