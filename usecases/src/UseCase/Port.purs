module UseCase.Port where

import Prelude

import Control.Monad.Reader (ReaderT)
import Data.Either (Either)
import Data.ReaderTEtaConversionTransformer (readerT)
import Domain.Error (Error)
import Domain.GitHubRepository (GitHubRepositories, GitHubRepositoryName)
import Type.Equality (class TypeEquals)

class Monad m <= GitHubRepositoryPort m where
  searchByName :: GitHubRepositoryName -> m (Either Error GitHubRepositories)

type GitHubRepositoryPortFunction m r = { 
  searchByName :: GitHubRepositoryName -> m (Either Error GitHubRepositories) | r
}

instance portReaderT ::
  (Monad m, TypeEquals f (GitHubRepositoryPortFunction m r)) =>
  GitHubRepositoryPort (ReaderT f m) where
  searchByName = readerT _.searchByName

class Monad m <= GitHubRepositoryOutputPort m where
  setRepositories :: GitHubRepositories -> m Unit
  setLoading :: Boolean -> m Unit
  setErrorMessage :: Error -> m Unit

type GitHubRepositoryOutputPortFunction m r = {
  setRepositories :: GitHubRepositories -> m Unit,
  setLoading :: Boolean -> m Unit,
  setErrorMessage :: Error -> m Unit
  | r
}

instance outputPortReaderT ::
  (Monad m, TypeEquals f (GitHubRepositoryOutputPortFunction m r)) =>
  GitHubRepositoryOutputPort (ReaderT f m) where
  setRepositories = readerT _.setRepositories
  setLoading = readerT _.setLoading
  setErrorMessage = readerT _.setErrorMessage
