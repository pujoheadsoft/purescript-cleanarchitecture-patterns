module Presenter.Port where

import Prelude

import Control.Monad.Reader (ReaderT)
import Data.ReaderTEtaConversionTransformer (readerT)
import State.SearchGitHubRepositoryState (GitHubRepositories, ErrorMessage)
import Type.Equality (class TypeEquals)

class Monad m <= GitHubRepositoryPresenterPort m where
  setRepositories :: GitHubRepositories -> m Unit
  setLoading :: Boolean -> m Unit
  setErrorMessage :: ErrorMessage -> m Unit

type GitHubRepositoryPresenterPortFunction m = { 
  setRepositories :: GitHubRepositories -> m Unit,
  setLoading :: Boolean -> m Unit,
  setErrorMessage :: ErrorMessage -> m Unit
}

instance instancePortReaderT ::
  (Monad m, TypeEquals f (GitHubRepositoryPresenterPortFunction m)) =>
  GitHubRepositoryPresenterPort (ReaderT f m) where
  setRepositories = readerT _.setRepositories
  setLoading = readerT _.setLoading
  setErrorMessage = readerT _.setErrorMessage