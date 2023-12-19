module Presenter.GitHubRepositoryPresenter where

import Prelude

import Control.Monad.Reader (runReaderT)
import Data.DateTime.Instant (fromDate, toDateTime)
import Data.Either (either)
import Data.Formatter.DateTime (formatDateTime)
import Data.Maybe (Maybe(..), fromMaybe)
import Domain.Error (Error(..))
import Domain.GitHubRepository (GitHubRepositories(..), GitHubRepository(..), GitHubRepositoryName(..), GitHubRepositoryOwner(..), GitHubRepositoryUpdateDate(..), GitHubRepositoryUrl(..))
import Presenter.Port (class GitHubRepositoryPresenterPort, GitHubRepositoryPresenterPortFunction)
import Presenter.Port as Port
import State.SearchGitHubRepositoryState as State
import UseCase.Port (GitHubRepositoryOutputPortFunction)

setRepositories
  :: forall m
   . Monad m
  => GitHubRepositoryPresenterPort m
  => GitHubRepositories
  -> m Unit
setRepositories (GitHubRepositories r) = Port.setRepositories $ convert <$> r

convert :: GitHubRepository -> State.GitHubRepository
convert (GitHubRepository { 
  name: (GitHubRepositoryName n), 
  owner: (GitHubRepositoryOwner o), 
  url: (GitHubRepositoryUrl u), 
  updateDate: (GitHubRepositoryUpdateDate d) }) = 
  let
    date = either (const Nothing) Just =<< format <$> toDate <$> d
  in {
    name: n,
    owner: o,
    url: u,
    updateDate: fromMaybe "-" date
  }
  where
  format = formatDateTime "YYYY/MM/DD"
  toDate = toDateTime <<< fromDate

setLoading
  :: forall m
   . Monad m
  => GitHubRepositoryPresenterPort m
  => Boolean
  -> m Unit
setLoading = Port.setLoading

setErrorMessage
  :: forall m
   . Monad m
  => GitHubRepositoryPresenterPort m
  => Error
  -> m Unit
setErrorMessage (Error e) = Port.setErrorMessage e

gitHubRepositoryPresenterPortFunction
  :: forall m
   . Monad m
  => GitHubRepositoryPresenterPortFunction m
  -> GitHubRepositoryOutputPortFunction m ()
gitHubRepositoryPresenterPortFunction f = {
    setRepositories: run <<< setRepositories,
    setLoading: run <<< setLoading,
    setErrorMessage: run <<< setErrorMessage
  }
  where
  run = flip runReaderT f