module Presenter.GitHubRepositoryPresenter where

import Prelude

import Control.Alt ((<|>))
import Data.Date (day, month, year)
import Data.DateTime.Instant (fromDate, toDateTime)
import Data.Either (either)
import Data.Formatter.DateTime (formatDateTime)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Domain.GitHubRepository (GitHubRepositories(..), GitHubRepository(..), GitHubRepositoryName(..), GitHubRepositoryOwner(..), GitHubRepositoryUpdateDate(..), GitHubRepositoryUrl(..))
import Presenter.Port (class GitHubRepositoryPresenterPort)
import Presenter.Port as Port
import State.SearchGitHubRepositoryState as State

setRepositories
  :: forall m
   . Monad m
  => GitHubRepositoryPresenterPort m
  => GitHubRepositories
  -> m Unit
setRepositories (GitHubRepositories r) = do
  let repositories = convert <$> r
  Port.setRepositories repositories

convert :: GitHubRepository -> State.GitHubRepository
convert (GitHubRepository { 
  name: (GitHubRepositoryName n), 
  owner: (GitHubRepositoryOwner o), 
  url: (GitHubRepositoryUrl u), 
  updateDate: (GitHubRepositoryUpdateDate date) }) = 
  let 
    x = case formatDateTime "YYYY/MM/DD" <$> (toDateTime <$> fromDate <$> date) of 
      Nothing -> ""
      Just j -> either
        (\_ -> "")
        (\r -> r)
        j
  in {
    name: n,
    owner: o,
    url: u,
    updateDate: x
  }