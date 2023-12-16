module UseCase.SearchGitHubRepositoryUseCase where

import Prelude

import Data.Either (either)
import Domain.GitHubRepository (GitHubRepositoryName)
import UseCase.Port (class GitHubRepositoryPort, class GitHubRepositoryPresenterPort, searchByName, setErrorMessage, setLoading, setRepositories)

searchRepositoryBy
  :: forall m
   . GitHubRepositoryPort m
  => GitHubRepositoryPresenterPort m
  => GitHubRepositoryName
  -> m Unit
searchRepositoryBy name = do
  setLoading true
  searchByName name >>= either setErrorMessage setRepositories
  setLoading false
