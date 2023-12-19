module UseCase.SearchGitHubRepositoryUseCase where

import Prelude

import Data.Either (either)
import Domain.GitHubRepository (GitHubRepositoryName)
import UseCase.Port (class GitHubRepositoryPort, class GitHubRepositoryOutputPort, searchByName, setErrorMessage, setLoading, setRepositories)

execute
  :: forall m
   . GitHubRepositoryPort m
  => GitHubRepositoryOutputPort m
  => GitHubRepositoryName
  -> m Unit
execute name = do
  setLoading true
  searchByName name >>= either setErrorMessage setRepositories
  setLoading false
