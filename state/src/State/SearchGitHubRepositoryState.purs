module State.SearchGitHubRepositoryState where

import Data.Either (Either)


type SearchGitHubRepositoryState
  = { searchRepositoryName :: String
    , repositories :: Either ErrorMessage GitHubRepositories
    , isLoading :: Boolean
    }

type GitHubRepositories
  = Array GitHubRepository

type GitHubRepository
  = { name :: String
    , url :: String
    , owner :: String
    , updateDate :: String
    }

type ErrorMessage = String