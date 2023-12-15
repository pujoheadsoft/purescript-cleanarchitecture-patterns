module State.SearchGitHubRepositoryState where

import Network.RemoteData (RemoteData)

type SearchGitHubRepositoryState
  = { searchRepositoryName :: String
    , repositories :: RemoteData String GitHubRepositories
    }

type GitHubRepositories
  = Array GitHubRepository

type GitHubRepository
  = { name :: String
    , url :: String
    , owner :: String
    , updateDate :: String
    }
