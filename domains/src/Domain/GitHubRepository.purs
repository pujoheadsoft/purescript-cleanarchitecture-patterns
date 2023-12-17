module Domain.GitHubRepository where

import Prelude

import Data.Date (Date)
import Data.Maybe (Maybe)

newtype GitHubRepositories
  = GitHubRepositories (Array GitHubRepository)

newtype GitHubRepository
  = GitHubRepository
  { name :: GitHubRepositoryName
  , url :: GitHubRepositoryUrl
  , owner :: GitHubRepositoryOwner
  , updateDate :: GitHubRepositoryUpdateDate
  }

newtype GitHubRepositoryName = GitHubRepositoryName String
newtype GitHubRepositoryUrl = GitHubRepositoryUrl String
newtype GitHubRepositoryOwner = GitHubRepositoryOwner String
newtype GitHubRepositoryUpdateDate = GitHubRepositoryUpdateDate (Maybe Date)

derive newtype instance showRepositories :: Show GitHubRepositories
derive newtype instance eqRepositories :: Eq GitHubRepositories
derive newtype instance showRepository :: Show GitHubRepository
derive newtype instance eqRepository :: Eq GitHubRepository
derive newtype instance showName :: Show GitHubRepositoryName
derive newtype instance eqName :: Eq GitHubRepositoryName
derive newtype instance showUrl :: Show GitHubRepositoryUrl
derive newtype instance eqUrl :: Eq GitHubRepositoryUrl
derive newtype instance showOwner :: Show GitHubRepositoryOwner
derive newtype instance eqOwner :: Eq GitHubRepositoryOwner
derive newtype instance showDate :: Show GitHubRepositoryUpdateDate
derive newtype instance eqDate :: Eq GitHubRepositoryUpdateDate