module Gateway.GitHubRepositoryGateway where

import Prelude

import Data.Either (Either(..))
import Domain.Error (Error)
import Domain.GitHubRepository (GitHubRepositories(..), GitHubRepositoryName)

searchByName
  :: forall m
   . Monad m
  => GitHubRepositoryName
  -> m (Either Error GitHubRepositories)
searchByName name = pure $ Right $ GitHubRepositories []