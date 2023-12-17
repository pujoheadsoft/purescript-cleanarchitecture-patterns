module Gateway.GitHubRepositoryGateway
  ( searchByName
  )
  where

import Prelude

import Data.Date (Date, canonicalDate)
import Data.Either (Either(..), either)
import Data.Enum (class BoundedEnum, toEnum)
import Data.Int (fromString)
import Data.JSDate (JSDate, parse, toDate)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.String.Regex (regex, replace)
import Data.Traversable (traverse)
import Domain.Error (Error(..))
import Domain.GitHubRepository (GitHubRepositories(..), GitHubRepository(..), GitHubRepositoryName(..), GitHubRepositoryOwner(..), GitHubRepositoryUpdateDate(..), GitHubRepositoryUrl(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Gateway.Port (class GitHubRepositoryGatewayPort, SearchResult)
import Gateway.Port as Port

searchByName
  :: forall m
   . MonadAff m
  => GitHubRepositoryGatewayPort m
  => GitHubRepositoryName
  -> m (Either Error GitHubRepositories)
searchByName (GitHubRepositoryName name) = do
  Port.searchByName name >>= either
    (pure <<< Left <<< Error)
    \output -> do
      r <- traverse toRepository output
      (pure <<< Right <<< GitHubRepositories) r

  where
  toRepository :: SearchResult -> m GitHubRepository
  toRepository result = do
    d <- dateFromString result.updated_at
    pure $ GitHubRepository {
      name: GitHubRepositoryName result.full_name,
      url: GitHubRepositoryUrl result.html_url,
      owner: GitHubRepositoryOwner result.owner.login,
      updateDate: GitHubRepositoryUpdateDate d
    }

  dateFromString :: String -> m (Maybe Date)
  dateFromString s = toDate <$> (liftEffect $ parse s)
