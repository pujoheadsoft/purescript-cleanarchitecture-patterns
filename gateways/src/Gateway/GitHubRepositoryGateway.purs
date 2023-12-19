module Gateway.GitHubRepositoryGateway
  ( searchByName
  , gitHubRepositoryPortFunction
  )
  where

import Prelude

import Control.Monad.Reader (runReaderT)
import Data.Date (Date)
import Data.Either (Either(..), either)
import Data.JSDate (parse, toDate)
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Domain.Error (Error(..))
import Domain.GitHubRepository (GitHubRepositories(..), GitHubRepository(..), GitHubRepositoryName(..), GitHubRepositoryOwner(..), GitHubRepositoryUpdateDate(..), GitHubRepositoryUrl(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Gateway.Port (class GitHubRepositoryGatewayPort, SearchResult, GitHubRepositoryGatewayPortFunction)
import Gateway.Port as Port
import UseCase.Port (GitHubRepositoryPortFunction)

searchByName
  :: forall m
   . MonadAff m
  => GitHubRepositoryGatewayPort m
  => GitHubRepositoryName
  -> m (Either Error GitHubRepositories)
searchByName (GitHubRepositoryName name) = do
  Port.searchByName name >>= either
    (pure <<< Left <<< Error)
    (pure <<< Right <<< GitHubRepositories <=< traverse toRepository <<< _.items)

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


gitHubRepositoryPortFunction
  :: forall m
   . MonadAff m
  => GitHubRepositoryGatewayPortFunction m
  -> GitHubRepositoryPortFunction m ()
gitHubRepositoryPortFunction f = {
    searchByName: run <<< searchByName
  }
  where
  run = flip runReaderT f