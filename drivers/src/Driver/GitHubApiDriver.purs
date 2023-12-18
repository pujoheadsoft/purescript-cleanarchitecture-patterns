module Driver.GitHubApiDriver where

import Prelude

import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web (Error, get, printError)
import Data.Array (cons, snoc)
import Data.Either (Either(..))
import Data.List.NonEmpty (foldl, foldr, toList)
import Data.String (joinWith)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Foreign (MultipleErrors, renderForeignError)
import Gateway.Port (ErrorMessage, GitHubRepositoryGatewayPortFunction, SearchResults)
import Simple.JSON (readJSON)

gitHubRepositoryGatewayPortFunction :: forall m. MonadAff m => GitHubRepositoryGatewayPortFunction m
gitHubRepositoryGatewayPortFunction = { searchByName }

searchByName :: forall m. MonadAff m => String -> m (Either ErrorMessage SearchResults)
searchByName s = pure $ Right []

request :: Aff (Either ErrorMessage SearchResults)
request = do
  res <- get ResponseFormat.string "https://httpbin.org/ip"
  case res of
    Left e -> pure $ Left $ printError e
    Right r -> do
      case toSearchResult r.body of
        Left e -> pure $ Left $ joinWith "\n" $ foldl snoc [] (renderForeignError <$> e)
        Right v -> pure $ Right v

toSearchResult :: String -> Either MultipleErrors SearchResults
toSearchResult = readJSON