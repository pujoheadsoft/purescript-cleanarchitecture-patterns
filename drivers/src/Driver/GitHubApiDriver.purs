module Driver.GitHubApiDriver where

import Prelude

import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web (get, printError)
import Data.Array (snoc)
import Data.Either (Either(..), either)
import Data.List.NonEmpty (NonEmptyList, foldl)
import Data.String (joinWith)
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
  get ResponseFormat.string "https://httpbin.org/ip" >>= either
    (pure <<< Left <<< printError)
    \r -> toSearchResult r.body #
      either
        (\e -> pure <<< Left <<< joinWith "\n" $ renderForeignError <$> toArray e)
        (pure <<< Right)
        
  where
  toArray :: forall a. NonEmptyList a -> Array a
  toArray = foldl snoc []

toSearchResult :: String -> Either MultipleErrors SearchResults
toSearchResult = readJSON