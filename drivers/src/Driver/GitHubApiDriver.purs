module Driver.GitHubApiDriver where

import Prelude

import Affjax.RequestBody (json)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web (defaultRequest, get, printError, request)
import Data.Array (snoc)
import Data.Either (Either(..), either)
import Data.EtaConversionTransformer ((<<|))
import Data.List.NonEmpty (NonEmptyList, foldl)
import Data.MediaType (MediaType(..))
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

x :: String -> Aff (Either ErrorMessage SearchResults)
x name = do
  let
    r = defaultRequest {
      url = "https://api.github.com/search/repositories?q=" <> name <> "&language:purescript&sort=created&order=desc&page=1&per_page=10",
      headers = [Accept $ MediaType "application/vnd.github+json"],
      responseFormat = ResponseFormat.string
    }
  request r >>= either
    (pure <<< Left <<< printError)
    (either
      (pure <<< Left <<< toErrorMessage)
      (pure <<< Right)
      <<< toSearchResult <<| _.body)
        
  where
  toErrorMessage :: MultipleErrors -> String
  toErrorMessage e = joinWith "\n" $ renderForeignError <$> foldl snoc [] e

toSearchResult :: String -> Either MultipleErrors SearchResults
toSearchResult = readJSON