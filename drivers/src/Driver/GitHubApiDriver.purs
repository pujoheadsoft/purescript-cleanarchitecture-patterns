module Driver.GitHubApiDriver where

import Prelude

import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web (Request, defaultRequest, printError, request)
import Data.Array (snoc)
import Data.Either (Either(..), either)
import Data.EtaConversionTransformer ((<<|))
import Data.List.NonEmpty (foldl)
import Data.MediaType (MediaType(..))
import Data.String (joinWith)
import Effect.Aff.Class (class MonadAff, liftAff)
import Foreign (MultipleErrors, renderForeignError)
import Gateway.Port (ErrorMessage, GitHubRepositoryGatewayPortFunction, SearchResults)
import Simple.JSON (readJSON)

gitHubRepositoryGatewayPortFunction :: forall m. MonadAff m => GitHubRepositoryGatewayPortFunction m
gitHubRepositoryGatewayPortFunction = { searchByName }

searchByName :: forall m. MonadAff m => String -> m (Either ErrorMessage SearchResults)
searchByName = doRequest jsonToSearchResult <<| searchByNameRequest

doRequest :: forall m a. MonadAff m => (String -> Either MultipleErrors a) -> Request String -> m (Either ErrorMessage a)
doRequest f r = liftAff do
  request r >>= either
    (pure <<< Left <<< printError)
    (either
      (pure <<< Left <<< toErrorMessage)
      (pure <<< Right)
      <<< f <<| _.body)

toErrorMessage :: MultipleErrors -> String
toErrorMessage e = joinWith "\n" $ renderForeignError <$> foldl snoc [] e

searchByNameRequest :: String -> Request String
searchByNameRequest name = defaultRequest {
  url = "https://api.github.com/search/repositories?q=" <> name <> "&language:purescript&sort=created&order=desc&page=1&per_page=10",
  headers = [Accept $ MediaType "application/vnd.github+json"],
  responseFormat = ResponseFormat.string
}

jsonToSearchResult :: String -> Either MultipleErrors SearchResults
jsonToSearchResult = readJSON