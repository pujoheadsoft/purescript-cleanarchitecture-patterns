module Gateway.Port where

import Prelude

import Control.Monad.Reader (ReaderT)
import Data.Either (Either)
import Data.ReaderTEtaConversionTransformer (readerT)
import Type.Equality (class TypeEquals)

class Monad m <= GitHubRepositoryGatewayPort m where
  searchByName :: String -> m (Either ErrorMessage SearchResults)

type GitHubRepositoryGatewayPortFunction m = 
  { searchByName :: String -> m (Either ErrorMessage SearchResults) }

instance instancePortReaderT ::
  (Monad m, TypeEquals f (GitHubRepositoryGatewayPortFunction m)) =>
  GitHubRepositoryGatewayPort (ReaderT f m) where
  searchByName = readerT _.searchByName

type ErrorMessage = String

type SearchResults = { items :: Array SearchResult }

type SearchResult = {
  full_name :: String,
  owner :: {
    login :: String
  },
  html_url :: String,
  updated_at :: String
}