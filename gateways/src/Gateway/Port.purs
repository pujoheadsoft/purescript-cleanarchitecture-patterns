module Gateway.Port where

import Prelude

import Control.Monad.Reader (ReaderT)
import Data.Either (Either)
import Data.ReaderTEtaConversionTransformer (readerT)
import Type.Equality (class TypeEquals)

class Monad m <= GitHubRepositoryGatewayPort m where
  searchByName :: String -> m (Either ErrorMessage SearchResults)

type GitHubRepositoryGatewayPortFunction m r
  = { searchByName :: String -> m (Either ErrorMessage SearchResults)
    | r
    }

instance instancePortReaderT ::
  (Monad m, TypeEquals f (GitHubRepositoryGatewayPortFunction m r)) =>
  GitHubRepositoryGatewayPort (ReaderT f m) where
  searchByName = readerT _.searchByName

type ErrorMessage = String

type SearchResults = Array SearchResult

type SearchResult = {

}