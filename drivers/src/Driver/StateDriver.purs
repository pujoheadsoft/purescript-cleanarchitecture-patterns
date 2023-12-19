module Driver.StateDriver where

import Control.Monad.State (class MonadState, modify_)
import Data.Either (Either(..))
import Presenter.Port (GitHubRepositoryPresenterPortFunction)
import State.SearchGitHubRepositoryState (SearchGitHubRepositoryState)

presenterPortFunction :: forall m. MonadState SearchGitHubRepositoryState m => GitHubRepositoryPresenterPortFunction m
presenterPortFunction = {
  setRepositories: \r -> modify_ (_ { repositories = Right r }),
  setLoading: \loading -> modify_ (_ { isLoading = loading }),
  setErrorMessage: \m -> modify_ (_ { repositories = Left m })
}