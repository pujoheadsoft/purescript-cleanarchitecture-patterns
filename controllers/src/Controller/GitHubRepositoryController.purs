module Controller.GitHubRepositoryController where

import Prelude

import Control.Monad.Reader (runReaderT)
import Control.Monad.State (class MonadState)
import Domain.GitHubRepository (GitHubRepositoryName(..))
import Driver.GitHubApiDriver (gitHubRepositoryGatewayPortFunction)
import Driver.StateDriver (presenterPortFunction)
import Effect.Aff.Class (class MonadAff)
import Gateway.GitHubRepositoryGateway (gitHubRepositoryPortFunction)
import Presenter.GitHubRepositoryPresenter (gitHubRepositoryPresenterPortFunction)
import Record.Builder (build, merge)
import State.SearchGitHubRepositoryState (SearchGitHubRepositoryState)
import UseCase.SearchGitHubRepositoryUseCase (execute)

searchRepositoryByName
 :: forall m
  . Monad m
 => MonadAff m
 => MonadState SearchGitHubRepositoryState m
 => String
 -> m Unit
searchRepositoryByName name = do
  let
    gf = gitHubRepositoryPortFunction gitHubRepositoryGatewayPortFunction
    pf = gitHubRepositoryPresenterPortFunction presenterPortFunction 
    functions = build (merge (gf)) pf
  runReaderT (execute (GitHubRepositoryName name)) functions