module Presenter.GitHubRepositoryPresenter where

import Prelude

import Domain.GitHubRepository (GitHubRepositories)

setRepositories :: forall m. Monad m => GitHubRepositories -> m Unit
setRepositories r = pure unit