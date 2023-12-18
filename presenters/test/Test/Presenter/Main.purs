module Test.Presenter.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Presenter.GitHubRepositoryPresenterSpec (spec)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  spec

