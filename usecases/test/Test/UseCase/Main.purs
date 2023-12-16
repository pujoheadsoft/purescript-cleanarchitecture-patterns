module Test.UseCase.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.UseCase.SearchGitHubRepositoryUseCaseSpec (spec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  spec

