module Test.UseCase.SearchGitHubRepositoryUseCaseSpec where

import Prelude

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Search GitHub Repository By Repository Name" do
    it "Successful searched" do
      "" `shouldEqual` ""