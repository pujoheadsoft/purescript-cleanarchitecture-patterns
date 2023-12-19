module Test.UseCase.SearchGitHubRepositoryUseCaseSpec where

import Prelude

import Control.Monad.Reader (runReaderT)
import Data.Either (Either(..))
import Domain.Error (Error(..))
import Domain.GitHubRepository (GitHubRepositories(..), GitHubRepositoryName(..))
import Effect.Aff (Aff)
import Test.PMock (any, fun, hasBeenCalledInOrder, hasBeenCalledWith, hasNotBeenCalledWith, mock, mockFun, (:>))
import Test.Spec (Spec, describe, it)
import UseCase.SearchGitHubRepositoryUseCase (execute)

spec :: Spec Unit
spec = do
  describe "Search GitHub Repository By Repository Name" do
    it "Successful searched" do
      let
        repositories = GitHubRepositories []
        name = GitHubRepositoryName "name"

        -- mocks
        setLoading = mock $ any@Boolean :> pure@Aff unit
        searchByName = mock $ name :> pure@Aff (Right repositories)
        setRepositories = mock $ any :> pure@Aff unit

      -- sut
      _ <- runReaderT (execute name) {
        searchByName: fun searchByName,
        setRepositories: fun setRepositories,
        setLoading: fun setLoading,
        setErrorMessage: mockFun $ any@Error :> pure@Aff unit
      }

      -- verify
      setRepositories `hasBeenCalledWith` repositories
      setLoading `hasBeenCalledInOrder` [true, false]

    it "Fail searched" do
      let
        name = GitHubRepositoryName "name"

        -- mocks
        setLoading = mock $ any@Boolean :> pure@Aff unit
        searchByName = mock $ name :> pure@Aff (Left $ Error "search error")
        setRepositories = mock $ any@GitHubRepositories :> pure@Aff unit
        setErrorMessage = mock $ any@Error :> pure@Aff unit

      -- sut
      _ <- runReaderT (execute name) {
        searchByName: fun searchByName,
        setRepositories: fun setRepositories,
        setLoading: fun setLoading,
        setErrorMessage: fun setErrorMessage
      }

      -- verify
      setRepositories `hasNotBeenCalledWith` any@GitHubRepositories
      setErrorMessage `hasBeenCalledWith` Error "search error"
      setLoading `hasBeenCalledInOrder` [true, false]
