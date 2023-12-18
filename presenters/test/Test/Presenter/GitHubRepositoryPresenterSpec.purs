module Test.Presenter.GitHubRepositoryPresenterSpec where

import Prelude

import Control.Monad.Reader (runReaderT)
import Data.Date (Date, canonicalDate)
import Data.Enum (class BoundedEnum, toEnum)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Domain.GitHubRepository (GitHubRepositories(..), GitHubRepository(..), GitHubRepositoryName(..), GitHubRepositoryOwner(..), GitHubRepositoryUpdateDate(..), GitHubRepositoryUrl(..))
import Effect.Aff (Aff)
import Presenter.GitHubRepositoryPresenter (setLoading, setRepositories)
import State.SearchGitHubRepositoryState (ErrorMessage)
import State.SearchGitHubRepositoryState as State
import Test.PMock (any, fun, hasBeenCalledWith, mock, mockFun, (:>))
import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec = do
  describe "Presenter" do
    it "setRepository" do
      let 
        repositories = GitHubRepositories [
          GitHubRepository {
            name: GitHubRepositoryName "name",
            url: GitHubRepositoryUrl "url",
            owner: GitHubRepositoryOwner "owner",
            updateDate: GitHubRepositoryUpdateDate $ parse "2023/12/18"
          }
        ]
        stateMock = mock $ any@State.GitHubRepositories :> pure@Aff unit
      _ <- runReaderT (setRepositories repositories) {
        setRepositories: fun stateMock,
        setLoading: mockFun $ any@Boolean :> pure@Aff unit,
        setErrorMessage: mockFun $ any@ErrorMessage :> pure@Aff unit
      }
      stateMock `hasBeenCalledWith` [
        {
          name: "name",
          url: "url",
          owner: "owner",
          updateDate: "2023/12/18"
        }
      ]
    
    it "setLoading" do
      let 
        stateMock = mock $ any@Boolean :> pure@Aff unit
      _ <- runReaderT (setLoading true) {
        setRepositories: mockFun $ any@State.GitHubRepositories :> pure@Aff unit,
        setLoading: fun stateMock,
        setErrorMessage: mockFun $ any@ErrorMessage :> pure@Aff unit
      }
      stateMock `hasBeenCalledWith` true


parse :: String -> Maybe Date
parse s = case split (Pattern "/") s of
  [year, month, day] -> canonicalDate <$> (convert year) <*> (convert month) <*> (convert day)
  _ -> Nothing
  where
  convert :: forall c. BoundedEnum c => String -> Maybe c
  convert = toEnum <=< fromString