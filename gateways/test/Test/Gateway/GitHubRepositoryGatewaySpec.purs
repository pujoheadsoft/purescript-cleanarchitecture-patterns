module Test.Gateway.GitHubRepositoryGatewaySpec where

import Prelude

import Control.Monad.Reader (runReaderT)
import Data.Date (Date, canonicalDate)
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, toEnum)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Domain.Error (Error(..))
import Domain.GitHubRepository (GitHubRepositories(..), GitHubRepository(..), GitHubRepositoryName(..), GitHubRepositoryOwner(..), GitHubRepositoryUpdateDate(..), GitHubRepositoryUrl(..))
import Effect.Aff (Aff)
import Gateway.GitHubRepositoryGateway (searchByName)
import Test.PMock (mockFun, (:>))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Search GitHub Repository By Repository Name" do
    it "Success" do
      let
        output = {
          items: [
            {
              full_name: "nameX",
              owner: {
                login: "user"
              },
              html_url: "http://url",
              updated_at: "2023-12-17T14:17:49Z"
            }
          ]
        }
        expected = Right $ GitHubRepositories [
          GitHubRepository { 
            name: GitHubRepositoryName "nameX",
            url: GitHubRepositoryUrl "http://url",
            owner: GitHubRepositoryOwner "user",
            updateDate: GitHubRepositoryUpdateDate $ parse "2023/12/17"
          }
        ]

      actual <- runReaderT (searchByName (GitHubRepositoryName "name")) {
        searchByName: mockFun $ "name" :> pure@Aff (Right output)
      }

      actual `shouldEqual` expected
    
    it "Failed" do
      let expected = Left (Error "search error")

      actual <- runReaderT (searchByName (GitHubRepositoryName "name")) {
        searchByName: mockFun $ "name" :> pure@Aff (Left "search error")
      }

      actual `shouldEqual` expected

parse :: String -> Maybe Date
parse s = case split (Pattern "/") s of
  [year, month, day] -> canonicalDate <$> (convert year) <*> (convert month) <*> (convert day)
  _ -> Nothing
  where
  convert :: forall c. BoundedEnum c => String -> Maybe c
  convert = toEnum <=< fromString