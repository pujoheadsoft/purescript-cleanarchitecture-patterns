module SearchGitHubRepositoryComponent where

import Prelude

import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (json)
import Affjax.ResponseFormat as AXRF
import Affjax.Web (defaultRequest)
import Affjax.Web as AX
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.MediaType (MediaType(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (method)
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), isLoading)
import State.SearchGitHubRepositoryState (SearchGitHubRepositoryState)
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Web.XHR.XMLHttpRequest (response)

data Action
  = SetSearchRepositoryName String
  | SearchRepository Event

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> SearchGitHubRepositoryState
initialState _ = { searchRepositoryName: "", repositories: NotAsked }

render :: forall m. SearchGitHubRepositoryState -> H.ComponentHTML Action () m
render state =
  HH.form
    [ HE.onSubmit SearchRepository ]
    [ HH.h1_ [ HH.text "Search GitHub Repository" ]
    , HH.label_
        [ HH.div_ [ HH.text "Enter repository name:" ]
        , HH.input
            [ HP.value state.searchRepositoryName
            , HE.onValueInput SetSearchRepositoryName
            ]
        ]
    , HH.button
        [ HP.disabled $ isLoading state.repositories
        , HP.type_ HP.ButtonSubmit
        ]
        [ HH.text "Search" ]
    , renderRepositories state.repositories
    ]
  where
  renderRepositories = case _ of
    NotAsked ->
      HH.div_
        [ HH.text "Repository not loaded" ]
    Loading ->
      HH.div_
        [ HH.text "Loading Repositories" ]
    Failure err ->
      HH.div_
        [ HH.text $ "Failed loading repositories: " <> err ]
    Success repositories ->
      HH.div_
        (map renderRepository repositories)
  
  renderRepository repository =
    HH.div_
      [ HH.text repository.name
      , HH.text repository.owner
      ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM SearchGitHubRepositoryState Action () o m Unit
handleAction = case _ of
  SetSearchRepositoryName searchRepositoryName -> do
    H.modify_ (_ { searchRepositoryName = searchRepositoryName, repositories = NotAsked })
  SearchRepository event -> do
    H.liftEffect $ Event.preventDefault event
    name <- H.gets _.searchRepositoryName
    H.modify_ (_ { repositories = Loading })

    r <- H.liftAff $ AX.request (defaultRequest 
      { url = "https://api.github.com/search/repositories?q=" <> name <> "&language:purescript&sort=created&order=desc&page=1&per_page=10"
      , headers = [Accept $ MediaType "application/vnd.github+json"]
      , responseFormat = json
      })
    case r of
      Left e -> pure unit
      Right v -> pure unit

    -- curl -H 'Accept: application/vnd.github+json' 'https://api.github.com/search/repositories?q=spago&language:purescript&sort=created&order=desc&page=1&per_page=10'
    response <- H.liftAff $ AX.get AXRF.string ("https://api.github.com/users/" <> name)
    --H.modify_ (_ { result = map _.body (hush response) })
    H.modify_ (_ { repositories = Success [] })
