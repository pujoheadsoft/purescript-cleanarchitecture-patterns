module SearchGitHubRepositoryComponent where

import Prelude

import Affjax.ResponseFormat as AXRF
import Affjax.Web as AX
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), isLoading)
import State.SearchGitHubRepositoryState (SearchGitHubRepositoryState)
import Web.Event.Event (Event)
import Web.Event.Event as Event

data Action
  = SetUsername String
  | MakeRequest Event

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> SearchGitHubRepositoryState
initialState _ = { name: "", repositories: NotAsked }

render :: forall m. SearchGitHubRepositoryState -> H.ComponentHTML Action () m
render state =
  HH.form
    [ HE.onSubmit MakeRequest ]
    [ HH.h1_ [ HH.text "Lookup GitHub user" ]
    , HH.label_
        [ HH.div_ [ HH.text "Enter username:" ]
        , HH.input
            [ HP.value state.name
            , HE.onValueInput SetUsername
            ]
        ]
    , HH.button
        [ HP.disabled $ isLoading state.repositories
        , HP.type_ HP.ButtonSubmit
        ]
        [ HH.text "Fetch info" ]
    , renderRepositories state.repositories
    ]
  where
  renderRepositories = case _ of
    NotAsked ->
      HH.div_
        [ HH.text "Tags not loaded" ]
    Loading ->
      HH.div_
        [ HH.text "Loading Tags" ]
    Failure err ->
      HH.div_
        [ HH.text $ "Failed loading tags: " <> err ]
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
  SetUsername name -> do
    H.modify_ (_ { name = name, repositories = NotAsked })
  MakeRequest event -> do
    H.liftEffect $ Event.preventDefault event
    username <- H.gets _.name
    H.modify_ (_ { repositories = Loading })
    -- curl -H 'Accept: application/vnd.github+json' 'https://api.github.com/search/repositories?q=spago&language:purescript&sort=created&order=desc&page=1&per_page=10'
    response <- H.liftAff $ AX.get AXRF.string ("https://api.github.com/users/" <> username)
    --H.modify_ (_ { result = map _.body (hush response) })
    H.modify_ (_ { repositories = Success [] })
