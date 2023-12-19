module SearchGitHubRepositoryComponent where

import Prelude

import Controller.GitHubRepositoryController (searchRepositoryByName)
import Data.Either (Either(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import State.SearchGitHubRepositoryState (SearchGitHubRepositoryState)
import Web.Event.Event (Event)
import Web.Event.Event as Event

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
initialState _ = { searchRepositoryName: mempty, repositories: Right mempty, isLoading: false }

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
        [ HP.disabled $ state.isLoading
        , HP.type_ HP.ButtonSubmit
        ]
        [ HH.text "Search" ]
    , renderRepositories state.repositories
    ]
  where
  renderRepositories = case _ of
    Left err ->
      HH.div_ [ HH.text $ "Failed loading repositories: " <> err ]
    Right repositories ->
      HH.div_ (renderRepository <$> repositories)
  
  renderRepository repository =
    HH.div_
      [ HH.text repository.owner
      , HH.a [HP.href repository.url ] [ HH.text repository.name ]
      , HH.text repository.updateDate
      ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM SearchGitHubRepositoryState Action () o m Unit
handleAction = case _ of
  SetSearchRepositoryName searchRepositoryName -> do
    H.modify_ (_ { searchRepositoryName = searchRepositoryName })

  SearchRepository event -> do
    H.liftEffect $ Event.preventDefault event
    searchRepositoryByName =<< H.gets _.searchRepositoryName
