module State.UserTodoState where

import Network.RemoteData (RemoteData)

type UserTodoState
  = { username :: String
    , todos :: RemoteData String Todos
    }

type Todos
  = Array Todo

type Todo
  = { title :: String
    , completed :: Boolean
    }
