module Domain.Error where

import Prelude

newtype Error = Error String

derive newtype instance showError :: Show Error
derive newtype instance eqError :: Eq Error