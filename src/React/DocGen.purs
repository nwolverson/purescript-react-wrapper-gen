module React.DocGen where

import Data.StrMap as M

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)

foreign import parse :: forall eff. String -> Eff (exception :: EXCEPTION | eff) DocResult

type DocResult = {
  props :: M.StrMap PropInfo,
  description :: String
}

type PropInfo = {
  type :: { name :: String },
  required :: Boolean,
  description :: String
}
