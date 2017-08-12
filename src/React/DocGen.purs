module React.DocGen (parse, DocResult, PropInfo, PropType, FlowType) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Maybe (Maybe)
import Data.StrMap as M
import Data.Undefinable (Undefinable, toMaybe)

foreign import parseImpl :: forall eff. String -> Eff (exception :: EXCEPTION | eff) (DocResult PropInfo_)

parse ::forall eff. String -> Eff (exception :: EXCEPTION | eff) (DocResult PropInfo)
parse s = (\x -> x { props = conv <$> x.props }) <$> parseImpl s
  where
  conv r@{ "type": t, flowType } = r { "type" = toMaybe r."type", flowType = toMaybe r.flowType }

type DocResult a = {
  props :: M.StrMap a,
  description :: String
}

type PropType = { name :: String }
type FlowType = { name :: String }

type PropInfo = {
  type :: Maybe PropType,
  flowType :: Maybe FlowType,
  required :: Boolean,
  description :: String
}

type PropInfo_ = {
  type :: Undefinable PropType,
  flowType :: Undefinable FlowType,
  required :: Boolean,
  description :: String
}