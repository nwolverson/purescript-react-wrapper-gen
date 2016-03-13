module React.WrapperGen.Main where

import Prelude
import Data.Maybe (Maybe(Just,Nothing))
import Data.Array ((!!))
import Data.Array as A
import Data.Tuple (Tuple(Tuple))
import Data.List as L
import Data.StrMap as M
import Data.Traversable (traverse)
import Data.Foldable (intercalate)
import Data.String as S
import Data.Char as C
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, catchException)
import Control.Monad.Eff.Console (CONSOLE, error, log)
import Node.FS (FS) as FS
import Node.FS.Sync (writeTextFile, readTextFile) as FS
import Node.Encoding as E
import Node.Process as P
import Node.Path (FilePath, parse, relative)
import Node.Path (concat) as NP
import React.DocGen as DG

data PropType = PBool | PFunc | PNumber | PString | PElement | PNode | PUnknown String

propType :: DG.PropInfo -> PropType
propType p =
  case p.type.name of
    "bool" -> PBool
    "func" -> PFunc
    "number" -> PNumber
    "string" -> PString
    "node" -> PNode
    "element" -> PElement
    s -> PUnknown s

showType :: PropType -> String
showType PBool = "Boolean"
showType PFunc = "EventHandlerOpt"
showType PNumber = "Number"
showType PString = "String"
showType PElement = "ReactElement"
showType PNode = "Node"
showType (PUnknown s) = "UnknownType"

excludeProp :: Tuple String PropType -> Boolean
excludeProp (Tuple _ (PUnknown _)) = true
excludeProp (Tuple "children" _) = true
excludeProp _ = false

mapStrInitial :: (Char -> Char) -> String -> String
mapStrInitial f s =
  case S.uncons s of
    Just { head : c, tail : s } -> (C.toString <<< f) c ++ s
    Nothing -> ""

sepToPascal :: String -> String -> String
sepToPascal sep = S.joinWith "" <<< map toUpperInitial <<< S.split sep

toUpperInitial :: String -> String
toUpperInitial = mapStrInitial C.toUpper

toLowerInitial :: String -> String
toLowerInitial = mapStrInitial C.toLower

kebabToCamel :: String -> String
kebabToCamel = toLowerInitial <<< sepToPascal "-"
kebabToPascal :: String -> String
kebabToPascal = sepToPascal "-"

type TranslateResults = { extern :: String, js :: String, props :: String, name :: String }

getType :: FilePath -> FilePath -> String -> String -> DG.DocResult -> TranslateResults
getType baseFname fname name prefix info =
  let types = map (\(Tuple s a) -> Tuple s $ propType a) $ M.toList info.props
      camelName = toLowerInitial name
      knownTypes = L.filter (not <<< excludeProp) types
      unknownTypes = L.filter excludeProp types
      extern = "foreign import " ++ camelName ++ "Class :: ReactClass " ++ nameProps ++ "\n"

      rel = (\f -> NP.concat [f.dir, f.name]) $ parse (relative baseFname fname)
      requireName = prefix ++ rel

      js = "exports." ++ camelName ++ " = require('" ++ requireName ++ "');\n"
      props = "foreign import data " ++ name ++ "Option :: *\n"
              ++ "newtype " ++ nameProps ++ " = " ++ nameProps ++ " Foreign\n"
              ++ toLowerInitial nameProps ++ " :: Options " ++ name ++ "Option -> " ++ nameProps ++ "\n"
              ++ toLowerInitial nameProps ++ " = " ++ nameProps ++ " <<< options\n"
              ++ toLowerInitial name ++ " :: Options " ++ name ++ "Option -> Array ReactElement -> ReactElement\n"
              ++ toLowerInitial name ++ " opts = createElement " ++ camelName ++ "Class " ++ "(" ++ toLowerInitial nameProps ++ " opts)\n"
              ++ intercalate "" (map showFieldOption types)

  in { extern, js, props, name }
  where
    nameProps = name ++ "Props"

    showFieldOption (Tuple pname ty) =
      optName ++ " :: Option " ++ name++"Option (" ++ showType ty ++ ")\n"
      ++ optName ++ " = opt \"" ++ pname ++ "\""
      ++ (case ty of
          (PUnknown s) -> " -- " ++ s
          _ -> "")
      ++ "\n"
      where optName = toLowerInitial pname


    showField (Tuple pname (PUnknown s)) = "-- " ++ pname ++ " :: {" ++ s ++ "}"
    showField (Tuple pname ty) = (if pname /= toLowerInitial pname then "-- " else "")
      ++ pname ++ " :: " ++ showType ty

getOutputFiles :: forall e. FilePath -> String -> String -> String -> Array FilePath -> Eff (fs :: FS.FS, err :: EXCEPTION, console :: CONSOLE | e) Unit
getOutputFiles baseFname moduleName prefix outDir files = do
  log $ "Generating for " ++ (show $ A.length files) ++ " files"
  infos :: Array (Maybe TranslateResults) <- traverse getOutputType files
  let infos' :: Array TranslateResults
      infos' = A.mapMaybe id infos
      mainPurs = "module " ++ moduleName ++ " where" ++ """
import Prelude (Unit)
import React (EventHandler, ReactElement)
import Data.Foreign (Foreign)
import Unsafe.Coerce (unsafeCoerce)

newtype EventHandlerOpt = EventHandlerOpt (EventHandler Unit)

newtype UnknownType = UnknownType Foreign

foreign import data Node :: *

numberNode :: Number -> Node
numberNode = unsafeCoerce
numbersNode :: Array Number -> Node
numbersNode = unsafeCoerce
stringNode :: String -> Node
stringNode = unsafeCoerce
stringsNode :: Array String -> Node
stringsNode = unsafeCoerce
elementNode :: ReactElement -> Node
elementNode = unsafeCoerce
elementsNode :: Array ReactElement -> Node
elementsNode = unsafeCoerce

"""
      purs mn {extern, props} = "module " ++ mn ++ " where" ++ """
import Prelude (Unit, unit, (<<<))
import React (EventHandler, ReactClass, ReactElement, createElement)
import Data.Options (Option, Options, opt, options)
import Data.Foreign (Foreign)
"""
        ++ "import " ++ moduleName ++ " (EventHandlerOpt, UnknownType, Node)\n"
        ++ extern ++ props
      js mn info = "// module " ++ mn ++ "\n\n" ++ info.js

  FS.writeTextFile E.UTF8 (NP.concat [outDir, moduleName] ++ ".purs") mainPurs
  void $ traverse (\info -> do
    let mn = moduleName ++ "." ++ info.name
    FS.writeTextFile E.UTF8 (NP.concat [outDir, moduleName, info.name] ++ ".purs") (purs mn info)
    FS.writeTextFile E.UTF8 (NP.concat [outDir, moduleName, info.name] ++ ".js") (js mn info)
    ) infos'

  where
  getOutputType :: forall e'. FilePath -> Eff (fs :: FS.FS, console :: CONSOLE | e') (Maybe TranslateResults)
  getOutputType fname = catchException
    (\e -> do
      error $ "Failed processing: " ++ fname ++ ": " ++ show e
      pure Nothing)
    do
      let name = kebabToPascal $ _.name $ parse fname
      contents <- FS.readTextFile E.UTF8 fname
      info <- DG.parse contents
      log $ "Read: " ++ fname
      return $ Just $ getType baseFname fname name prefix info

main :: forall e. Eff (console :: CONSOLE, fs :: FS.FS, process :: P.PROCESS, err :: EXCEPTION | e) Unit
main = do
  argv <- P.argv
  log $ "Input: " ++ intercalate " | " argv
  case { base: _, moduleName: _, prefix: _, outDir: _ } <$> argv !! 2 <*> argv !! 3 <*> argv !! 4 <*> argv !! 5 of
    Just { base, moduleName, prefix, outDir } -> do
      log $ "Running getOutputFiles : " ++ base ++ " / " ++ moduleName ++ " / " ++ prefix ++ " / " ++ outDir
      log $ "Files: " ++ intercalate " , " (A.drop 6 argv)
      getOutputFiles base moduleName prefix outDir $ A.drop 6 argv
    _ -> error "Usage: gen baseFileName moduleName prefix outDir [files...]"
