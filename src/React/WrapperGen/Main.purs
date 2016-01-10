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

data PropType = PBool | PFunc | PNumber | PString | PElement | PUnknown String

propType :: DG.PropInfo -> PropType
propType p =
  case p.type.name of
    "bool" -> PBool
    "func" -> PFunc
    "number" -> PNumber
    "string" -> PString
    s -> PUnknown s

showType :: PropType -> String
showType PBool = "Boolean"
showType PFunc = "EventHandlerOpt"
showType PNumber = "Number"
showType PString = "String"
showType PElement = "ReactElement"
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

type TranslateResults = { extern :: String, js :: String, props :: String }

getType :: FilePath -> FilePath -> String -> String -> DG.DocResult -> TranslateResults
getType baseFname fname name prefix info =
  let types = map (\(Tuple s a) -> Tuple s $ propType a) $ M.toList info.props
      camelName = toLowerInitial name
      knownTypes = L.filter (not <<< excludeProp) types
      unknownTypes = L.filter excludeProp types
      extern = "foreign import mk" ++ name ++ " :: Unit -> ReactClass " ++ nameProps ++ "\n" ++
        camelName ++ " :: ReactClass " ++ nameProps ++ "\n" ++
        camelName ++ " = mk" ++ name ++ " unit\n"

      rel = (\f -> NP.concat [f.dir, f.name]) $ parse (relative baseFname fname)
      requireName = prefix ++ rel


      js = "exports.mk" ++ name ++ " = function () { return require('" ++ requireName ++ "'); }\n"
      -- props = "type " ++ nameProps ++ " = \n    " ++ intercalate "\n    "
      --   [ "{"
      --   , intercalate ",\n    " (map showField knownTypes)
      --   , intercalate "\n    " (map showField unknownTypes)
      --   , "}\n" ]
      props = "foreign import data " ++ name ++ "Option :: *\n"
              ++ "type " ++ nameProps ++ " = Foreign\n"
              ++ toLowerInitial nameProps ++ " :: Options " ++ name ++ "Option -> " ++ nameProps ++ "\n"
              ++ toLowerInitial nameProps ++ " = options\n"
              ++ intercalate "" (map showFieldOption types)


  in { extern, js, props }
  where
    nameProps = name ++ "Props"

    showFieldOption (Tuple pname ty) =
      optName ++ " :: Option " ++ name++"Option (" ++ showType ty ++ ")\n"
      ++ optName ++ " = opt \"" ++ pname ++ "\""
      ++ (case ty of
          (PUnknown s) -> " -- " ++ s
          _ -> "")
      ++ "\n"
      where optName = "opt" ++ name ++ "_" ++ toUpperInitial pname


    showField (Tuple pname (PUnknown s)) = "-- " ++ pname ++ " :: {" ++ s ++ "}"
    showField (Tuple pname ty) = (if pname /= toLowerInitial pname then "-- " else "")
      ++ pname ++ " :: " ++ showType ty

getOutputFiles :: forall e. FilePath -> String -> String -> String -> Array FilePath -> Eff (fs :: FS.FS, err :: EXCEPTION, console :: CONSOLE | e) Unit
getOutputFiles baseFname moduleName prefix outDir files = do
  log $ "Generating for " ++ (show $ A.length files) ++ " files"
  infos :: Array (Maybe TranslateResults) <- traverse getOutputType files
  let infos' :: Array TranslateResults
      infos' = A.mapMaybe id infos
  let purs = "module " ++ moduleName ++ " where" ++ """
import Prelude (Unit, unit)
import React (EventHandler, ReactClass)
import Data.Options (class IsOption, Option, Options, opt, options)
import Data.Foreign (Foreign)

newtype EventHandlerOpt = EventHandlerOpt (EventHandler Unit)
instance eventHandlerIsOption :: IsOption EventHandlerOpt where
  assoc k a = isOptionPrimFn k a
foreign import isOptionPrimFn :: forall b a. (Option b a) -> a -> (Options b)

newtype UnknownType = UnknownType Foreign
instance unknownIsOption :: IsOption UnknownType where
  assoc k a = isOptionPrimFn k a

"""
        ++ (S.joinWith "" $ map _.extern infos' ++ map _.props infos')
  let js = "// module " ++ moduleName ++ """
exports.isOptionPrimFn = function (k) {
  return function (v) {
    return [[k,v]];
  };
};
"""
            ++ (S.joinWith "" $ map _.js infos')
  FS.writeTextFile E.UTF8 (NP.concat [outDir, moduleName] ++ ".purs") purs
  FS.writeTextFile E.UTF8 (NP.concat [outDir, moduleName] ++ ".js") js

  where
  getOutputType :: forall e'. FilePath -> Eff (fs :: FS.FS, console :: CONSOLE | e') (Maybe TranslateResults)
  getOutputType fname = catchException
    (\e -> do
      error $ "Failed to read: " ++ fname ++ ": " ++ show e
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
  case { base: _, moduleName: _, prefix: _, outDir: _ } <$> argv !! 2 <*> argv !! 3 <*> argv !! 4 <*> argv !! 5 of
    Just { base, moduleName, prefix, outDir } -> do
      log "Running getOutputFiles"
      getOutputFiles base moduleName prefix outDir $ A.drop 5 argv
    _ -> error "Usage: gen baseFileName moduleName prefix outDir [files...]"
