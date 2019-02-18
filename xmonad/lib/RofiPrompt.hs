module RofiPrompt
  ( exec
  , execZip
  , selectWorkspace
  , withWorkspace
  )
where

import           XMonad
import qualified XMonad.StackSet               as W
import           XMonad.Util.Run                ( runProcessWithInput )
import qualified XMonad.Actions.DynamicWorkspaces
                                               as DynWs

import           Data.Char                      ( isSpace )
import           Data.List                      ( reverse
                                                , dropWhile
                                                , intercalate
                                                , map
                                                )

-- Switch to a workspace, creating it if it doesn't exist.
selectWorkspace :: X ()
selectWorkspace = withWorkspace DynWs.addWorkspace

-- Executes a given function once a workspace is selected.
withWorkspace :: (String -> X ()) -> X ()
withWorkspace fn = do
  tags <- getTags
  execZip "workspace" tags fn

exec :: String -> [(String, X ())] -> (String -> X ()) -> X ()
exec prompt opts def = do
  let ss  = fsts opts
  let out = intercalate "\n" ss
  chosen <- runProcessWithInput "rofi" ["-p", prompt, "-dmenu"] out
  let s = rstrip chosen
  case find s opts of
    Just pair         -> snd pair
    Nothing | s /= "" -> def s
    _                 -> return ()

-- Execute rofi with a list of strings, executing the provided function with the
-- selected option. If the input from rofi is an empty string, nothing will be 
-- done.
execZip :: String -> [String] -> (String -> X ()) -> X ()
execZip prompt opts fn = do
  let ts = map (\x -> (x, fn x)) opts
  exec prompt ts fn

fsts :: [(String, X ())] -> [String]
fsts []       = []
fsts (x : xs) = [fst x] ++ fsts xs

find :: String -> [(String, X ())] -> Maybe (String, X ())
find _ [] = Nothing
find s (x : xs) | fst x == s = Just x
                | otherwise  = find s xs

getTags :: X [WorkspaceId]
getTags = do
  ws <- gets (W.workspaces . windowset)
  let tags = map W.tag ws
  return tags

rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse
