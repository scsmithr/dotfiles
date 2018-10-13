module RofiPrompt (
    execRofiPrompt,
    selectWorkspace,
    withWorkspace 
) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.Run ( runProcessWithInput )

import qualified XMonad.Actions.DynamicWorkspaces as DynWs

import Data.Char 
import Data.List

-- switch to a workspace, creating it if it doesn't exist.
selectWorkspace :: X()
selectWorkspace = withWorkspace DynWs.addWorkspace

-- withWorkspace executes a given function once a workspace is selected.
withWorkspace :: (String -> X()) -> X()
withWorkspace fn = do
    tags <- getTags
    execRofiPrompt tags fn

-- execute rofi with a list of strings, executing the provided function with the
-- selected option. If the input from rofi is an empty string, nothing will be 
-- done.
execRofiPrompt :: [String] -> (String -> X()) -> X()
execRofiPrompt opts fn = do
    let out = intercalate "\n" opts
    chosen <- runProcessWithInput "rofi" ["-dmenu"] out
    case chosen of
        "" -> return()
        _ -> fn $ rstrip chosen

getTags :: X [WorkspaceId]
getTags = do
    ws <- gets (W.workspaces . windowset)
    let tags = map W.tag ws
    return tags


rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse
