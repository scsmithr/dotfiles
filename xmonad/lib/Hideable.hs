module Hideable
  ( hiddenWindows
  , pushHidden
  , popHidden
  , getMap
  )
where

import           XMonad
import qualified XMonad.StackSet               as W
import qualified XMonad.Util.ExtensibleState   as XS
import           XMonad.Layout.Hidden           ( hiddenWindows
                                                , hideWindow
                                                , popNewestHiddenWindow
                                                )

import qualified Data.Map.Strict               as M

data HiddenCounts = HiddenCounts {
        hiddenCountsMap :: M.Map WorkspaceId Int
    }
    deriving (Typeable, Read, Show)

instance ExtensionClass HiddenCounts where
    initialValue = HiddenCounts M.empty
    extensionType = PersistentExtension

pushHidden :: Window -> X ()
pushHidden win = do
  alterCounts 1
  hideWindow win

popHidden :: X ()
popHidden = do
  alterCounts (-1)
  popNewestHiddenWindow

getMap :: X (M.Map WorkspaceId Int)
getMap = XS.gets hiddenCountsMap

alterCounts :: Int -> X ()
alterCounts diff = do
  wtag   <- gets (W.currentTag . windowset)
  counts <- XS.gets hiddenCountsMap
  let f a = case a of
        Just n | (n + diff) > 0 -> Just (n + diff)
        Nothing | diff > 0      -> Just (diff)
        _                       -> Nothing
  XS.modify (\s -> s { hiddenCountsMap = M.alter f wtag counts })
