module PinnedWorkspaces
  ( PinnedIndex
  , pinCurrentWorkspace
  , unpinCurrentWorkspace
  , withPinnedIndex
  , getSortByPinned
  , getIndex
  , getMap
  )
where

import           XMonad
import qualified XMonad.StackSet               as W
import qualified XMonad.Util.ExtensibleState   as XS
import           XMonad.Util.WorkspaceCompare   ( WorkspaceCompare
                                                , WorkspaceSort
                                                , mkWsSort
                                                )

import qualified Data.List                     as List
import qualified Data.Map.Strict               as M
import           Data.Function                  ( on )

type WorkspaceTag = String
type PinnedIndex = Int

data PinnedWorkspaceState = PinnedWorkspaceState {
        pinnedWorkspaceMap :: M.Map PinnedIndex WorkspaceTag
    }
    deriving (Typeable, Read, Show)

instance ExtensionClass PinnedWorkspaceState where
    initialValue = PinnedWorkspaceState M.empty
    extensionType = PersistentExtension

-- Pin the currently selected workspaces to the given index.
pinCurrentWorkspace :: PinnedIndex -> X ()
pinCurrentWorkspace idx = do
  wtag <- gets (W.currentTag . windowset)
  wmap <- XS.gets pinnedWorkspaceMap
  let cleared = deleteIfExists wmap wtag
  XS.modify $ \s -> s { pinnedWorkspaceMap = M.insert idx wtag cleared }
  -- TODO: Shifting to this workspace is just used to force xmonad to 
  -- trigger an event so that our log hook is called. Figure out how to 
  -- avoid needing to shift.
  withPinnedIndex W.shift idx

-- Unpins the currently selected workspace.
unpinCurrentWorkspace :: X ()
unpinCurrentWorkspace = do
  wtag <- gets (W.currentTag . windowset)
  wmap <- XS.gets pinnedWorkspaceMap
  XS.modify $ \s -> s { pinnedWorkspaceMap = deleteIfExists wmap wtag }
  windows $ W.greedyView wtag -- Same "workaround" as above

-- Execute the given function on the workspace at the given index.
withPinnedIndex :: (String -> WindowSet -> WindowSet) -> PinnedIndex -> X ()
withPinnedIndex job pidx = do
  wtag <- ilookup pidx
  maybe (return ()) (windows . job) wtag
 where
  ilookup :: PinnedIndex -> X (Maybe WorkspaceTag)
  ilookup idx = M.lookup idx `fmap` XS.gets pinnedWorkspaceMap

-- Get a workspace's pinned index.
getIndex :: [(PinnedIndex, WorkspaceId)] -> WorkspaceId -> Maybe PinnedIndex
getIndex wlist w = case List.find (\(_, x) -> x == w) wlist of
  Just p  -> Just (fst p)
  Nothing -> Nothing

-- Delete the workspace from the pinned workspace map if it exists.
deleteIfExists
  :: (M.Map PinnedIndex WorkspaceTag)
  -> WorkspaceId
  -> (M.Map PinnedIndex WorkspaceTag)
deleteIfExists wmap w = case pinnedLookup (M.toList wmap) w of
  Just idx -> M.delete idx wmap
  Nothing  -> wmap

pinnedCompare :: Maybe Int -> Maybe Int -> Ordering
pinnedCompare Nothing  Nothing  = EQ
pinnedCompare Nothing  (Just _) = GT
pinnedCompare (Just _) Nothing  = LT
pinnedCompare a        b        = compare a b

pinnedLookup :: [(PinnedIndex, WorkspaceId)] -> WorkspaceId -> Maybe PinnedIndex
pinnedLookup l w = case List.find (\(_, x) -> x == w) l of
  Just p  -> Just (fst p)
  Nothing -> Nothing

getPinnedCompare :: X WorkspaceCompare
getPinnedCompare = do
  wmap <- XS.gets pinnedWorkspaceMap
  return $ mconcat [on pinnedCompare (getIndex (M.toList wmap)), compare]

getSortByPinned :: X WorkspaceSort
getSortByPinned = mkWsSort getPinnedCompare

getMap :: X (M.Map PinnedIndex WorkspaceId)
getMap = XS.gets pinnedWorkspaceMap
