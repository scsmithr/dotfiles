module PinnedWorkspaces
  ( PinnedIndex
  , pinCurrentWorkspace
  , unpinCurrentWorkspace
  , withPinnedIndex
  , indexReader
  , getSortByPinned
  )
where

import           XMonad
import qualified XMonad.StackSet               as W
import qualified XMonad.Util.ExtensibleState   as XS
import           XMonad.Util.WorkspaceCompare

import           Data.List
import           Data.Function
import qualified Data.Map.Strict               as SM

type WorkspaceTag = String
type PinnedIndex = Int

data PinnedWorkspaceState = PinnedWorkspaceState {
        pinnedWorkspaceMap :: SM.Map PinnedIndex WorkspaceTag
    }
    deriving (Typeable, Read, Show)

instance ExtensionClass PinnedWorkspaceState where
    initialValue = PinnedWorkspaceState SM.empty
    extensionType = PersistentExtension

-- Pin the currently selected workspaces to the given index.
pinCurrentWorkspace :: PinnedIndex -> X ()
pinCurrentWorkspace idx = do
  wtag <- gets (W.currentTag . windowset)
  wmap <- XS.gets pinnedWorkspaceMap
  let cleared = deleteIfExists wmap wtag
  XS.modify $ \s -> s { pinnedWorkspaceMap = SM.insert idx wtag cleared }
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
  ilookup idx = SM.lookup idx `fmap` XS.gets pinnedWorkspaceMap

-- Allows for retrieving the index of a workspace.
-- I don't think this is the best way to do this.
indexReader :: X (WorkspaceId -> Maybe Int)
indexReader = do
  wmap <- XS.gets pinnedWorkspaceMap
  return (pinnedLookup $ SM.toList wmap)

-- Delete the workspace from the pinned workspace map if it exists.
deleteIfExists
  :: (SM.Map PinnedIndex WorkspaceTag)
  -> WorkspaceId
  -> (SM.Map PinnedIndex WorkspaceTag)
deleteIfExists wmap w = case pinnedLookup (SM.toList wmap) w of
  Just idx -> SM.delete idx wmap
  Nothing  -> wmap

pinnedCompare :: Maybe Int -> Maybe Int -> Ordering
pinnedCompare Nothing  Nothing  = EQ
pinnedCompare Nothing  (Just _) = GT
pinnedCompare (Just _) Nothing  = LT
pinnedCompare a        b        = compare a b

pinnedLookup :: [(PinnedIndex, WorkspaceId)] -> WorkspaceId -> Maybe PinnedIndex
pinnedLookup l w = case find (\(_, x) -> x == w) l of
  Just p  -> Just (fst p)
  Nothing -> Nothing

getPinnedCompare :: X WorkspaceCompare
getPinnedCompare = do
  idx <- indexReader
  return $ mconcat [pinnedCompare `on` idx, compare]

getSortByPinned :: X WorkspaceSort
getSortByPinned = mkWsSort getPinnedCompare
