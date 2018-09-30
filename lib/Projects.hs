{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, PatternGuards #-}

module Projects where

import MyPrompts
import XMonad.Prompt.Directory

import Graphics.X11.Types
import System.IO (hClose)
import Control.Exception (bracket)

import Data.Map.Strict (Map)
import Data.Monoid (All(..))
import qualified Data.Map.Strict as Map
import XMonad.Actions.DynamicWorkspaces
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Run

import System.FilePath
import XMonad.Hooks.ServerMode
import qualified XMonad.StackSet as W

import System.Directory ( setCurrentDirectory, getCurrentDirectory )
import Control.Monad ( when )

import XMonad hiding ( focus )
import XMonad.Layout.LayoutModifier
import XMonad.StackSet ( tag, currentTag )

import XMonad.Prompt.Input
import Colors
import MyGroups (groups, accentmap)

import Data.Function ((&))

projectPrompts :: XPConfig -> PromptList
projectPrompts conf =
  --     Action ["x"] "Rename project" (renameProjectPrompt conf),
  --     Action ["X"] "Change project directory" (changeProjectDirPrompt conf),
   [Action ["G"] "New Project from Directory"
        (spawn "xmonadctl -a NEW_WORKSPACE $(cd ~; realpath $(menu directory))")

   ,Action ["g"] "Switch Project" $ do
         w <- workspacenames
         liftIO $ bracket
          (spawnPipe $ "menu ! xmonadctl -a SWITCH_WORKSPACE")
          hClose
          (flip hPutStr (unlines w))

    ,Action ["X"] "Move to Directory"
        (spawn "xmonadctl -a MOVE_WORKSPACE $(cd ~; realpath $(menu directory))")

    ,Action ["x"] "Rename Project" $ do
         newname <- inputPrompt myXPConfig "rename workspace >"
         case newname of
          Just n -> renameWorkspaceByName n
          Nothing -> return ()
    ]


-- Need to manually update the project list

workspacenames :: X [String]
workspacenames = do
    ws <- gets windowset
    return $ (map W.workspace (W.current ws : W.visible ws) ++ W.hidden ws)
      & map (\x -> let t = W.tag x
                in "$(fgcolor '"++accentcolors accentmap t++"' "++t++")")


newDir :: XPConfig -> (String -> X ()) -> X ()
newDir conf s =  directoryPrompt conf "Directory: " $ goDir s

goDir :: (String -> X()) -> String -> X ()
goDir s x =
      if x == ""
        then return ()
        else do
         s x
         sendMessage $ Chdir x


chdirEventHook :: Event -> X All
chdirEventHook event =  foldr mappend mempty $ map (\x -> x event)
  [  serverModeEventHookF "NEW_WORKSPACE" $ goDir addWorkspace
  ,  serverModeEventHookF "MOVE_WORKSPACE" $ goDir (const (return ()))
  ,  serverModeEventHookF "RENAME_WORKSPACE" $ renameWorkspaceByName
  ,  serverModeEventHookF  "SWITCH_WORKSPACE" $ addWorkspace
  ]



cleanupworkspaces :: X ()
cleanupworkspaces = withWindowSet $ \ws ->
  flip mapM_ (W.hidden ws) $ \w ->
    case W.stack w of
      Nothing -> do
        removeEmptyWorkspaceByTag (W.tag w)
      _ -> return ()


-----------------------------------------
-- COPIED FROM XMonad.Layout.WorkspaceDir
-----------------------------------------
--
-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.WorkspaceDir
--
-- Then edit your @layoutHook@ by adding the Workspace layout modifier
-- to some layout:
--
-- > myLayout = workspaceDir "~" (Tall 1 (3/100) (1/2))  ||| Full ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- WorkspaceDir provides also a prompt. To use it you need to import
-- "XMonad.Prompt" and add something like this to your key bindings:
--
-- >  , ((modm .|. shiftMask, xK_x     ), changeDir def)
--
-- For detailed instruction on editing the key binding see:
--
-- "XMonad.Doc.Extending#Editing_key_bindings".

newtype Chdir = Chdir String deriving ( Typeable )
instance Message Chdir

newtype WorkspaceDir a = WorkspaceDir String deriving ( Read, Show )

instance LayoutModifier WorkspaceDir Window where
    modifyLayout (WorkspaceDir d) w r = do tc <- gets (currentTag.windowset)
                                           when (tc == tag w) $ scd d
                                           runLayout w r
    handleMess (WorkspaceDir _) m
        | Just (Chdir wd) <- fromMessage m = do wd' <- cleanDir wd
                                                return $ Just $ WorkspaceDir wd'
        | otherwise = return Nothing

workspaceDir :: LayoutClass l a => String -> l a
             -> ModifiedLayout WorkspaceDir l a
workspaceDir s = ModifiedLayout (WorkspaceDir s)

cleanDir :: String -> X String
cleanDir x = scd x >> io getCurrentDirectory

scd :: String -> X ()
scd x = catchIO $ setCurrentDirectory x
