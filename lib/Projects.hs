{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, PatternGuards #-}

module Projects where

import MyPrompts
import XMonad.Prompt.Directory

import Graphics.X11.Types

import Data.Map.Strict (Map)
import Data.Monoid (All(..))
import qualified Data.Map.Strict as Map
import XMonad.Actions.DynamicWorkspaces
import qualified XMonad.Util.ExtensibleState as XS

import System.FilePath
import XMonad.Hooks.ServerMode


import System.Directory ( setCurrentDirectory, getCurrentDirectory )
import Control.Monad ( when )

import XMonad hiding ( focus )
import XMonad.Layout.LayoutModifier
import XMonad.StackSet ( tag, currentTag )


projectPrompts :: XPConfig -> PromptList
projectPrompts conf = [
--     Action ["x"] "Rename project" (renameProjectPrompt conf),
--     Action ["X"] "Change project directory" (changeProjectDirPrompt conf),
    Action ["g"] "New Project from Directory"
        (spawn "xmonadctl -a DIRNAME \"switch $(cd ~; realpath $(menu directory))\""),
    Action ["G"] "New Project from Directory"
        (spawn "xmonadctl -a DIRNAME \"rename $(cd ~; realpath $(menu directory))\"")
    ]

-- Need to manually update the project list

newDir :: XPConfig -> (String -> X ()) -> X ()
newDir conf s =  directoryPrompt conf "Directory: " $ goDir s

goDir :: (String -> X()) -> String -> X ()
goDir s x =
      let fn = case takeFileName $ takeDirectory (x++"/") of
                  ('.':xs) -> xs
                  [] -> "scratch"
                  xs -> xs
      in do
         s fn
         sendMessage $ Chdir x


chdirEventHook :: Event -> X All
chdirEventHook =  serverModeEventHookF "DIRNAME" $ \x ->
    let (cmd,dir) = span (/=' ') x
        s = case cmd of
              "switch" -> addWorkspace
              _ -> renameWorkspaceByName
    in goDir s (dropWhile (==' ') dir)


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
