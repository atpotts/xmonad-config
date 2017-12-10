module Projects where

import MyPrompts
import XMonad.Prompt.Directory

import Graphics.X11.Types

import XMonad
import XMonad.Prompt
import XMonad.Prompt.Window (XWindowMap)
import DynamicProjects as P
import qualified XMonad.Util.ExtensibleState as XS

import System.FilePath

dynamicProjects = P.dynamicProjects

projects :: [Project]
projects =
    [Project { projectName = "scratch",
               projectDirectory = "~/",
               projectStartHook = Just $ do spawn "firefox"},
     Project { projectName = "haskell-spock",
               projectDirectory = "~/Desktop/spock",
               projectStartHook = Just $ do spawn "emacsclient -c"},
     Project { projectName = "dotfiles",
               projectDirectory = "~/.dot",
               projectStartHook = Just $ do spawn "emacsclient -c"
                                            spawn "urxvt" }
     ]

projectPrompts :: XPConfig -> PromptList 
projectPrompts conf = [
    (["M-p"],   "Go to project",            switchProjectPrompt conf),  
    (["M-S-p"], "Send to project",          shiftToProjectPrompt conf), 
    (["M-'"],   "Rename project",           renameProjectPrompt conf),  
    (["M-S-'"], "Change project directory", changeProjectDirPrompt conf),
    (["M-g"],   "New Project from Directory", newDir conf
                $ \p -> switchProject p ),
    (["M-S-g"], "Window to new directory",   newDir conf
                $ \p ->  shiftToProject p )
    ] 

-- Need to manually update the project list

newDir :: XPConfig -> (Project -> X ()) -> X ()
newDir conf s =  directoryPrompt conf "Directory: " (\x ->
                    let fn = case takeFileName $ takeDirectory (x++"/") of
                                ('.':xs) -> xs
                                [] -> "scratch"
                                xs -> xs
                        p = Project fn x Nothing
                    in s p )
