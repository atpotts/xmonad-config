module Projects where

import MyPrompts

import Graphics.X11.Types

import XMonad
import XMonad.Prompt
import XMonad.Prompt.Window (XWindowMap)
import XMonad.Actions.DynamicProjects as P

dynamicProjects = P.dynamicProjects

projects :: [Project]
projects =
    [Project { projectName = "scratch",
               projectDirectory = "~/",
               projectStartHook = Just $ do spawn "firefox"},
     Project { projectName = "haskell-spock",
               projectDirectory = "~/Desktop/spock",
               projectStartHook = Just $ do spawn "emacsclient-snapshot -c"},
     Project { projectName = "dotfiles",
               projectDirectory = "~/.dot",
               projectStartHook = Just $ do spawn "emacsclient-snapshot -c"
                                            spawn "urxvt" }
     ]

projectPrompts :: XPConfig -> PromptList 
projectPrompts conf = [
    (["M-p"],   "Go to project",            switchProjectPrompt conf),  
    (["M-S-p"], "Send to project",          shiftToProjectPrompt conf), 
    (["M-'"],   "Rename project",           renameProjectPrompt conf),  
    (["M-S-'"], "Change project directory", changeProjectDirPrompt conf)
    ] 
