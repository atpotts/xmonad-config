module WindowNames where

import Graphics.X11.Types
import XMonad
import XMonad.StackSet (tag)
import XMonad.Actions.WindowBringer
import XMonad.Util.NamedWindows

import qualified Data.Map as M

windowNames :: X (M.Map String Window)
windowNames = windowMap' windowNamer

windowNamer :: WindowSpace -> Window -> X String
windowNamer = anyWindowNamer . Just

anyWindowNamer :: Maybe WindowSpace -> Window -> X String
anyWindowNamer wss w = do
                app   <- runQuery className w
                title <- getName w
                return $ app++": "
                       ++show title
                       ++case wss of
                              Just ws -> " ("++tag ws++")"
                              _ -> ""

