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
windowNamer ws w = anyWindowNamer (Just (id,ws)) id id w

type Formatter = String -> String

anyWindowNamer :: Maybe (Formatter,WindowSpace)
               -> Formatter -> Formatter -> Window -> X String
anyWindowNamer wss fa ft w = do
                app   <- runQuery className w
                title <- getName w
                return $ fa app++": "
                       ++ft (show title)
                       ++case wss of
                              Just (fws, ws) -> " ("++fws (tag ws)++")"
                              _ -> ""
    
