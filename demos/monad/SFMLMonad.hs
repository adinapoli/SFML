import SFML.Audio
import SFML.Graphics hiding (display, waitEvent, createRenderWindow)
import SFML.Window hiding (waitEvent, display)
import Control.Monad.SFML
import Control.Monad
import Control.Applicative

import Foreign.Ptr (nullPtr)

import Paths_sfml_demos


--------------------------------------------------------------------------------
main = runSFML $ do
    let ctxSettings = Just $ ContextSettings 24 8 0 1 2
        videoMode = VideoMode 640 480 32
        title = "SFML Monad Demo"
    wnd <- createRenderWindow videoMode title [SFDefaultStyle] Nothing
    loop wnd


--------------------------------------------------------------------------------
isCloseEvt (Just SFEvtClosed) = True
isCloseEvt _ = False


--------------------------------------------------------------------------------
loop :: RenderWindow -> SFML ()
loop wnd = do
    display wnd
    quit <- isCloseEvt <$> waitEvent wnd
    unless quit $ loop wnd
