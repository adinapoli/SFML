import SFML.Audio
import SFML.Graphics.Color
import SFML.Graphics hiding (
  display,
  waitEvent,
  createSprite,
  clearRenderWindow,
  drawRectangle,
  createRenderWindow)
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
    wnd <- createRenderWindow videoMode title [SFDefaultStyle] ctxSettings
    rect <- drawRectangleOfSize (Vec2f 30 30)
    io $ setFillColor rect red
    loop wnd rect


--------------------------------------------------------------------------------
isCloseEvt (Just SFEvtClosed) = True
isCloseEvt _ = False


--------------------------------------------------------------------------------
loop :: RenderWindow -> RectangleShape -> SFML ()
loop wnd rect = do
    clearRenderWindow wnd blue
    drawRectangle wnd rect $ Just (renderStates { transform = translation 460 40 })
    display wnd
    quit <- isCloseEvt <$> waitEvent wnd
    unless quit $ loop wnd rect
