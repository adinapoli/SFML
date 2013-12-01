import SFML.Audio
import SFML.Graphics hiding (createRenderWindow)
import SFML.Window
import Control.Monad.SFML

import Foreign.Ptr (nullPtr)

import Paths_sfml_demos


main = runSFML $ do
    let ctxSettings = Just $ ContextSettings 24 8 0 1 2
        videoMode = VideoMode 640 480 32
        title = "SFML Monad Demo"
    wnd <- createRenderWindow videoMode title [SFDefaultStyle] ctxSettings
    loop wnd


loop :: RenderWindow -> SFML ()
loop wnd = do
    liftIO $ display wnd
    evt <- liftIO $ waitEvent wnd
    case evt of
        Nothing -> return ()
        Just SFEvtClosed -> return ()
        _ -> loop wnd
