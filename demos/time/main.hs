import SFML.System


main = do
    clock <- createClock
    loop clock 3
    destroy clock


loop clock 0 = return ()
loop clock n = do
    sfSleep $ seconds 0.150
    time <- getElapsedTime clock
    if asSeconds time >= 1
        then putStrLn "tick" >> restartClock clock >> loop clock (n-1)
        else loop clock n

