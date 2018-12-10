module GreetIfCool3 where

    greetIfCool :: String -> IO ()
    greetIfCool coolness =
        case cool of
            True ->
                putStrLn "eyyyy. What's shagin'?"
            False ->
                putStrLn "pshhhh."
        where cool = coolness == "downright frost yo"