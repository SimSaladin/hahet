module Main where

import Hahet
import Manifests

-- todos
-- - inotify limits
-- - networking
-- - snd alsa conf
-- - /etc/systemd/system/auto-tunnel-functor.service
--
-- - pacman
--  * mirrorlist

main :: IO ()
main = do
    app     <- configure TestConf conf
    results <- apply app
    putStrLn "------- applied -------"
    putStrLn $ prettyPrintResults results
