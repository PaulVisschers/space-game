import Network.Channel
import Control.Monad
import Common

main = withSockets $ do
  s <- listen 3000
  ch <- accept s
  forever $ do
    x <- receive ch
    print (x :: ClientMessage)