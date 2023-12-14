import Graphics.UI.Qt
import Control.Monad.IO
import Data.List

main :: IO ()
main = do
  app <- newApp
  execApp (app `withApp` [])

  data Chat = Chat {
  chatId :: Int,
  chatName :: String,
  chatWindow :: Window
}

createChatWindow :: (String -> IO ()) -> IO (Chat -> IO ())
createChatWindow handle = do
  window <- newWindow 0 "Chat Window" Nothing False (Just (chatName -> do
    button <- newPushButton (chatName ++ " Button") Nothing (Just $ do
      clickCaught (return (Chat 0 chatName window))
      (return ())
    )
    (return ())
  )
  (return ())

  sendMessage :: Chat -> String -> IO ()
sendMessage chat message = do

updateChatWindow :: Chat -> [String] -> IO ()
updateChatWindow chat messages = do
runChat :: IO ()
runChat = do
  chat <- newChat
  createChatWindow (sendMessage chat)
  (forever $ do
    (events <- waitEvents)
    case events of
      (EventReceived (msg@(Message _ _ _ _ _)) -> do
        updateChatWindow chat messages
      _ -> return ())
  )