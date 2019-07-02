module Network.Compat where

import Network.Socket
import Network.BSD (getProtocolNumber)
import System.IO (Handle, IOMode (..))

import qualified Control.Exception as Exception

connectTo :: String             -- Hostname
          -> PortNumber         -- Port Identifier
          -> IO Handle          -- Connected Socket
connectTo host port = do
    proto <- getProtocolNumber "tcp"
    let hints = defaultHints { addrFlags = [AI_ADDRCONFIG]
                             , addrProtocol = proto
                             , addrSocketType = Stream }
    addrs <- getAddrInfo (Just hints) (Just host) (Just serv)
    firstSuccessful "connectTo" $ map tryToConnect addrs
  where
  serv = show port

  tryToConnect addr =
    Exception.bracketOnError
        (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
        close  -- only done if there's an error
        (\sock -> do
          connect sock (addrAddress addr)
          socketToHandle sock ReadWriteMode
        )

-- Returns the first action from a list which does not throw an exception.
-- If all the actions throw exceptions (and the list of actions is not empty),
-- the last exception is thrown.
-- The operations are run outside of the catchIO cleanup handler because
-- catchIO masks asynchronous exceptions in the cleanup handler.
-- In the case of complete failure, the last exception is actually thrown.
firstSuccessful :: String -> [IO a] -> IO a
firstSuccessful caller = go Nothing
  where
  -- Attempt the next operation, remember exception on failure
  go _ (p:ps) =
    do r <- tryIO p
       case r of
         Right x -> return x
         Left  e -> go (Just e) ps

  -- All operations failed, throw error if one exists
  go Nothing  [] = ioError $ userError $ caller ++ ": firstSuccessful: empty list"
  go (Just e) [] = Exception.throwIO e

-- Version of try implemented in terms of the locally defined catchIO
tryIO :: IO a -> IO (Either Exception.IOException a)
tryIO m = Exception.catch (fmap Right m) (return . Left)
