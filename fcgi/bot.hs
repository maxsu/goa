 
import GOA
import Network.FastCGI
import Data.List (intersperse)

form :: String
form = concat ["<html><body><form>",
               "<input type='text' name='cmd' /><br />",
               "<input type='submit' />",
               "</form></body></html>"]

maybeM :: Monad m => m b -> (a -> b) -> m (Maybe a) -> m b
maybeM d f optional = do o <- optional
                         case o of
                           Nothing -> d
                           Just v  -> return (f v)

cgiMain :: CGI CGIResult
cgiMain = do remote   <- maybeM remoteAddr         id remoteHost
             user     <- maybeM (return "Unknown") id remoteUser
             agent    <- maybeM (return "Unknown") id $ requestHeader userAgent
             rawCmd   <- queryString
             maybeCmd <- getInput "cmd"
             cmd      <- case maybeCmd of
                           Nothing -> return Nothing
                           Just c  -> do r <- liftIO $ query c []
                                         return $ Just r
             let response = maybe [] (concat . intersperse "\n") cmd
             logCGI (show (user++"@"++remote, agent, rawCmd, cmd, response))
             case maybeCmd of
               Nothing -> output form
               Just _
                 -- we suspect that when the script has no output
                 -- the web server will think it has timed out
                 -- here we force some output in the hopes preventing that bug
                 | null response -> output " "
                 | otherwise     -> output response
  where
  userAgent = "User-Agent"

main :: IO ()
main = do setLambdabotHome "."
          setLambdabotFlags "--restricted"
          wakeup
          runFastCGI (handleErrors cgiMain)
