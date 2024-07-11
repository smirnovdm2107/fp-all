module Main(main) where

import           System.Console.Haskeline

import           Data.Functor ()

import           Data.Void                (Void)
import           HW5.Base

import           HW5.Evaluator
import           HW5.Parser
import           HW5.Pretty
import           Text.Megaparsec.Error    (ParseErrorBundle)

import           Data.Set                 (Set, fromList)
import           HW5.Action
import qualified Data.Text as T
import Control.Exception ()
import Control.Monad ()
import Control.Monad.IO.Class

unwrap :: Either (ParseErrorBundle String Void) (IO (Either HiError HiValue)) -> IO (Either HiError HiValue)
unwrap e = case e of
    Left l -> return $ Right $ HiValueString $ T.pack $ show l
    Right r -> r 

permissions :: Set HiPermission
permissions = fromList [AllowRead, AllowWrite, AllowTime]

main :: IO ()
main = runInputT defaultSettings loop
    where
        loop :: InputT IO ()
        loop = do
           minput <- getInputLine "% "
           case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just input -> do
                 v <- liftIO $ unwrap (parse input >>= (\x -> return $ runHIO (eval x) permissions))
                 case v of
                    Left l -> outputStrLn $ show l
                    Right r -> outputStrLn $ show $ prettyValue r
           loop
