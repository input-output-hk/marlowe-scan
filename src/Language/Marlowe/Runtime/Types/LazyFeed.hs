module Language.Marlowe.Runtime.Types.LazyFeed(LazyFeed, consToLazyFeed, errorToLazyFeed, emptyLazyFeed,
                                               foldThroughLazyFeed, fromExceptTIO, prependListToLazyFeed,
                                               fromIO, unConsLazyFeed) where
import Control.Monad.Except (ExceptT, runExceptT)

newtype LazyFeed a = LazyFeed (IO (LazyResult a))

data LazyResult a = EmptyFeed
                  | NonEmptyFeed a (LazyFeed a)
                  | ErrorReading String

emptyLazyFeed :: LazyFeed a
emptyLazyFeed = LazyFeed $ return EmptyFeed

consToLazyFeed :: a -> LazyFeed a -> LazyFeed a
consToLazyFeed h c = LazyFeed $ do return $ NonEmptyFeed h c

fromIO :: IO (LazyFeed a) -> LazyFeed a
fromIO h = LazyFeed $ do LazyFeed x <- h
                         x

fromExceptTIO :: ExceptT String IO (LazyFeed a) -> LazyFeed a
fromExceptTIO h = fromIO $ do x <- runExceptT h
                              return $ either errorToLazyFeed id x


                     

errorToLazyFeed :: String -> LazyFeed a
errorToLazyFeed str = LazyFeed $ do return $ ErrorReading str

prependListToLazyFeed :: [a] -> LazyFeed a -> LazyFeed a
prependListToLazyFeed t c = foldr consToLazyFeed c t

unConsLazyFeed :: LazyFeed a -> IO (Either String (Maybe (a, LazyFeed a)))
unConsLazyFeed (LazyFeed lf) = do x <- lf
                                  return $ case x of
                                             EmptyFeed -> Right Nothing
                                             NonEmptyFeed y c -> Right $ Just (y, c)
                                             ErrorReading str -> Left str

foldThroughLazyFeed :: (Maybe a -> Either b (b -> b)) -> LazyFeed a -> IO (Either String b)
foldThroughLazyFeed f lf = do res <- unConsLazyFeed lf
                              case res of
                                Left err -> return $ Left err
                                Right (Just (x, c)) -> case f (Just x) of
                                                         Left b -> return $ Right b
                                                         Right fbb -> do r <- foldThroughLazyFeed f c
                                                                         case r of
                                                                           Left s -> return $ Left s
                                                                           Right b -> return $ Right (fbb b)
                                Right Nothing -> case f Nothing of
                                                   Right _ -> error "Function in foldThroughLazyFeed returned Right for Nothing"
                                                   Left x -> return $ Right x


