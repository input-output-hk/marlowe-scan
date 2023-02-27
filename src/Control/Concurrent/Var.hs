module Control.Concurrent.Var
  ( Var
  , modifyVar
  , modifyVar_
  , newVar
  , readVar
  )
  where

{- This module was inspired by Neil Mitchell's Flavours of MVar blog post and
   sample code. The important take-away is to build simple, focused
   abstractions on top of the (very general) MVar module. And then possibly
   combine those patterns into solutions where concerns are clearly separated
   and simpler to reason about.

   This example is called Var and represents allowing operations on a mutable
   variable in a thread safe way.

   A usage example

      hits <- newVar 0
      forkIO $ do ...; modifyVar_ hits (+1); ...
      i <- readVar hits
      print ("HITS",i)

   https://neilmitchell.blogspot.com/2012/06/flavours-of-mvar_04.html

-}

import Control.Concurrent.MVar ( MVar, modifyMVar, modifyMVar_, newMVar, readMVar )


type Var a = MVar a

newVar :: a -> IO (Var a)
newVar = newMVar

modifyVar :: Var a -> (a -> IO (a, b)) -> IO b
modifyVar = modifyMVar

modifyVar_ :: Var a -> (a -> IO a) -> IO ()
modifyVar_ = modifyMVar_

readVar :: Var a -> IO a
readVar = readMVar
