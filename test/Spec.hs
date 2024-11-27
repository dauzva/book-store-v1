{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Test.Tasty.QuickCheck as QC
import Control.Concurrent ( Chan, newChan, writeChan, readChan, forkIO )
import Control.Monad.IO.Class ( liftIO )
import Lib1 qualified
import Lib2 qualified
import Lib3 qualified
import GHC.Generics (Generic)
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, atomically)
import Test.QuickCheck.Monadic (monadicIO, assert)
import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import System.IO (writeFile, readFile)

-- Main test entry point
main :: IO ()
main = defaultMain tests

-- Test tree that includes unit tests and property tests
tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

-- Unit tests for functionality
unitTests :: TestTree
unitTests = testGroup "Book Store Tests" [
    testCase "Empty input" $
        Lib2.parseQuery "" @?= Left "Invalid command",
    
    testCase "Invalid command" $
        Lib2.parseQuery "hello" @?= Left "Invalid command",
    
    testCase "Add book - valid input" $
        Lib2.parseQuery "add \"The Hobbit\", John Tolkien, Fantasy, 1937, 29.99" @?= 
        Right (Lib2.AddQuery "The Hobbit, John Tolkien, Fantasy, 1937, 29.99"),

    testCase "Add book - invalid title" $
        Lib2.parseQuery "add The Hobbit, John Tolkien, jasdja, 1937, 29.99" @?= 
        Left "Invalid Title syntax",

    testCase "Add book - invalid author" $   
        Lib2.parseQuery "add \"The Hobbit\", John 123, jasdja, 1937, 29.99" @?= 
        Left "Invalid Author",
    
    testCase "Add book - invalid year" $
        Lib2.parseQuery "add \"The Hobbit\", John Tolkien, Fantasy, 1, 29.99" @?= 
        Left "Invalid Year",
    
    testCase "Add book - invalid genre" $
        Lib2.parseQuery "add \"The Hobbit\", John Tolkien, jasdja, 1937, 29.99" @?= 
        Left "Invalid Genre",
    
    testCase "Add book - invalid price format" $
        Lib2.parseQuery "add \"The Hobbit\", John Tolkien, Fantasy, 1937, 29.999" @?= 
        Left "Invalid Price",
    
    testCase "Remove - valid id" $
        Lib2.parseQuery "remove 1" @?= Right (Lib2.RemoveQuery "1"),
    
    testCase "Remove - invalid id format" $
        Lib2.parseQuery "remove abc" @?= Left "Invalid ID",
    
    testCase "List command" $
        Lib2.parseQuery "list" @?= Right Lib2.ListQuery
    ]


instance QC.Arbitrary Lib3.Statements where
  arbitrary = QC.oneof [ Lib3.Single <$> arbitrary
                       , Lib3.Batch <$> arbitrary ]

instance QC.Arbitrary Lib3.Command where
  arbitrary = QC.oneof [ return Lib3.LoadCommand
                       , return Lib3.SaveCommand
                       , Lib3.StatementCommand <$> arbitrary ]

instance QC.Arbitrary Lib2.Query where
  arbitrary = QC.oneof [
      Lib2.AddQuery <$> arbitrary
    , Lib2.RemoveQuery <$> arbitrary
    , return Lib2.ListQuery
    ]

propertyTests :: TestTree
propertyTests = testGroup "Lib3 Property Tests"
  [
    testCase "storage operations round-trip" $
      ioRoundTrip "Sample content for storage"
  ]

ioRoundTrip :: String -> IO ()
ioRoundTrip content = do
  ioChan <- newChan
  _ <- forkIO $ Lib3.storageOpLoop ioChan
  saveResponseChan <- newChan
  loadResponseChan <- newChan
  writeChan ioChan (Lib3.Save content saveResponseChan)
  _ <- readChan saveResponseChan
  writeChan ioChan (Lib3.Load loadResponseChan)
  loadedContent <- readChan loadResponseChan
  loadedContent @?= content++"\n"