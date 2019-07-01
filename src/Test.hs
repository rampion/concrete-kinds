module Main where
import Data.Type.Coercion.Test
import Test.Tasty.Hspec
import Test.Tasty

main :: IO ()
main = defaultMain =<< testSpec "tests" eliminateSpec
