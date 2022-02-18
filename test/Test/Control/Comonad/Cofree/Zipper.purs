module Test.Control.Comonad.Cofree.Zipper where

import Prelude

import Control.Comonad.Cofree.Zipper (Zipper(..), fromCofree)
import Control.Comonad.Cofree (Cofree, (:<))
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Data.Maybe.First (First(..))

type MaybeCofree a = Cofree Maybe a

spec :: Spec Unit
spec = describe "Cofree.Zipper" do
    it "creates a Zipper for a simple cofree" do
        let expected = Zipper
                { extract : 1 :< First Nothing
                , up : Nil
                , left : First Nothing
                , right : First Nothing
                }
        fromCofree (1 :< First Nothing) `shouldEqual` expected
