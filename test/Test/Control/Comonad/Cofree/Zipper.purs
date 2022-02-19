module Test.Control.Comonad.Cofree.Zipper where

import Prelude

import Control.Comonad.Cofree (Cofree, (:<))
import Control.Comonad.Cofree.Zipper (Trace(..), Zipper(..), fromCofree, goDown, goUp)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Maybe.First (First(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type MaybeCofree a = Cofree Maybe a

spec :: Spec Unit
spec = describe "Cofree.Zipper" do
    it "creates a Zipper for a simple cofree" do
        let expected = Zipper
                { extract : 1 :< First Nothing
                , trace : Nil
                , left : First Nothing
                , right : First Nothing
                }
        fromCofree (1 :< First Nothing) `shouldEqual` expected
    it "can go down" do
        (1 :< (First (Just (2 :< First Nothing))))
            # fromCofree
            # goDown
            # shouldEqual $ Just $ Zipper
                { extract : 2 :< First Nothing
                , trace : Trace
                    { left: First Nothing
                    , focus: 1
                    , right: First Nothing
                    } : Nil
                , left : First Nothing
                , right : First Nothing
                }
    it "can go down" do
        Zipper
            { extract : 2 :< First Nothing
            , trace : Trace
                { left: First Nothing
                , focus: 1
                , right: First Nothing
                } : Nil
            , left : First Nothing
            , right : First Nothing
            }
            # goUp
            # shouldEqual $ Just $ fromCofree (1 :< (First (Just (2 :< First Nothing))))

