module Test.Control.Comonad.Cofree.Zipper where

import Prelude

import Control.Comonad.Cofree (Cofree, (:<))
import Control.Comonad.Cofree.Zipper (Trace(..), Zipper(..), fromCofree, goDown, goRight, goUp)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Maybe.First (First(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Control.Extend (duplicate)
import Control.Comonad (extract)
import Data.Array (fromFoldable)

type MaybeCofree a = Cofree Maybe a

spec :: Spec Unit
spec = describe "Cofree.Zipper" do
    it "creates a Zipper for a simple cofree" do
        let
            expected = Zipper
                { focus: 1 :< First Nothing
                , trace: Nil
                , left: First Nothing
                , right: First Nothing
                }
        fromCofree (1 :< First Nothing) `shouldEqual` expected
    it "can go down" do
        (1 :< (First (Just (2 :< First Nothing))))
            # fromCofree
            # goDown
            # shouldEqual $ Just $ Zipper
            { focus: 2 :< First Nothing
            , trace:
                  Trace
                      { left: First Nothing
                      , focus: 1
                      , right: First Nothing
                      } : Nil
            , left: First Nothing
            , right: First Nothing
            }
    it "can go down" do
        Zipper
            { focus: 2 :< First Nothing
            , trace:
                  Trace
                      { left: First Nothing
                      , focus: 1
                      , right: First Nothing
                      } : Nil
            , left: First Nothing
            , right: First Nothing
            }
            # goUp
            # shouldEqual $ Just $ fromCofree
            (1 :< (First (Just (2 :< First Nothing))))
    it "can go right" do
        let
            maybeZipper = do
                let zipper = fromCofree (1 :< [ 2 :< [], 3 :< [] ])
                down <- goDown zipper
                goRight down
        maybeZipper `shouldEqual` (Just <<< Zipper)
            { focus: 3 :< []
            , trace: Trace { left: [], focus: 1, right: [] } : Nil
            , left: [ 2 :< [] ]
            , right: []
            }
    it "can duplicate" do
        let zipper = fromCofree (1 :< [])
        duplicate zipper `shouldEqual` Zipper
            { focus: fromCofree (1 :< []) :< []
            , trace: Nil
            , left: []
            , right: []
            }
    it "can extract" do
        let zipper = fromCofree (1 :< [])
        extract zipper `shouldEqual` 1

    it "duplicate >>> extract = id" do
        let zipper = fromCofree (1 :< [])
        (duplicate >>> extract) zipper `shouldEqual` zipper

    it "can fold" do
        let
            array :: Array Int
            array = (1 :< [ 2 :< [], 3 :< [] ])
                # fromCofree
                # fromFoldable
        array `shouldEqual` [ 1, 2, 3 ]

