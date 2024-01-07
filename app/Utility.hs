module Utility where

import Control.Monad
import Data.List
import System.Random

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return

sortDesc :: (Ord a) => [a] -> [a]
sortDesc = sortBy (flip compare)

flatten :: [[a]] -> [a]
flatten = foldr1 (++)

replaceElemAt :: (Eq a) => [a] -> Int -> a -> Maybe [a]
replaceElemAt lst idx newVal = if idx > length lst - 1 
                               then Nothing
                               else let (x, _:ys) = splitAt idx lst 
                                    in Just (x ++ newVal : ys)

eitherGuard :: a -> (a ->  Bool) -> e -> Either e a
eitherGuard x p e = if p x then Right x else Left e

maybeRight :: Either e a -> Maybe a
maybeRight (Right x) = Just x
maybeRight (Left _) = Nothing

replaceOn :: (Functor f) => (a -> a -> Bool) -> a -> f a -> f a
replaceOn p v = fmap (\x -> if p x v then v else x)

whileM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m ()
whileM test act init =
   when (test init) $ act init >>= whileM test act

rolls :: RandomGen g => Int -> g -> ([Word], g)
rolls n g =  let results = take n $ drop 1 $ iterate (\(_, gg) -> uniformR (1, 6) gg) (0, g)
                 vals = map fst results
                 lastG = snd $ last results
            in (vals, lastG)