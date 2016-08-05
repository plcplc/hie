{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.Aux where

import Reflex.Dom
import qualified Data.Map as M

selectMapShallow ::
  (Reflex t, Ord k) =>
  Dynamic t (M.Map k a) ->
  k ->
  Dynamic t (Maybe a)
selectMapShallow d k =
  unsafeDynamic b e

  where

    b = pull $ do
      cur <- sample $ current d
      return (M.lookup k cur)

    e = attachWithMaybe
        (\prev m -> case (prev, M.lookup k m) of
            (Nothing, Just v) -> Just (Just v) -- constructor changed: fire.
            (Just _, Nothing) -> Just Nothing -- constructor changed: fire.
            _                 -> Nothing -- constructor hasn't changed, so don't fire.
        )
        b
        (updated d)

joinDynThroughMaybe ::
  forall t a.
  (Reflex t) =>
  Dynamic t (Maybe (Dynamic t a)) ->
  Dynamic t (Maybe a)
joinDynThroughMaybe d =
  unsafeDynamic b (leftmost [eBoth, eOuter, eInner])

  where

    b :: Behavior t (Maybe a)
    b = pull $ do
      cur <- sample $ current d
      case cur of
        Nothing -> return Nothing
        Just cur' -> Just <$> sample (current cur')

    eOuter :: Event t (Maybe a)
    eOuter = pushAlways
      (maybe (return Nothing)
      (fmap Just . sample . current))
      (updated d)

    innerMaybe :: Maybe (Dynamic t a) -> Event t (Maybe a)
    innerMaybe = (maybe never (fmap Just . updated))

    eInner :: Event t (Maybe a)
    eInner = switch (fmap innerMaybe $ current d)

    eBoth :: Event t (Maybe a)
    eBoth = coincidence (fmap innerMaybe $ updated d)
