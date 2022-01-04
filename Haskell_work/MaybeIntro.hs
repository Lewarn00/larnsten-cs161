-- Lewis Arnsten

-- Exercize 8.1

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing  = Nothing
mapMaybe f (Just x) = Just $ f x

-- A function to apply mapMaybe to every element of a list.
mapListMaybe :: (a -> b) -> [Maybe a] -> [Maybe b]
mapListMaybe f [] = []
mapListMaybe f n = map (mapMaybe f) n

-- Exercize 8.3

applyMaybeFancy :: (a -> Maybe b) -> Maybe a -> Maybe b
applyMaybeFancy _ Nothing  = Nothing
applyMaybeFancy f (Just x) = f x

andThenMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
andThenMaybe = flip applyMaybeFancy

-- Implementing the join function in terms of andThenMaybe
joinMaybe :: Maybe (Maybe a) -> Maybe a
joinMaybe = flip andThenMaybe id
-- joinMaybe (Maybe (Maybe a)) = andThenMaybe (Maybe (Maybe a)) id