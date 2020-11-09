# Minimal live coding library for model-view-event-update applications.

Minimal live coding library.
It can store model of the model-view-event-update applications.
It uses `FromJSON` and `ToJSON` with default value
for absent data elements, and `Generic` parser otherwise:

```haskell
import Initially(initially, Initial)
data MyModel = Model { timestamp :: UTCTime }
instance ToJSON MyModel where
instance FromJSON MyModel where
  parseJSON = initially myDefaultValue
instance Initial MyModel

main = restartable
```

It is preferred to attach initial value to every substructure,
so whenever it fails to parse the remaining part of the state is still parsed correctly.

