# Minimal live coding library for model-view-event-update applications.

Minimal live coding library.
It can store model of the model-view-event-update applications.
It uses `FromJSON` and `ToJSON` with default value
for absent data elements, and `Generic` parser otherwise:

```haskell
import Control.Restartable.Initially(initially, Initial)

data MyModel = Model { timestamp :: UTCTime }

instance ToJSON MyModel where
instance FromJSON MyModel where
  parseJSON = initially myDefaultValue
instance Initial MyModel
```

Then in the main module use:

```
import Control.Restartable.Checkpoint(restartable, Ending(..))

main = restartable "checkpoint.save" myMain

myMain :: MyModel -> IO (MyModel, Ending)
```

It is preferred to attach initial value to every substructure,
so whenever it fails to parse the remaining part of the state is still parsed correctly.

