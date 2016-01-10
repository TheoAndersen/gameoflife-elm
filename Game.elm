module Game where

type alias World = Int
type alias Model = World

type Action = Tick
            

initModel : Model
initModel =
  12

update : Action -> Model -> Model
update action model =
  model
