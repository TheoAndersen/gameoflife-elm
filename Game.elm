module Game where

type Cell = Alive | Empty
type alias World = List (List Cell)
type alias Model = World

type Action = Tick
            

initModel : Model
initModel =
  [[]]

update : Action -> Model -> Model
update action model =
  initModel
