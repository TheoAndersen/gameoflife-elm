module Game where

type Cell = Alive | Empty
type alias World = List (List Cell)
type alias Model = World

type Action = Tick
            

initModel : Int -> Model
initModel size =
  List.repeat 3 (List.concat (List.repeat size [Empty]))
  
update : Action -> Model -> Model
update action model =
  model
