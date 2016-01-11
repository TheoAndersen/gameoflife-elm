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
  let
    emptyIfAlive _ value =
      Empty
    emptyAllAlives list =
      List.indexedMap emptyIfAlive list
  in
    List.map emptyAllAlives model
