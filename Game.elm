module Game where

type Cell = Alive | Empty
type alias World = List (List Cell)
type alias Model = World

type Action = Tick
            

initModel : Int -> Model
initModel size =
  List.repeat 3 (List.concat (List.repeat size [Empty]))

numberOfNeigbours : Model -> Int -> Int -> Int
numberOfNeigbours model colIndex rowIndex = 
  let
    locationIsNotOwnLocation ownCol ownRow otherCol otherRow =
      if (ownCol == otherCol) &&
         (ownRow == otherRow) then
        False
      else
        True
    
    neigbours model colIndex rowIndex =
      List.indexedMap (\cIndex cList ->
                         List.indexedMap
                               (\rIndex value ->
                                  if (cIndex >= colIndex - 1) &&
                                     (cIndex <= colIndex + 1) &&
                                     (rIndex >= rowIndex - 1) &&
                                     (rIndex <= rowIndex + 1) &&
                                     (value == Alive) &&
                                     (locationIsNotOwnLocation cIndex rIndex colIndex rowIndex) then
                                    1
                                  else
                                    0
                               ) cList
                      ) model
  in
    List.sum (List.concat (neigbours model colIndex rowIndex))
  
update : Action -> Model -> Model
update action model =
  let
    cellAfterNextTick model colIndex rowIndex state =
      if state == Alive && numberOfNeigbours model colIndex rowIndex > 1 then
        Alive
      else
        Empty
      
    handleSubList model colIndex sublist =
      List.indexedMap (\rowIndex value ->
                         cellAfterNextTick model colIndex rowIndex value
                      ) sublist
  in
    List.indexedMap (handleSubList model) model






