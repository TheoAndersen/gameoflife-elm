module Game where

import Html exposing (..)

type Cell = Alive | Empty
type alias World = List (List Cell)
type alias Model = World

type alias Location = { row : Int
                      , col : Int
                      }

type Action = Tick
            
initModel : Int -> Model
initModel size =
  [Empty]
  |> List.repeat size
  |> List.concat
  |> List.repeat 3


indexMap2Location : (Location -> a -> b) -> List (List a) -> List b
indexMap2Location func list2 =
  list2
  |> List.indexedMap (\row subList ->
                        subList
                        |> List.indexedMap (\col cell ->
                                              func {row = row, col = col} cell
                                           )
                  )
  |> List.concat

numberOfNeigbours : World -> Location -> Int
numberOfNeigbours world location = 
  let
    isNeigbour ownLocation cellLocation cell =
      if (cellLocation.col >= ownLocation.col - 1) &&
         (cellLocation.col <= ownLocation.col + 1) &&
         (cellLocation.row >= ownLocation.row - 1) &&
         (cellLocation.row <= ownLocation.row + 1) &&
         (cell == Alive) &&
         (ownLocation /= cellLocation) then
        1
      else
        0
  in
      world
        |> indexMap2Location (isNeigbour location)
        |> List.sum
  
update : Action -> Model -> Model
update action model =
  let
    cellAfterNextTick model location state =
      let
        neigbours = (numberOfNeigbours model location)
      in
        if (state == Alive && neigbours > 1 && neigbours < 4) ||
           (state == Empty && neigbours == 3) then
          Alive
        else
          Empty
      
    handleSubList model rowIndex sublist =
      sublist
      |> List.indexedMap (\colIndex value ->
                            cellAfterNextTick model { col = colIndex, row = rowIndex} value
                         )
  in
    model
    |> List.indexedMap (model |> handleSubList)

view : Model -> Html
view model =
  div[][]






