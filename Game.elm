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


indexMap2Location : (Location -> a -> b) -> List (List a) -> List (List b)
indexMap2Location func list2 =
  list2
  |> List.indexedMap (\row subList ->
                        subList
                        |> List.indexedMap (\col cell ->
                                              func {row = row, col = col} cell
                                           )
                  )

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
      |> List.concat
      |> List.sum
  
update : Action -> Model -> Model
update action model =
  let
    cellAfterNextTick model neigbours state =
      if (state == Alive && neigbours > 1 && neigbours < 4) ||
         (state == Empty && neigbours == 3) then
        Alive
      else
        Empty
  in
  model
  |> indexMap2Location (\loc cell ->
                          cellAfterNextTick model (numberOfNeigbours model loc) cell
                       )

view : Model -> Html
view model =
  div[][]






