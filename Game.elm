module Game where

import Html exposing (..)
import Html.Attributes exposing (..)
import Time

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
  |> List.repeat size


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
drawCell : Cell -> Html
drawCell cell =
  let
    cellFillColor =
      if cell == Alive
         then "black"
         else "white"
  in      
    div
    [
     style [("border", "1px solid black")
           ,("width", "6px")
           ,("height","6px")
           ,("background-color", cellFillColor)
           ,("display", "block")
           ,("float", "left")
           ]
    ]
  [text "  "]
  
view : Model -> Html
view model =
  div
  []
  (model
   |> indexMap2Location(\_ cell -> drawCell cell)
   |> List.intersperse ([div[style [("clear", "both")]][]])
   |> List.concat)

ticker : Signal Action
ticker =
  Signal.map (always Tick) (Time.fps 50)

input : Signal Action
input =
  Signal.mergeMany [ticker]
  
model : Signal Model
model =
  Signal.foldp update (initialModel) ticker

initialModel : Model
initialModel =
  (initModel 100)
  |> indexMap2Location (\loc cell ->
      if(loc.row > 24 && loc.row < 30) &&
        (loc.col > 24 && loc.col < 30)
      then Alive
      else Empty
    )
  
main : Signal Html
main = Signal.map view model






