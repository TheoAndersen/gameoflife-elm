module Game where

import Html exposing (..)
import Html.Attributes exposing (..)
import Time
import List.Extra exposing (group)

type Cell = Alive | Empty
type alias Location = { row : Int
                      , col : Int
                      }
type alias Model = { alive: AliveCells
                   , size: Int
                   }
type alias AliveCells = List Location
                 
type Action = Tick
            
initModel : Int -> Model
initModel size = {alive = []
                 ,size = size
                 }

indexMap2Location : (Location -> a -> b) -> List (List a) -> List (List b)
indexMap2Location func list2 =
  list2
  |> List.indexedMap (\row subList ->
                        subList
                        |> List.indexedMap (\col cell ->
                                              func {row = row, col = col} cell
                                           )
                  )

groupByOccurenceOfExactly : Int -> List Location -> List Location
groupByOccurenceOfExactly minSize list =
  list
  |> List.sortBy (\loc -> toString loc.row ++ toString loc.col)
  |> group
  |> List.filter (\value -> ((List.length value) == minSize))
  |> List.map (\val -> (List.take 1 val))
  |> List.concat

removeDuplicates : List Location -> List Location
removeDuplicates list =
  list
  |> List.sortBy (\loc -> toString loc.row ++ toString loc.col)
  |> group
  |> List.map (\val -> (List.take 1 val))
  |> List.concat

numberOfNeigbours : AliveCells -> Location -> Int
numberOfNeigbours alive location = 
  alive
  |> List.filter (\loc -> (location.row >= loc.row - 1) &&
                          (location.row <= loc.row + 1) &&
                          (location.col >= loc.col - 1) &&
                          (location.col <= loc.col + 1) &&
                          (location /= loc)
                 )
  |> List.map (\_ -> 1)
  |> List.sum

update : Action -> Model -> Model
update action model =
  let
    aliv =
      List.filter (\loc ->
                     (numberOfNeigbours model.alive loc) == 2 ||
                     (numberOfNeigbours model.alive loc) == 3 
                  ) model.alive
    reproduced =
      model.alive
      |> List.map (\loc ->
                     [(Location (loc.row-1) (loc.col-1)), 
                     (Location (loc.row-1) (loc.col)), 
                     (Location (loc.row-1) (loc.col+1)), 
                     (Location (loc.row)   (loc.col-1)), 
                     (Location (loc.row)   (loc.col+1)), 
                     (Location (loc.row+1) (loc.col-1)), 
                     (Location (loc.row+1) (loc.col)), 
                     (Location (loc.row+1) (loc.col+1))]
                  )
      |> List.concat
      |> groupByOccurenceOfExactly 3

    total =
      aliv ++ reproduced
      |> removeDuplicates
  in
    {alive = total, size=model.size}

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
  (
   (drawCell Empty)
   |> List.repeat model.size
   |> List.repeat model.size
   |> List.intersperse ([div[style [("clear", "both")]][]])      
   |> List.concat
  )

ticker : Signal Action
ticker =
  Signal.map (always Tick) (Time.fps 10)

input : Signal Action
input =
  Signal.mergeMany [ticker]
  
model : Signal Model
model =
  Signal.foldp update initialModel ticker

initialModel : Model
initialModel = {alive= [], size = 100}
  
main : Signal Html
main = Signal.map view model






