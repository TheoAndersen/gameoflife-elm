module Game exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (..)
import List.Extra exposing (group)
import Html.App as Html

type Cell = Alive | Empty
type alias Location = { row : Int
                      , col : Int
                      }
type alias Model = { alive: AliveCells
                   , size: Int
                   }
type alias AliveCells = List Location
                 
type Action = NoOp | Tick | Press Location
            
initModel : Int -> Model
initModel size = {alive = []
                 ,size = size
                 }

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

tick : Model -> Model
tick model =
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
 
update : Action -> Model -> (Model, Cmd Action)
update action model =
  case action of
    NoOp -> (model, Cmd.none)
    Tick -> ((tick model), Cmd.none)
    Press location ->
      ({ model | alive = model.alive ++ [location] }
         , Cmd.none)


drawCell : Location -> Cell -> Html Action
drawCell location cell =
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
     ,onClick (Press location)
    ]
  [text "  "]
  
view : Model -> Html Action
view model =
  div
  []
  (
   [0..model.size]
   |> List.map (\row ->
                  
                   [0..model.size]
                     |> List.map (\col ->
                                    if (List.member (Location row col) model.alive) then
                                      (drawCell (Location row col) Alive)
                                    else
                                      (drawCell (Location row col) Empty)
                                 )
                  
               )
   |> List.intersperse ([div[style [("clear", "both")]][]])      
   |> List.concat
  )

subscriptions : Model -> Sub Action
subscriptions model =
  every second (always Tick)

initialModel : Model
initialModel = {alive= [(Location 10 10)
                       ,(Location 11 11)
                       ,(Location 10 12)
                       ,(Location 10 13)
                       ,(Location 9 13)
                       ,(Location 20 20)
                       ,(Location 19 20)
                       ,(Location 19 21)
                       ,(Location 19 22)
                       ,(Location 30 30)
                       ,(Location 29 30)
                       ,(Location 31 30)
                       ,(Location 30 29)
                       ,(Location 30 31)], size = 70}

main =
  Html.program
  {
    init = (initialModel, Cmd.none)
  , update = update
  , view = view
  , subscriptions = \_ -> Sub.none
  }
