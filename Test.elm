import Task
import Console
import Graphics.Element exposing (Element)
--import Html exposing (..)
import ElmTest exposing (..)
import Game exposing (Model, initModel, update, Action
                     , numberOfNeigbours, view, Location
                     , indexMap2Location)

emptyWorld : Int -> Game.Model
emptyWorld size = initModel size

loc1p1 : Location
loc1p1 =
  { col = 1, row = 1}

tests : Test
tests =
  suite "beginning"
        [
          test "No living cells have no living cells in the next tick"
               (assertEqual (update Game.Tick (emptyWorld 2)) (emptyWorld 2))
               
        , test "One living cell with no neighbours die in next tick"
               (
                let
                  oneLivingCellModel = [[Game.Empty, Game.Empty, Game.Empty]
                                       ,[Game.Empty, Game.Alive, Game.Empty]
                                       ,[Game.Empty, Game.Empty, Game.Empty]]
                in
                  (assertEqual (update Game.Tick oneLivingCellModel) (emptyWorld 3))
               )
        , test "One living cell with one neighbour die in next tick"
               (
                let
                  oneLivingCellModel = [[Game.Empty, Game.Alive, Game.Empty]
                                       ,[Game.Empty, Game.Alive, Game.Empty]
                                       ,[Game.Empty, Game.Empty, Game.Empty]]
                in
                  (assertEqual (update Game.Tick oneLivingCellModel) (emptyWorld 3))
               )
        , test "One living cell with two neighbours live in next tick"
               (
                let
                  before = [[Game.Empty, Game.Alive, Game.Empty]
                           ,[Game.Empty, Game.Alive, Game.Empty]
                           ,[Game.Empty, Game.Alive, Game.Empty]]
                  
                  after = [[Game.Empty, Game.Empty, Game.Empty]
                          ,[Game.Alive, Game.Alive, Game.Alive]  -- two outer live on because of rule 3
                          ,[Game.Empty, Game.Empty, Game.Empty]]

                in
                  (assertEqual (update Game.Tick before) after)
               )
        , test "One living cell with three neighbours live in next tick"
               (
                let
                  before = [[Game.Alive, Game.Empty, Game.Alive]
                           ,[Game.Empty, Game.Alive, Game.Empty]
                           ,[Game.Empty, Game.Alive, Game.Empty]]
                  after = [[Game.Empty, Game.Alive, Game.Empty]
                          ,[Game.Alive, Game.Alive, Game.Alive]
                          ,[Game.Empty, Game.Empty, Game.Empty]]

                in
                  (assertEqual (update Game.Tick before) after)
               )
          , test "A living cell with four neighbours die, as by overpopulation"
               (
                let
                  before = [[Game.Alive, Game.Empty, Game.Alive]
                           ,[Game.Empty, Game.Alive, Game.Empty]
                           ,[Game.Alive, Game.Empty, Game.Alive]]
                  after = [[Game.Empty, Game.Alive, Game.Empty]
                          ,[Game.Alive, Game.Empty, Game.Alive]
                          ,[Game.Empty, Game.Alive, Game.Empty]]

                in
                  (assertEqual (update Game.Tick before) after)
               )

        , test "A dead cell with exactly three live neighbours becomes a live cell, as by reproduction"
               (
                let
                  before = [[Game.Alive, Game.Empty, Game.Empty]
                           ,[Game.Empty, Game.Alive, Game.Empty]
                           ,[Game.Alive, Game.Empty, Game.Empty]]
                  
                  after = [[Game.Empty, Game.Empty, Game.Empty]
                          ,[Game.Alive, Game.Alive, Game.Empty]
                          ,[Game.Empty, Game.Empty, Game.Empty]]

                in
                  (assertEqual (update Game.Tick before) after)
               )
--         , test "view should render game states as divs"
--           (
--            let
-- --             emptyDiv = div[][]
--              before = [[Game.Alive, Game.Empty, Game.Empty]
--                       ,[Game.Empty, Game.Alive, Game.Empty]
--                       ,[Game.Alive, Game.Empty, Game.Empty]]
                  
--              after = div[][div[][]]
                     
                            
--                           --[emptyDiv, emptyDiv, emptyDiv
--                           --,emptyDiv, emptyDiv, emptyDiv
--                           --,emptyDiv, emptyDiv, emptyDiv
--                            --]
      
--                 in
--                   (assertEqual (view before) after)
--           )
        , test "initModel can init a 3x3 celled world"
               (
                let
                  expectedWorld = [[Game.Empty, Game.Empty, Game.Empty]
                                  ,[Game.Empty, Game.Empty, Game.Empty]
                                  ,[Game.Empty, Game.Empty, Game.Empty]]
                in
                  (assertEqual (emptyWorld 3) expectedWorld)
               )
         , test "numberOfNeigbours with 1 neigbour"
                (
                 let
                   world = [[Game.Empty, Game.Empty, Game.Alive]
                           ,[Game.Empty, Game.Empty, Game.Empty]
                           ,[Game.Empty, Game.Empty, Game.Empty]]
                 in
                   (assertEqual (numberOfNeigbours world loc1p1) 1)
                )
         , test "numberOfNeigbours with 2 neigbours"
                (
                 let
                   world = [[Game.Empty, Game.Alive, Game.Empty]
                           ,[Game.Empty, Game.Empty, Game.Alive]
                           ,[Game.Empty, Game.Empty, Game.Empty]]
                 in
                   (assertEqual (numberOfNeigbours world loc1p1) 2)
                )
         , test "numberOfNeigbours with 5 neigbours"
                (
                 let
                   world = [[Game.Alive, Game.Alive, Game.Alive]
                           ,[Game.Empty, Game.Empty, Game.Empty]
                           ,[Game.Alive, Game.Empty, Game.Alive]]
                 in
                   (assertEqual (numberOfNeigbours world loc1p1) 5)
                )
         , test "numberOfNeigbours with 1 neigbours other location"
                (
                 let
                   world = [[Game.Alive, Game.Alive, Game.Alive]
                           ,[Game.Empty, Game.Empty, Game.Empty]
                           ,[Game.Alive, Game.Empty, Game.Alive]]
                 in
                   (assertEqual 1 (numberOfNeigbours world (Location 0 0)))
                )

         , test "numberOfNeigbours dosen't take own cell into account"
                (
                 let
                   world = [[Game.Empty, Game.Empty, Game.Empty]
                           ,[Game.Empty, Game.Alive, Game.Empty]
                           ,[Game.Empty, Game.Empty, Game.Alive]]
                 in
                   (assertEqual (numberOfNeigbours world loc1p1) 1)
                )
         , test "indexedMap2Location works"
                (
                 let
                   elem r c char =
                     ({row = r, col = c}, char)
                   before = [["a", "b", "c"]
                            ,["d", "e", "f"]
                            ,["g", "h", "i"]]
                   after = [(elem 0 0 "a"), (elem 0 1 "b"), (elem 0 2 "c")
                           ,(elem 1 0 "d"), (elem 1 1 "e"), (elem 1 2 "f")
                           ,(elem 2 0 "g"), (elem 2 1 "h"), (elem 2 2 "i")]
                 in
                   (assertEqual after (indexMap2Location (\loc val -> (loc, val)) before))
                )
                  

        ]


main : Element
main =
  elementRunner tests


--FOR RUNNING IN CONSOLE

port runner : Signal (Task.Task x ())
port runner =
  Console.run (consoleRunner tests)
