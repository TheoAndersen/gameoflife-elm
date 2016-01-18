import Task
import Console
import Graphics.Element exposing (Element)
--import Html exposing (..)
import ElmTest exposing (..)
import Game exposing (Model, initModel, update, Action
                     , numberOfNeigbours, view, Location
                     , indexMap2Location
                     , groupByOccurenceOfExactly)

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
                  oneLivingCellModel = [(Location 1 1)]
                in
                  (assertEqual (update Game.Tick (Model oneLivingCellModel 3)) (Model [] 3))
               )
        , test "One living cell with one neighbour die in next tick"
               (
                let
                  oneLivingCellModel = [(Location 0 1)
                                       ,(Location 1 1)
                                       ]

                in
                  (assertEqual (update Game.Tick (Model oneLivingCellModel 3)) (Model [] 3))
               )
        , test "One living cell with two neighbours live in next tick"
               (
                let
                  before = [(Location 0 1)  -- -x-
                           ,(Location 1 1)  -- -x-
                           ,(Location 2 1)  -- --x
                           ]

                  after = [(Location 1 1)   -- ---
                                            -- -1-
                                            -- ---
                          ]
                in
                  (assertEqual (update Game.Tick (Model before 3)) (Model after 3))
               )
        , test "One living cell with three neighbours live in next tick"
               (
                let
                  before = [(Location 0 0)
                           ,(Location 0 2)  -- x-x
                           ,(Location 1 1)  -- -x-
                           ,(Location 2 1)  -- -x-
                           ]

                  after = [--(Location 0 1)
--                          ,(Location 1 0)   -- ---
                             (Location 1 1)   -- -x-
  --                        ,(Location 1 2)   -- ---
                          ]
                in
                  (assertEqual (update Game.Tick (Model before 3)) (Model after 3))
               )
          , test "A living cell with four neighbours die, as by overpopulation"
               (
                let
                  before = [(Location 0 0)
                           ,(Location 0 2)
                           ,(Location 1 1)
                           ,(Location 2 0)
                           ,(Location 2 2)
                           ]
                  after = [(Location 0 1)
                          ,(Location 1 0)
                          ,(Location 1 2)
                          ,(Location 2 1)
                          ]
                in
                  (assertEqual (update Game.Tick (Model before 3)) (Model after 3))
               )

        , test "A dead cell with exactly three live neighbours becomes a live cell, as by reproduction"
               (
                let
                  before = [(Location 0 0)
                           ,(Location 1 1)
                           ,(Location 2 0)
                           ]
                  
                  after = [(Location 1 0)
                          ,(Location 1 1)
                          ]

                in
                  (assertEqual (update Game.Tick (Model before 3)) (Model after 3))
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
                  assertEqual (emptyWorld 3) (Model [] 3)
               )
         , test "numberOfNeigbours with 1 neigbour"
                (
                 let
                   aliveCells = [(Location 2 0)]
                 in
                   (assertEqual 1 (numberOfNeigbours aliveCells loc1p1))
                )
         , test "numberOfNeigbours with 2 neigbours"
                (
                 let
                   world = [(Location 1 0), (Location 2 1)]
                 in
                   (assertEqual (numberOfNeigbours world loc1p1) 2)
                )
         , test "numberOfNeigbours with 5 neigbours"
                (
                 let
                   world = [(Location 0 0)
                           ,(Location 0 2)
                           ,(Location 1 0) --  x-x
                           ,(Location 2 0) --  x*-
                           ,(Location 2 2) --  x-x
                           ]
                 in
                   (assertEqual (numberOfNeigbours world loc1p1) 5)
                )
         , test "numberOfNeigbours with 1 neigbours other location"
                (
                 let
                   world = [(Location 0 0)
                           ,(Location 0 1)
                           ,(Location 0 2)
                           ,(Location 2 0)
                           ,(Location 2 2)
                           ]
                 in
                   (assertEqual 1 (numberOfNeigbours world (Location 0 0)))
                )

         , test "numberOfNeigbours dosen't take own cell into account"
                (
                 let
                   world = [(Location 1 1)
                           ,(Location 2 2)
                           ]
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
                   after = [[(elem 0 0 "a"), (elem 0 1 "b"), (elem 0 2 "c")]
                           ,[(elem 1 0 "d"), (elem 1 1 "e"), (elem 1 2 "f")]
                           ,[(elem 2 0 "g"), (elem 2 1 "h"), (elem 2 2 "i")]]
                 in
                   (assertEqual after (indexMap2Location (\loc val -> (loc, val)) before))
                )
         , test "groupByOccurenceOfExactly"
                (
                 (assertEqual [2] (groupByOccurenceOfExactly 3 [1, 2, 3, 2, 3, 2]))
                )
                  

        ]


main : Element
main =
  elementRunner tests


--FOR RUNNING IN CONSOLE

port runner : Signal (Task.Task x ())
port runner =
  Console.run (consoleRunner tests)
