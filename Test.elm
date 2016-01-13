--import Task
--import Console
import Graphics.Element exposing (Element)
import ElmTest exposing (..)
import Game exposing (Model, initModel, update, Action, numberOfNeigbours)

emptyWorld : Int -> Game.Model
emptyWorld size = initModel size

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
                  before = [[Game.Empty, Game.Empty, Game.Alive]
                           ,[Game.Empty, Game.Alive, Game.Empty]
                           ,[Game.Empty, Game.Alive, Game.Empty]]
                  after = [[Game.Empty, Game.Empty, Game.Empty]
                          ,[Game.Empty, Game.Alive, Game.Empty]
                          ,[Game.Empty, Game.Empty, Game.Empty]]

                in
                  (assertEqual (update Game.Tick before) after)
               )

          
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
                   world = [[Game.Empty, Game.Alive, Game.Empty]
                           ,[Game.Empty, Game.Empty, Game.Empty]
                           ,[Game.Empty, Game.Empty, Game.Empty]]
                 in
                   (assertEqual (numberOfNeigbours world 1 1) 1)
                )
         , test "numberOfNeigbours with 2 neigbours"
                (
                 let
                   world = [[Game.Empty, Game.Alive, Game.Empty]
                           ,[Game.Empty, Game.Empty, Game.Alive]
                           ,[Game.Empty, Game.Empty, Game.Empty]]
                 in
                   (assertEqual (numberOfNeigbours world 1 1) 2)
                )
         , test "numberOfNeigbours with 5 neigbours"
                (
                 let
                   world = [[Game.Alive, Game.Alive, Game.Empty]
                           ,[Game.Empty, Game.Empty, Game.Alive]
                           ,[Game.Alive, Game.Empty, Game.Alive]]
                 in
                   (assertEqual (numberOfNeigbours world 1 1) 5)
                )
         , test "numberOfNeigbours dosen't take own cell into account"
                (
                 let
                   world = [[Game.Empty, Game.Empty, Game.Empty]
                           ,[Game.Empty, Game.Alive, Game.Empty]
                           ,[Game.Empty, Game.Empty, Game.Alive]]
                 in
                   (assertEqual (numberOfNeigbours world 1 1) 1)
                )         
                  

        ]


main : Element
main =
  elementRunner tests


--FOR RUNNING IN CONSOLE

--port runner : Signal (Task.Task x ())
--port runner =
--  Console.run (consoleRunner tests)
