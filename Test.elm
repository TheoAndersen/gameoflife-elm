--import Task
--import Console
import Graphics.Element exposing (Element)
import ElmTest exposing (..)
import Game exposing (Model, initModel, update, Action)

tests : Test
tests =
  suite "beginning"
        [
          test "No living cells have no living cells in the next tick"
               (assertEqual (update Game.Tick (initModel 2)) (initModel 2))
        , test "One living cell with no neighbours die in next tick"
               (
                let
                  oneLivingCellModel = initModel 2
                in
                  (assertEqual (update Game.Tick oneLivingCellModel) (initModel 2))
               )
        , test "initModel can init a 3x3 celled world"
               (
                let
                  expectedWorld = [[Game.Empty, Game.Empty, Game.Empty]
                                  ,[Game.Empty, Game.Empty, Game.Empty]
                                  ,[Game.Empty, Game.Empty, Game.Empty]]
                in
                  (assertEqual (initModel 3) expectedWorld)
               )
        ]


main : Element
main =
  elementRunner tests


--FOR RUNNING IN CONSOLE

--port runner : Signal (Task.Task x ())
--port runner =
--  Console.run (consoleRunner tests)
