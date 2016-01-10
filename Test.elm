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
               (assertEqual (update Game.Tick initModel) (initModel))
        , test "One living cell with no neighbours die in next tick"
               (
                let
                  oneLivingCellModel = [[Game.Alive]]
                in
                  (assertEqual (update Game.Tick oneLivingCellModel) (initModel))
               )
        ]


main : Element
main =
  elementRunner tests


--FOR RUNNING IN CONSOLE

--port runner : Signal (Task.Task x ())
--port runner =
--  Console.run (consoleRunner tests)
