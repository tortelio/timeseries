module TimeseriesClientTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import TimeseriesClient exposing (..)


suite : Test
suite =
    describe "Derivate"
      [ test "Mean of 2 number"
          ( \_ -> means [ 1, 2 ] |> Expect.equal [ 1.5 ] )
      ]
