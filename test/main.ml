open OUnit2
open Poker

let turn_test (name : string) (t : table) (expected : string) : test =
  name >:: fun _ -> assert_equal expected (turn t)

let turn tests =
  [ turn_test "In a game of two players returns the number of acting player" ]

let tests = "poker test suite" >::: List.flatten []
let _ = run_test_tt_main tests
