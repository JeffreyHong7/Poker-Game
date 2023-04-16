open OUnit2
open Poker

let tests = "poker test suite" >::: List.flatten []
let _ = run_test_tt_main tests
