open OUnit2
open Poker

(*** Test Plan:
     To test the system, we created a test suite entirely composing of automated
     OUnit tests. The OUnit tests were for checking the correctness of various
     outputs, including different player and table fields following diverse
     sequences of game actions. After a specified action was taken, the game
     table was examined and different game factors were tested/assessed.
     The only module tested was Poker, as that was the only necessary module for
     the game. Overall, the test cases were a combination of black box and
     glass box. Arguably, there was a randomness component to the testing
     because the player hands and game board are randomly generated, which is
     a key component of many test cases. A solid standard of correctness was
     achieved due to the fact that test cases were thoroughly developed and
     planned in both the black box and glass box senses, and various tests were
     intricate edge cases on both minimum and maximum size tables. *)

(* Below are 9 players that will be used to create different tables*)
let p1 = create_player "jeff" 100.
let p2 = create_player "daniel" 100.
let initial_two_table = start [ p1; p2 ] 1. 2.
let sb_two_table = turn initial_two_table

let bb_two_table =
  match get_players initial_two_table with
  | _ :: h' :: _ -> get_player_name h'
  | _ -> failwith "impossible"

let p3 = create_player "dawid" 100.
let initial_three_table = start [ p1; p2; p3 ] 1. 2.

let button_three_table =
  match get_players initial_three_table with
  | h :: _ -> get_player_name h
  | _ -> failwith "impossible"

let sb_three_table =
  match get_players initial_three_table with
  | _ :: p :: _ -> get_player_name p
  | _ -> failwith "impossible"

let bb_three_table =
  match get_players initial_three_table with
  | _ :: _ :: p :: _ -> get_player_name p
  | _ -> failwith "impossible"

let p4 = create_player "jon" 100.
let p5 = create_player "ivan" 100.
let p6 = create_player "harley" 100.
let p7 = create_player "jay" 100.
let p8 = create_player "tye" 100.
let p9 = create_player "cole" 100.
let initial_full_table = start [ p1; p2; p3; p4; p5; p6; p7; p8; p9 ] 1. 2.

let sb_full_table =
  match get_players initial_full_table with
  | _ :: _ :: _ :: _ :: _ :: _ :: _ :: p :: _ -> get_player_name p
  | _ -> failwith "impossible"

let bb_full_table =
  match get_players initial_full_table with
  | _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: p :: _ -> get_player_name p
  | _ -> failwith "impossible"

let first_player_full_table_pre =
  match get_players initial_full_table with
  | p :: _ -> get_player_name p
  | _ -> failwith "impossible"

let second_player_full_table_pre =
  match get_players initial_full_table with
  | _ :: p :: _ -> get_player_name p
  | _ -> failwith "impossible"

let third_player_full_table_pre =
  match get_players initial_full_table with
  | _ :: _ :: p :: _ -> get_player_name p
  | _ -> failwith "impossible"

let fourth_player_full_table_pre =
  match get_players initial_full_table with
  | _ :: _ :: _ :: p :: _ -> get_player_name p
  | _ -> failwith "impossible"

let fifth_to_act_player_pre =
  match get_players initial_full_table with
  | _ :: _ :: _ :: _ :: p :: _ -> get_player_name p
  | _ -> failwith "impossible"

let sixth_to_act_player_pre =
  match get_players initial_full_table with
  | _ :: _ :: _ :: _ :: _ :: p :: _ -> get_player_name p
  | _ -> failwith "impossible"

let seventh_to_act_player_pre =
  match get_players initial_full_table with
  | _ :: _ :: _ :: _ :: _ :: _ :: p :: _ -> get_player_name p
  | _ -> failwith "impossible"

let raise_test (name : string) (t : table) (a : float)
    (expected_output : string * float * float) : test =
  name >:: fun _ ->
  let post_raise_table = raise t a in
  assert_equal expected_output
    ( turn post_raise_table,
      get_current_bet post_raise_table,
      pot_size post_raise_table )

(* two-player game tables for testing *)
let first_raise_two_table_preflop = raise initial_two_table 5.
let second_raise_two_table_preflop = raise first_raise_two_table_preflop 15.
let two_table_flop = call second_raise_two_table_preflop
let first_raise_two_table_flop = raise two_table_flop 5.
let second_raise_two_table_flop = raise first_raise_two_table_flop 10.
let third_raise_two_table_flop = raise second_raise_two_table_flop 20.
let two_table_turn = call third_raise_two_table_flop
let two_table_turn_raise = raise two_table_turn 20.
let two_table_river = call two_table_turn_raise
let first_raise_two_table_river = raise two_table_river 45.

(* three-player game tables for testing *)
let first_raise_three_table_preflop = raise initial_three_table 5.
let sb_call_pre_three_table = call first_raise_three_table_preflop
let flop_three_table = call sb_call_pre_three_table
let first_raise_flop_three_table = raise flop_three_table 6.
let second_raise_flop_three_table = raise first_raise_flop_three_table 20.
let first_call_flop_three_table = call second_raise_flop_three_table
let turn_three_table = call first_call_flop_three_table
let first_raise_turn_three_table = raise turn_three_table 10.
let second_raise_turn_three_table = raise first_raise_turn_three_table 20.
let third_raise_turn_three_table = raise second_raise_turn_three_table 40.
let first_call_turn_three_table = call third_raise_turn_three_table
let river_three_table = call first_call_turn_three_table
let first_raise_river_three_table = raise river_three_table 35.
let first_call_river_three_table = call first_raise_river_three_table

(* nine-player game tables for testing *)
let first_raise_full_table_preflop = raise initial_full_table 5.
let second_raise_full_table_preflop = raise first_raise_full_table_preflop 10.
let third_raise_full_table_preflop = raise second_raise_full_table_preflop 20.
let first_call_full_table_preflop = call third_raise_full_table_preflop
let second_call_full_table_preflop = call first_call_full_table_preflop
let third_call_full_table_preflop = call second_call_full_table_preflop
let fourth_call_full_table_preflop = call third_call_full_table_preflop
let fifth_call_full_table_preflop = call fourth_call_full_table_preflop
let sixth_call_full_table_preflop = call fifth_call_full_table_preflop
let seventh_call_full_table_preflop = call sixth_call_full_table_preflop
let full_table_flop = call seventh_call_full_table_preflop
let first_raise_full_table_flop = raise full_table_flop 5.
let first_call_full_table_flop = call first_raise_full_table_flop
let second_raise_full_table_flop = raise first_call_full_table_flop 10.
let second_call_full_table_flop = call second_raise_full_table_flop
let third_call_full_table_flop = call second_call_full_table_flop
let fourth_call_full_table_flop = call third_call_full_table_flop
let fifth_call_full_table_flop = call fourth_call_full_table_flop
let sixth_call_full_table_flop = call fifth_call_full_table_flop
let seventh_call_full_table_flop = call sixth_call_full_table_flop
let eighth_call_full_table_flop = call seventh_call_full_table_flop
let full_table_turn = call eighth_call_full_table_flop
let first_raise_full_table_turn = raise full_table_turn 10.
let first_call_full_table_turn = call first_raise_full_table_turn
let second_call_full_table_turn = call first_call_full_table_turn
let third_call_full_table_turn = call second_call_full_table_turn
let fourth_call_full_table_turn = call third_call_full_table_turn
let fifth_call_full_table_turn = call fourth_call_full_table_turn
let sixith_call_full_table_turn = call fifth_call_full_table_turn
let seventh_call_full_table_turn = call sixith_call_full_table_turn
let second_raise_full_table_turn = raise seventh_call_full_table_turn 25.
let ninth_call_full_table_turn = call second_raise_full_table_turn
let tenth_call_full_table_turn = call ninth_call_full_table_turn
let eleventh_call_full_table_turn = call tenth_call_full_table_turn
let twelfth_call_full_table_turn = call eleventh_call_full_table_turn
let thirteenth_call_full_table_turn = call twelfth_call_full_table_turn
let fourteenth_call_full_table_turn = call thirteenth_call_full_table_turn
let fifteenth_call_full_table_turn = call fourteenth_call_full_table_turn
let full_table_river = call fifteenth_call_full_table_turn
let first_raise_full_table_river = raise full_table_river 10.
let second_raise_full_table_river = raise first_raise_full_table_river 45.
let first_call_full_table_river = call second_raise_full_table_river
let second_call_full_table_river = call first_call_full_table_river
let third_call_full_table_river = call second_call_full_table_river
let fourth_call_full_table_river = call third_call_full_table_river
let fifth_call_full_table_river = call fourth_call_full_table_river
let sixth_call_full_table_river = call fifth_call_full_table_river
let seventh_call_full_table_river = call sixth_call_full_table_river

let raise_tests =
  (* tests for two-player table game *)
  [
    raise_test "two_table on player raise 5." initial_two_table 5.
      (bb_two_table, 5., 7.);
    raise_test "two_table on player re-raise 15." first_raise_two_table_preflop
      15. (sb_two_table, 15., 20.);
    raise_test "two_table flop bet of 5." two_table_flop 5.
      (sb_two_table, 5., 35.);
    raise_test "two_table flop re-raise 10." first_raise_two_table_flop 10.
      (bb_two_table, 10., 45.);
    raise_test "two_table flop re-raise 20." second_raise_two_table_flop 20.
      (sb_two_table, 20., 60.);
    raise_test "two_table turn bet of 20." two_table_turn 20.
      (sb_two_table, 20., 90.);
    raise_test "two_table all-in bet of 45" two_table_river 45.
      (sb_two_table, 45., 155.);
  ]
  (* tests for three-player table game *)
  @ [
      raise_test "three_table on raise 5." initial_three_table 5.
        (sb_three_table, 5., 8.);
      raise_test "three_table on raise 6" flop_three_table 6.
        (bb_three_table, 6., 21.);
      raise_test "three_table on re-raise 20." first_raise_flop_three_table 20.
        (button_three_table, 20., 41.);
      raise_test "three_table on raise 10." turn_three_table 10.
        (bb_three_table, 10., 85.);
      raise_test "three_table on re-raise 20." first_raise_turn_three_table 20.
        (button_three_table, 20., 105.);
      raise_test "three_table on re-raise 40." second_raise_turn_three_table 40.
        (sb_three_table, 40., 145.);
      raise_test "three_table on all-in bet of 35." river_three_table 35.
        (bb_three_table, 35., 230.);
    ]
  @ (* tests for nine-player table game *)
  [
    raise_test "full_table on raise 5." initial_full_table 5.
      (second_player_full_table_pre, 5., 8.);
    raise_test "full_table on re-raise to 10." first_raise_full_table_preflop
      10.
      (third_player_full_table_pre, 10., 18.);
    raise_test "full_table on re-raise to 20." second_raise_full_table_preflop
      20.
      (fourth_player_full_table_pre, 20., 38.);
    raise_test "full_table on raise 5." full_table_flop 5.
      (bb_full_table, 5., 185.);
    raise_test "full_table on re-raise 10." first_call_full_table_flop 10.
      (second_player_full_table_pre, 10., 200.);
    raise_test "full_table on raise 10." full_table_turn 10.
      (bb_full_table, 10., 280.);
    raise_test "full_table on raise 25." seventh_call_full_table_turn 25.
      (sb_full_table, 25., 375.);
    raise_test "full_table on raise 10." full_table_river 10.
      (bb_full_table, 10., 505.);
    raise_test "full_table on raise 20." first_raise_full_table_river 20.
      (first_player_full_table_pre, 20., 525.);
  ]

let call_test (name : string) (t : table)
    (expected_output : string * float * float * float * float) : test =
  name >:: fun _ ->
  let old_action = turn t in
  let post_call_table = call t in
  assert_equal expected_output
    ( turn post_call_table,
      show_money old_action post_call_table,
      get_bet post_call_table old_action,
      get_current_bet post_call_table,
      pot_size post_call_table )

let call_tests =
  (* tests on two_player table game *)
  [
    call_test "call raise 5. by initial action in two_table"
      first_raise_two_table_preflop
      (bb_two_table, 95., 0., 0., 10.);
    call_test "call re-raise 15. by big blind in two_table"
      second_raise_two_table_preflop
      (bb_two_table, 85., 0., 0., 30.);
    call_test "call second re-raise on flop" third_raise_two_table_flop
      (bb_two_table, 65., 0., 0., 70.);
    call_test "call raise on the turn by big blind in two_table"
      two_table_turn_raise
      (bb_two_table, 45., 0., 0., 110.);
    call_test "call all-in raise on the river by big blind in two_table"
      first_raise_two_table_river
      (bb_two_table, 0., 45., 45., 200.);
  ]
  (* tests on three_player table game *)
  @ [
      call_test "call initial raise from button player by small blind"
        first_raise_three_table_preflop
        (bb_three_table, 95., 5., 5., 12.);
      call_test "call initial raise from button player by big big blind"
        sb_call_pre_three_table
        (sb_three_table, 95., 0., 0., 15.);
      call_test "call re-raise from big blind by button player"
        second_raise_flop_three_table
        (sb_three_table, 75., 20., 20., 61.);
      call_test "call re-raise from big_blind by small blind player"
        first_call_flop_three_table
        (sb_three_table, 75., 0., 0., 75.);
      call_test "call three bet from button by small blind player"
        third_raise_turn_three_table
        (bb_three_table, 35., 40., 40., 175.);
      call_test "call three bet from button by big blind player"
        first_call_turn_three_table
        (sb_three_table, 35., 0., 0., 195.);
      call_test "call initial raise from small blind by big blind player"
        first_raise_river_three_table
        (button_three_table, 0., 35., 35., 265.);
      call_test "call initial raise from small blind by button player"
        first_call_river_three_table
        (sb_three_table, 0., 35., 35., 300.);
    ]
  @ (* tests on nine_player table game *)
  [
    call_test "call three-bet from third to act player by fourth to act player"
      third_raise_full_table_preflop
      (fifth_to_act_player_pre, 80., 20., 20., 58.);
    call_test "call three-bet from third to act player by first to act player"
      sixth_call_full_table_preflop
      (second_player_full_table_pre, 80., 20., 20., 170.);
    call_test "call three-bet from third to act player by second to act player"
      seventh_call_full_table_preflop
      (sb_full_table, 80., 0., 0., 180.);
    call_test "call initial from small blind by big blind on the flop"
      first_raise_full_table_flop
      (first_player_full_table_pre, 75., 5., 5., 190.);
    call_test
      "call raise from second to act player by fifth to act player on the flop"
      second_raise_full_table_flop
      (third_player_full_table_pre, 70., 10., 10., 210.);
    call_test "call raise from button player by high-jack player on the turn"
      fourteenth_call_full_table_turn
      (sixth_to_act_player_pre, 45., 25., 25., 480.);
    call_test "call raise from button player by cut-off player on the turn"
      fifteenth_call_full_table_turn
      (sb_full_table, 45., 0., 0., 495.);
    call_test
      "call on raise from big blind player by the small blind on the river"
      seventh_call_full_table_river
      (bb_full_table, 0., 45., 45., 900.);
  ]

(* Additional tables for check tests on two_table *)
let first_call_two_table_preflop = call initial_two_table
let snd_two_table_flop = check first_call_two_table_preflop
let first_check_two_table_flop = check snd_two_table_flop
let snd_two_table_turn = check first_check_two_table_flop
let first_check_two_table_turn = check snd_two_table_turn
let snd_two_table_river = check first_check_two_table_turn
let first_check_two_table_river = check snd_two_table_river

(* Additional tables for check tests on three_table *)
let first_call_three_table_preflop = call initial_three_table
let second_call_three_table_preflop = call first_call_three_table_preflop
let snd_three_table_flop = check second_call_three_table_preflop
let first_check_three_table_flop = check snd_three_table_flop
let second_check_three_table_flop = check first_check_three_table_flop
let snd_three_table_turn = check second_check_three_table_flop
let first_check_three_table_turn = check snd_three_table_turn
let second_check_three_table_turn = check first_check_three_table_turn
let snd_three_table_river = check second_check_three_table_turn
let first_check_three_table_river = check snd_three_table_river
let second_check_three_table_river = check first_check_three_table_river

(* Additional tables for check tests on full_table *)
let first_call_full_table_preflop = call initial_full_table
let second_call_full_table_preflop = call first_call_full_table_preflop
let third_call_full_table_preflop = call second_call_full_table_preflop
let fourth_call_full_table_preflop = call third_call_full_table_preflop
let fifth_call_full_table_preflop = call fourth_call_full_table_preflop
let sixth_call_full_table_preflop = call fifth_call_full_table_preflop
let seventh_call_full_table_preflop = call sixth_call_full_table_preflop
let eighth_call_full_table_preflop = call seventh_call_full_table_preflop
let snd_full_table_flop = check eighth_call_full_table_preflop
let first_check_full_table_flop = check snd_full_table_flop
let second_check_full_table_flop = check first_check_full_table_flop
let third_check_full_table_flop = check second_check_full_table_flop
let fourth_check_full_table_flop = check third_check_full_table_flop
let fifth_check_full_table_flop = check fourth_check_full_table_flop
let sixth_check_full_table_flop = check fifth_check_full_table_flop
let seventh_check_full_table_flop = check sixth_check_full_table_flop
let eighth_check_full_table_flop = check seventh_check_full_table_flop
let snd_full_table_turn = check eighth_check_full_table_flop
let first_check_full_table_turn = check snd_full_table_turn
let second_check_full_table_turn = check first_check_full_table_turn
let third_check_full_table_turn = check second_check_full_table_turn
let fourth_check_full_table_turn = check third_check_full_table_turn
let fifth_check_full_table_turn = check fourth_check_full_table_turn
let sixth_check_full_table_turn = check fifth_check_full_table_turn
let seventh_check_full_table_turn = check sixth_check_full_table_turn
let eighth_check_full_table_turn = check seventh_check_full_table_turn
let snd_full_table_river = check eighth_check_full_table_turn
let first_check_full_table_river = check snd_full_table_river
let second_check_full_table_river = check first_check_full_table_river
let third_check_full_table_river = check second_check_full_table_river
let fourth_check_full_table_river = check third_check_full_table_river
let fifth_check_full_table_river = check fourth_check_full_table_river
let sixth_check_full_table_river = check fifth_check_full_table_river
let seventh_check_full_table_river = check sixth_check_full_table_river
let eighth_check_full_table_river = check seventh_check_full_table_river

let check_test (name : string) (t : table)
    (expected_output : string * float * float * float * float) : test =
  name >:: fun _ ->
  let old_action = turn t in
  let post_check_table = check t in
  assert_equal expected_output
    ( turn post_check_table,
      show_money old_action post_check_table,
      get_bet post_check_table old_action,
      get_current_bet post_check_table,
      pot_size post_check_table )

let check_tests =
  [
    (* tests on two_player table game *)
    check_test "check on initial two_table after small blind call preflop"
      first_call_two_table_preflop
      (bb_two_table, 98., 0., 0., 4.);
    check_test "check as first action by big blind on flop" snd_two_table_flop
      (sb_two_table, 98., 0., 0., 4.);
    check_test "check as second action by small blind on flop"
      first_check_two_table_flop
      (bb_two_table, 98., 0., 0., 4.);
    check_test "check as first action on the turn" snd_two_table_turn
      (sb_two_table, 98., 0., 0., 4.);
    check_test "check as second action on the turn" first_check_two_table_turn
      (bb_two_table, 98., 0., 0., 4.);
    check_test "check as first action on the river" snd_two_table_river
      (sb_two_table, 98., 0., 0., 4.);
    check_test "check as second action on the river" first_check_two_table_river
      (bb_two_table, 98., 0., 0., 4.);
    check_test "check as first action on flop after betting preflop"
      two_table_flop
      (sb_two_table, 85., 0., 0., 30.);
    check_test "check as first action after betting preflop and flop"
      two_table_turn
      (sb_two_table, 65., 0., 0., 70.);
    check_test "check as first action after betting preflop, flop, and turn"
      two_table_river
      (sb_two_table, 45., 0., 0., 110.);
  ]
  (* tests for three_player table game *)
  @ [
      check_test "check as second action on three_table preflop"
        second_call_three_table_preflop
        (sb_three_table, 98., 0., 0., 6.);
      check_test "check as first action on three_table flop"
        snd_three_table_flop
        (bb_three_table, 98., 0., 0., 6.);
      check_test "check as second action on three_table flop"
        first_check_three_table_flop
        (button_three_table, 98., 0., 0., 6.);
      check_test "check as third action on three_table flop"
        second_check_three_table_flop
        (sb_three_table, 98., 0., 0., 6.);
      check_test "check as first action on three_table turn"
        snd_three_table_turn
        (bb_three_table, 98., 0., 0., 6.);
      check_test "check as second action on three_table turn"
        first_check_three_table_turn
        (button_three_table, 98., 0., 0., 6.);
      check_test "check as third action on three_table turn"
        second_check_three_table_turn
        (sb_three_table, 98., 0., 0., 6.);
      check_test "check as first action on three_table river"
        snd_three_table_river
        (bb_three_table, 98., 0., 0., 6.);
      check_test "check as second action on three_table river"
        first_check_three_table_river
        (button_three_table, 98., 0., 0., 6.);
      check_test "check as third action on three_table river"
        second_check_three_table_river
        (sb_three_table, 98., 0., 0., 6.);
      check_test "check as first action after betting preflop" flop_three_table
        (bb_three_table, 95., 0., 0., 15.);
      check_test
        "check as first action on the turn after betting preflop and flop"
        turn_three_table
        (bb_three_table, 75., 0., 0., 75.);
      check_test
        "check as first action on the river after betting preflop, flop, and \
         turn"
        river_three_table
        (bb_three_table, 35., 0., 0., 195.);
    ]
  @ (* tests for nine_player table game *)
  [
    check_test "check as last action preflop" eighth_call_full_table_preflop
      (sb_full_table, 98., 0., 0., 18.);
    check_test "check as first action on full_table flop after betting preflop"
      snd_full_table_flop
      (bb_full_table, 98., 0., 0., 18.);
    check_test "check as second action on full_table flop"
      first_check_full_table_flop
      (first_player_full_table_pre, 98., 0., 0., 18.);
    check_test "check as third action on full_table flop"
      second_check_full_table_flop
      (second_player_full_table_pre, 98., 0., 0., 18.);
    check_test "check as fourth action on full_table flop"
      third_check_full_table_flop
      (third_player_full_table_pre, 98., 0., 0., 18.);
    check_test "check as fifth action on full_table flop"
      fourth_check_full_table_flop
      (fourth_player_full_table_pre, 98., 0., 0., 18.);
    check_test "check as sixth action on full_table flop"
      fifth_check_full_table_flop
      (fifth_to_act_player_pre, 98., 0., 0., 18.);
    check_test "check as seventh action on full_table flop"
      sixth_check_full_table_flop
      (sixth_to_act_player_pre, 98., 0., 0., 18.);
    check_test "check as eighth action on full_table flop"
      seventh_check_full_table_flop
      (seventh_to_act_player_pre, 98., 0., 0., 18.);
    check_test "check as ninth action on full_table flop"
      eighth_check_full_table_flop
      (sb_full_table, 98., 0., 0., 18.);
    check_test "check as first action on full_table turn" snd_full_table_turn
      (bb_full_table, 98., 0., 0., 18.);
    check_test "check as second action on full_table turn"
      first_check_full_table_turn
      (first_player_full_table_pre, 98., 0., 0., 18.);
    check_test "check as third action on full_table turn"
      second_check_full_table_turn
      (second_player_full_table_pre, 98., 0., 0., 18.);
    check_test "check as fourth action on full_table turn"
      third_check_full_table_turn
      (third_player_full_table_pre, 98., 0., 0., 18.);
    check_test "check as fifth action on full_table turn"
      fourth_check_full_table_turn
      (fourth_player_full_table_pre, 98., 0., 0., 18.);
    check_test "check as sixth action on full_table turn"
      fifth_check_full_table_turn
      (fifth_to_act_player_pre, 98., 0., 0., 18.);
    check_test "check as seventh action on full_table turn"
      sixth_check_full_table_turn
      (sixth_to_act_player_pre, 98., 0., 0., 18.);
    check_test "check as eighth action on full_table turn"
      seventh_check_full_table_turn
      (seventh_to_act_player_pre, 98., 0., 0., 18.);
    check_test "check as ninth action on full_table turn"
      eighth_check_full_table_turn
      (sb_full_table, 98., 0., 0., 18.);
    check_test "check as first action on full_table river" snd_full_table_river
      (bb_full_table, 98., 0., 0., 18.);
    check_test "check as second action on full_table river"
      first_check_full_table_river
      (first_player_full_table_pre, 98., 0., 0., 18.);
    check_test "check as third action on full_table river"
      second_check_full_table_river
      (second_player_full_table_pre, 98., 0., 0., 18.);
    check_test "check as fourth action on full_table river"
      third_check_full_table_river
      (third_player_full_table_pre, 98., 0., 0., 18.);
    check_test "check as fifth action on full_table river"
      fourth_check_full_table_river
      (fourth_player_full_table_pre, 98., 0., 0., 18.);
    check_test "check as sixth action on full_table river"
      fifth_check_full_table_river
      (fifth_to_act_player_pre, 98., 0., 0., 18.);
    check_test "check as seventh action on full_table river"
      sixth_check_full_table_river
      (sixth_to_act_player_pre, 98., 0., 0., 18.);
    check_test "check as eighth action on full_table river"
      seventh_check_full_table_river
      (seventh_to_act_player_pre, 98., 0., 0., 18.);
    check_test "check as ninth action on full_table river"
      eighth_check_full_table_river
      (sb_full_table, 98., 0., 0., 18.);
  ]

let fold_test (name : string) (t : table) (expected_output : string * bool) :
    test =
  name >:: fun _ ->
  let old_action = turn t in
  let post_fold_table = fold t in
  assert_equal expected_output
    (turn post_fold_table, left_status post_fold_table old_action)

let fold_tests =
  [
    (* tests for three player game *)
    fold_test "preflop fold of first to act player" initial_three_table
      (sb_three_table, true);
    fold_test "preflop fold of second to act player"
      first_raise_three_table_preflop (bb_three_table, true);
    fold_test "preflop fold of last to act player"
      second_call_three_table_preflop (sb_three_table, true);
    fold_test "flop fold of first to act player" flop_three_table
      (bb_three_table, true);
    fold_test "flop fold of second to act player" first_raise_flop_three_table
      (button_three_table, true);
    fold_test "flop fold of last to act player" second_raise_flop_three_table
      (sb_three_table, true);
    fold_test "turn fold of first to act player" turn_three_table
      (bb_three_table, true);
    fold_test "turn fold of second to act player" first_raise_turn_three_table
      (button_three_table, true);
    fold_test "turn fold of last to act player" second_raise_turn_three_table
      (sb_three_table, true);
    fold_test "river fold of first to act player" river_three_table
      (bb_three_table, true);
    fold_test "river fold of second to act player" first_raise_river_three_table
      (button_three_table, true);
    fold_test "river fold of last to act player" first_call_river_three_table
      (sb_three_table, true);
  ]
  @ [
      (* tests for nine player game *)
      fold_test "preflop fold of first to act player" initial_full_table
        (second_player_full_table_pre, true);
      fold_test "preflop fold of second to act player"
        first_raise_full_table_preflop
        (third_player_full_table_pre, true);
      fold_test "preflop fold of third to act player"
        second_raise_full_table_preflop
        (fourth_player_full_table_pre, true);
      fold_test "preflop fold of fourth to act player"
        third_raise_full_table_preflop
        (fifth_to_act_player_pre, true);
      fold_test "preflop fold of fifth to act player"
        fourth_call_full_table_preflop
        (sixth_to_act_player_pre, true);
      fold_test "preflop fold of sixth to act player"
        fifth_call_full_table_preflop
        (seventh_to_act_player_pre, true);
      fold_test "preflop fold of seventh to act player"
        sixth_call_full_table_preflop (sb_full_table, true);
      fold_test "preflop fold of small blind player"
        seventh_call_full_table_preflop (bb_full_table, true);
      fold_test "preflop fold of big blind player"
        eighth_call_full_table_preflop (sb_full_table, true);
      fold_test "flop fold of first to act player" snd_full_table_flop
        (bb_full_table, true);
      fold_test "flop fold of second to act player" first_check_full_table_flop
        (first_player_full_table_pre, true);
      fold_test "flop fold of third to act player" second_check_full_table_flop
        (second_player_full_table_pre, true);
      fold_test "flop fold of fourth to act player" third_check_full_table_flop
        (third_player_full_table_pre, true);
      fold_test "flop fold of fifth to act player" fourth_check_full_table_flop
        (fourth_player_full_table_pre, true);
      fold_test "flop fold of sixth to act player" fifth_check_full_table_flop
        (fifth_to_act_player_pre, true);
      fold_test "flop fold of seventh to act player" sixth_check_full_table_flop
        (sixth_to_act_player_pre, true);
      fold_test "flop fold of eighth to act player"
        seventh_check_full_table_flop
        (seventh_to_act_player_pre, true);
      fold_test "turn fold of first to act player" snd_full_table_turn
        (bb_full_table, true);
      fold_test "turn fold of second to act player" first_check_full_table_turn
        (first_player_full_table_pre, true);
      fold_test "turn fold of third to act player" second_check_full_table_turn
        (second_player_full_table_pre, true);
      fold_test "turn fold of fourth to act player" third_check_full_table_turn
        (third_player_full_table_pre, true);
      fold_test "turn fold of fifth to act player" fourth_check_full_table_turn
        (fourth_player_full_table_pre, true);
      fold_test "turn fold of sixth to act player" fifth_check_full_table_turn
        (fifth_to_act_player_pre, true);
      fold_test "turn fold of seventh to act player" sixth_check_full_table_turn
        (sixth_to_act_player_pre, true);
      fold_test "turn fold of eighth to act player"
        seventh_check_full_table_turn
        (seventh_to_act_player_pre, true);
      fold_test "river fold of first to act player" snd_full_table_river
        (bb_full_table, true);
      fold_test "river fold of second to act player"
        first_check_full_table_river
        (first_player_full_table_pre, true);
      fold_test "river fold of third to act player"
        second_check_full_table_river
        (second_player_full_table_pre, true);
      fold_test "river fold of fourth to act player"
        third_check_full_table_river
        (third_player_full_table_pre, true);
      fold_test "river fold of fifth to act player"
        fourth_check_full_table_river
        (fourth_player_full_table_pre, true);
      fold_test "river fold of sixth to act player" fifth_check_full_table_river
        (fifth_to_act_player_pre, true);
      fold_test "river fold of seventh to act player"
        sixth_check_full_table_river
        (sixth_to_act_player_pre, true);
      fold_test "river fold of eighth to act player"
        seventh_check_full_table_river
        (seventh_to_act_player_pre, true);
    ]

let stand_off_test (name : string) (input : table)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output (winner_names input)
    ~printer:(List.fold_left (fun x acc -> acc ^ x) "")

let winnings_test (name : string) (input1 : string) (input2 : table)
    (expected_output : float) : test =
  name >:: fun _ ->
  assert_equal expected_output (show_money input1 input2)
    ~printer:string_of_float

(* players *)
let justin =
  create_custom_player "justin" 25.2
    [ create_custom_card 2 'c'; create_custom_card 3 's' ]

let jeff =
  create_custom_player "jeff" 20.3
    [ create_custom_card 14 's'; create_custom_card 5 'd' ]

let chris =
  create_custom_player "chris" 24.2
    [ create_custom_card 5 'c'; create_custom_card 8 'd' ]

let bric =
  create_custom_player "bric" 10.0
    [ create_custom_card 4 's'; create_custom_card 12 's' ]

let stephen =
  create_custom_player "stephen" 10.0
    [ create_custom_card 10 'd'; create_custom_card 12 'c' ]

let joe =
  create_custom_player "joe" 10.0
    [ create_custom_card 10 'h'; create_custom_card 9 'c' ]

let daniel =
  create_custom_player "daniel" 10.0
    [ create_custom_card 4 'h'; create_custom_card 13 'c' ]

let jungkook =
  create_custom_player "junkook" 10.0
    [ create_custom_card 7 'h'; create_custom_card 7 's' ]

let kayden =
  create_custom_player "kayden" 14.0
    [ create_custom_card 13 's'; create_custom_card 5 's' ]

let jonathan =
  create_custom_player "jonathan" 14.0
    [ create_custom_card 12 'd'; create_custom_card 5 'd' ]

let marcus =
  create_custom_player "marcus" 15.7
    [ create_custom_card 11 'd'; create_custom_card 6 'd' ]

let jayden =
  create_custom_player "jayden" 14.5
    [ create_custom_card 7 'd'; create_custom_card 8 's' ]

(* win with high card *)

let high =
  create_custom_table [ justin; jeff; chris ]
    [
      create_custom_card 10 'c';
      create_custom_card 9 's';
      create_custom_card 7 'h';
      create_custom_card 4 'd';
      create_custom_card 13 'd';
    ]
    20.0 jeff

let stand_off_high = stand_off high
let high_card_test = stand_off_test "high card" stand_off_high [ "jeff" ]

let high_winnings =
  winnings_test "winnings for high card win distributed properly" "jeff"
    stand_off_high 40.3

(* a tie between players *)
let tie =
  create_custom_table [ justin; chris ]
    [
      create_custom_card 10 'c';
      create_custom_card 9 's';
      create_custom_card 7 'h';
      create_custom_card 4 'd';
      create_custom_card 13 'd';
    ]
    20.0 justin

let stand_off_tie = stand_off tie
let tie_test = stand_off_test "tie" stand_off_tie [ "justin"; "chris" ]

let tie_winnings =
  winnings_test "winnings for tie should be distributed properly" "justin"
    stand_off_tie 35.2

(* a pair wins over weaker pair and high card *)

let pair =
  create_custom_table
    [ (*pairs: *) stephen; bric; (*high cards: *) justin; jeff; chris ]
    [
      create_custom_card 10 'c';
      create_custom_card 9 's';
      create_custom_card 7 'h';
      create_custom_card 4 'd';
      create_custom_card 13 'd';
    ]
    20.0 jeff

let stand_off_pair = stand_off pair
let pair_test = stand_off_test "pair" stand_off_pair [ "stephen" ]

let pair_winnings =
  winnings_test "winnings for a pair win should be distributed properly"
    "stephen" stand_off_pair 30.0

(* two pair beats weaker twopair, pair and high card *)

let two_pair =
  create_custom_table
    [
      (*two pairs: *)
      joe (*best*);
      daniel;
      (*pairs:*)
      stephen;
      bric;
      (*high cards:*)
      justin;
      jeff;
      chris;
    ]
    [
      create_custom_card 10 'c';
      create_custom_card 9 's';
      create_custom_card 7 'h';
      create_custom_card 4 'd';
      create_custom_card 13 'd';
    ]
    20.0 jeff

let stand_off_twopair = stand_off two_pair
let two_pair_test = stand_off_test "two_pair" stand_off_twopair [ "daniel" ]

let two_pair_winnings =
  winnings_test "winnings for a two pair win should be distributed properly"
    "daniel" stand_off_twopair 30.0

(* trips beats weak trips, twopair, pair, and high card *)

let trips =
  create_custom_table
    [
      (*trips*)
      joe (*best*);
      jungkook;
      (*two pairs*)
      stephen;
      (*pairs:*)
      bric;
      (* high cards *)
      justin;
      jeff;
      chris;
    ]
    [
      create_custom_card 9 'h';
      create_custom_card 9 's';
      create_custom_card 10 'h';
      create_custom_card 7 'c';
      create_custom_card 12 'd';
    ]
    20.0 jeff

let stand_off_trips = stand_off trips
let trips_test = stand_off_test "trips" stand_off_trips [ "joe" ]

let trips_winnings =
  winnings_test "winnings for a trips win should be distributed properly" "joe"
    stand_off_trips 30.0

(* straight beats weak straight, trips, twopair, pair, and high card*)
let straight =
  create_custom_table
    [
      chris (*best straight*);
      justin (*straight*);
      jungkook (*trips*);
      kayden (*two pair*);
      daniel (*two pair*);
      jeff (*pair*);
      bric (*pair*);
      stephen (*high*);
      joe (*high*);
    ]
    [
      create_custom_card 4 'h';
      create_custom_card 5 'd';
      create_custom_card 6 'h';
      create_custom_card 7 'c';
      create_custom_card 13 'd';
    ]
    20.0 jeff

let stand_off_straight = stand_off straight
let straight_test = stand_off_test "straight" stand_off_straight [ "chris" ]

let straight_winnings =
  winnings_test "winnings for a straight win should be distributed properly"
    "chris" stand_off_straight 44.2

(* lowest possible straights *)
let straight_low =
  create_custom_table [ justin; jeff ]
    [
      create_custom_card 4 'h';
      create_custom_card 5 'd';
      create_custom_card 14 'h';
      create_custom_card 7 'c';
      create_custom_card 7 'd';
    ]
    20.0 jeff

let stand_off_straight_low = stand_off straight_low

let straight_low_test =
  stand_off_test "straight low" stand_off_straight_low [ "justin" ]

let low_straight_winnings =
  winnings_test "winnings for a low straight win should be distributed properly"
    "justin" stand_off_straight_low 45.2

(* flush beats lower flush, straight, trips, two-pair, pairs, and high *)
let flush =
  create_custom_table
    [
      jonathan (*best flush*);
      marcus (*flush*);
      justin (*straight*);
      jungkook (*trips*);
      jeff (*two pair*);
      bric (*pair*);
      stephen (*high*);
    ]
    [
      create_custom_card 4 'h';
      create_custom_card 5 'd';
      create_custom_card 14 'h';
      create_custom_card 7 'd';
      create_custom_card 8 'd';
    ]
    20.0 jeff

let stand_off_flush = stand_off flush
let flush_test = stand_off_test "flush" stand_off_flush [ "jonathan" ]

let flush_winnings =
  winnings_test "winnings for a flush win should be distributed properly"
    "jonathan" stand_off_flush 34.0

(* flush tie *)
let flush_tie =
  create_custom_table [ jonathan; marcus ]
    [
      create_custom_card 4 'h';
      create_custom_card 5 'd';
      create_custom_card 14 'd';
      create_custom_card 7 'd';
      create_custom_card 7 'd';
    ]
    20.0 jonathan

let stand_off_flush_tie = stand_off flush_tie

let flush_tie_test =
  stand_off_test "flush tie" stand_off_flush_tie [ "jonathan"; "marcus" ]

let flush_tie_winnings =
  winnings_test "winnings for a flush tie should be distributed properly"
    "jonathan" stand_off_flush_tie 24.0

(* FullHouse beats weaker fullhouse + flush *)
let full_house =
  create_custom_table
    [ jayden (*best full house*); jungkook (*full house*); marcus (*flush*) ]
    [
      create_custom_card 4 'd';
      create_custom_card 5 'd';
      create_custom_card 8 's';
      create_custom_card 7 'd';
      create_custom_card 8 'h';
    ]
    20.0 jonathan

let stand_off_full_house = stand_off full_house

let full_house_test =
  stand_off_test "full house" stand_off_full_house [ "jayden" ]

let full_house_winnings =
  winnings_test "winnings for a full house win should be distributed properly"
    "jayden" stand_off_full_house 34.5

(*Quads beat anything*)
let quads =
  create_custom_table
    [ (*quads beat all: *) justin; jayden; jungkook; marcus; jonathan ]
    [
      create_custom_card 2 'd';
      create_custom_card 2 'h';
      create_custom_card 2 's';
      create_custom_card 7 'd';
      create_custom_card 8 'h';
    ]
    20.0 jonathan

let stand_off_quads = stand_off quads
let quads_test = stand_off_test "quads" stand_off_quads [ "justin" ]

let quads_winnings =
  winnings_test "winnings for a quads win should be distributed properly"
    "justin" stand_off_quads 45.2

(* straight flush *)

let straight_flush =
  create_custom_table
    [ (*quads beat all: *) chris; justin; daniel; jungkook; jeff; bric ]
    [
      create_custom_card 2 'd';
      create_custom_card 4 'c';
      create_custom_card 6 'c';
      create_custom_card 7 'c';
      create_custom_card 8 'c';
    ]
    20.0 jonathan

let stand_off_straight_flush = stand_off straight_flush

let straight_flush_test =
  stand_off_test "straight flush" stand_off_straight_flush [ "chris" ]

let straight_flush_winnings =
  winnings_test
    "winnings for a straight flush win should be distributed properly" "chris"
    stand_off_straight_flush 44.2

(****************************************************************************)
let stand_off_tests =
  [
    high_card_test;
    tie_test;
    pair_test;
    two_pair_test;
    trips_test;
    straight_test;
    straight_low_test;
    flush_test;
    flush_tie_test;
    full_house_test;
    quads_test;
    straight_flush_test;
  ]

let winnings_tests =
  [
    high_winnings;
    tie_winnings;
    pair_winnings;
    two_pair_winnings;
    trips_winnings;
    straight_winnings;
    low_straight_winnings;
    flush_winnings;
    flush_tie_winnings;
    full_house_winnings;
    quads_winnings;
    straight_flush_winnings;
  ]

let tests =
  "poker test suite"
  >::: List.flatten
         [
           stand_off_tests;
           winnings_tests;
           raise_tests;
           call_tests;
           check_tests;
           fold_tests;
         ]

let _ = run_test_tt_main tests
