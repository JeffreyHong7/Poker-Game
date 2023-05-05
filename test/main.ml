open OUnit2
open Poker

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
  "poker test suite" >::: List.flatten [ stand_off_tests; winnings_tests ]

let _ = run_test_tt_main tests
