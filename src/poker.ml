exception PlayerSize

open Random
open List

type suit = Clubs | Diamonds | Hearts | Spades
type card = { name : string; suit : suit; value : int }

type player = {
  name : string;
  cards : card list;
  bet : float;
  money : float;
  starting_pos : int;
  left : bool;
}

type table = {
  players : player list;
  current_bet : float;
  board : card list;
  pot : float;
  action : player;
  deck : card list;
  small_blind : float;
  big_blind : float;
}

type hands =
  | HighCard of int
  | Pair of int
  | TwoPair of int
  | Trips of int
  | Straight of int
  | Flush of int
  | FullHouse of (int * int)
  | Quads of int
  | StraightFlush of int

let card_info =
  [
    ("two", 2);
    ("three", 3);
    ("four", 4);
    ("five", 5);
    ("six", 6);
    ("seven", 7);
    ("eight", 8);
    ("nine", 9);
    ("ten", 10);
    ("jack", 11);
    ("queen", 12);
    ("king", 13);
    ("ace", 14);
  ]

let create_card info suit_type =
  match info with x, y -> { name = x; suit = suit_type; value = y }

let rec create_deck lst suit_type =
  match lst with
  | [] -> []
  | h :: t -> create_card h suit_type :: create_deck t suit_type

let full_deck =
  create_deck card_info Spades
  @ create_deck card_info Hearts
  @ create_deck card_info Diamonds
  @ create_deck card_info Clubs
  @ []

let create_player n m : player =
  { name = n; cards = []; bet = 0.; money = m; starting_pos = 0; left = false }

(* helper methods for start *)
let rec num_players = function [] -> 0 | _ :: t -> 1 + num_players t

let rec last_to_act lst =
  match lst with
  | [] -> raise PlayerSize
  | h :: t -> if t = [] then h else last_to_act t

let rec blind_assign p_list s b =
  match p_list with
  | [] -> []
  | h :: t ->
      if t = [] then
        {
          name = h.name;
          cards = h.cards;
          bet = b;
          money = h.money -. b;
          starting_pos = h.starting_pos;
          left = h.left;
        }
        :: blind_assign t s b
      else if tl t = [] then
        {
          name = h.name;
          cards = h.cards;
          bet = s;
          money = h.money -. s;
          starting_pos = h.starting_pos;
          left = h.left;
        }
        :: blind_assign t s b
      else h :: blind_assign t s b

let player_list_update lst s b =
  let unordered_plist =
    self_init ();
    map
      (fun x ->
        let rand_num = int 99999999 in
        {
          name = x.name;
          cards = x.cards;
          bet = x.bet;
          money = x.money;
          starting_pos = rand_num;
          left = x.left;
        })
      lst
  in
  blind_assign
    (fast_sort
       (fun x y -> Stdlib.compare x.starting_pos y.starting_pos)
       unordered_plist)
    s b

let rec update_player_cards p_list d =
  self_init ();
  match p_list with
  | [] -> []
  | h :: t ->
      let card_one = nth d (int (num_players d)) in
      let card_two =
        nth (filter (fun x -> x <> card_one) d) (int (num_players d - 1))
      in
      [
        {
          name = h.name;
          cards = [ card_one; card_two ];
          bet = h.bet;
          money = h.money;
          starting_pos = h.starting_pos;
          left = h.left;
        };
      ]
      @ update_player_cards t
          (filter (fun x -> x <> card_one && x <> card_two) d)

let rec update_deck player_cards deck =
  match player_cards with
  | [] -> deck
  | h :: t -> update_deck t (filter (fun x -> x <> h) deck)

let assign_cards t =
  let updated_player_list = update_player_cards t.players full_deck in
  let rec player_cards players =
    match players with [] -> [] | h :: t -> h.cards @ player_cards t
  in
  {
    players = updated_player_list;
    current_bet = t.current_bet;
    pot = t.pot;
    action = t.action;
    board = t.board;
    deck = update_deck (player_cards updated_player_list) t.deck;
    small_blind = t.small_blind;
    big_blind = t.big_blind;
  }

let start p_list s b =
  let length = num_players p_list in
  if length >= 2 && length <= 10 then
    let new_player_list = player_list_update p_list s b in
    let table =
      assign_cards
        {
          players = map (fun x -> { x with left = false }) new_player_list;
          current_bet = b;
          pot = s +. b;
          board = [];
          action =
            (match new_player_list with h :: _ -> h | _ -> raise PlayerSize);
          deck = full_deck;
          small_blind = s;
          big_blind = b;
        }
    in
    { table with action = hd table.players }
  else raise PlayerSize

let deal_cards t =
  self_init ();
  let burn_card = nth t.deck (int (length t.deck)) in
  let burnt =
    {
      players = t.players;
      pot = t.pot;
      current_bet = t.current_bet;
      action = t.action;
      board = t.board;
      deck = update_deck [ burn_card ] t.deck;
      small_blind = t.small_blind;
      big_blind = t.big_blind;
    }
  in
  let rec helper deck i acc =
    let card = nth deck (int (length deck)) in
    if i <> 0 then
      helper (filter (fun x -> x <> card) deck) (i - 1) (card :: acc)
    else acc
  in
  let table =
    {
      players = burnt.players;
      pot = burnt.pot;
      action = burnt.action;
      current_bet = t.current_bet;
      board =
        (match length t.board with
        | 0 -> helper burnt.deck 3 []
        | _ -> t.board @ helper burnt.deck 1 []);
      deck = burnt.deck;
      small_blind = t.small_blind;
      big_blind = t.big_blind;
    }
  in
  {
    players = table.players;
    pot = table.pot;
    action = table.action;
    current_bet = t.current_bet;
    board = table.board;
    deck = update_deck table.board table.deck;
    small_blind = t.small_blind;
    big_blind = t.big_blind;
  }

let rec restore_bets p_list =
  match p_list with
  | [] -> []
  | h :: t ->
      {
        name = h.name;
        cards = h.cards;
        bet = 0.;
        money = h.money;
        starting_pos = h.starting_pos;
        left = h.left;
      }
      :: restore_bets t

let rec post_flop_order table p_list =
  restore_bets
    (let lst =
       match p_list with
       | [] -> []
       | h :: t ->
           if tl t = [] then
             h :: hd t :: filter (fun x -> x <> h && x <> hd t) table.players
           else post_flop_order table t
     in
     if length lst = 2 then rev lst else lst)

let rec first_not_folded = function
  | [] -> failwith "no such player"
  | h :: t -> if not h.left then h else first_not_folded t

let rec not_folded_players = function
  | [] -> []
  | h :: t ->
      if not h.left then h :: not_folded_players t else not_folded_players t

let rec find_next_helper table lst =
  match lst with
  | [] -> failwith "no player"
  | h :: t ->
      if h.name = table.action.name then
        match t with
        | [] ->
            if not (hd table.players).left then hd table.players
            else first_not_folded table.players
        | h' :: t' -> (
            if not h'.left then h'
            else
              try first_not_folded t'
              with Failure _ -> (
                try first_not_folded table.players
                with Failure m -> failwith m))
      else find_next_helper table t

let find_next_player t = find_next_helper t t.players

let check_flop t p =
  let last = last_to_act p in
  if
    length t.board <> 0
    || (for_all (fun x -> x.bet = t.big_blind) p && t.action = last)
  then t
  else if for_all (fun x -> x.bet = t.current_bet) p then
    let table = deal_cards t in
    {
      players = post_flop_order t t.players;
      pot = t.pot;
      current_bet = 0.;
      board = table.board;
      action =
        (let order = post_flop_order t t.players in
         if not (hd order).left then hd order else first_not_folded order);
      deck = table.deck;
      small_blind = t.small_blind;
      big_blind = t.big_blind;
    }
  else t

let check_turn t =
  let not_folded = not_folded_players t.players in
  if for_all (fun x -> x.bet = t.current_bet) not_folded then
    let table = deal_cards t in
    {
      players = restore_bets t.players;
      current_bet = 0.;
      pot = t.pot;
      board = table.board;
      action = hd (not_folded_players (restore_bets t.players));
      deck = table.deck;
      small_blind = t.small_blind;
      big_blind = t.big_blind;
    }
  else t

let check_river t =
  let not_folded = not_folded_players t.players in
  if for_all (fun x -> x.bet = t.current_bet) not_folded then
    let table = deal_cards t in
    {
      players = restore_bets t.players;
      current_bet = 0.;
      pot = t.pot;
      board = table.board;
      action = hd (not_folded_players (restore_bets t.players));
      deck = table.deck;
      small_blind = t.small_blind;
      big_blind = t.big_blind;
    }
  else t

let pot_size t = t.pot
let turn t = t.action.name

let raise t a =
  let post_action_table =
    {
      players =
        fold_left
          (fun acc x ->
            if x.name <> t.action.name then acc @ [ x ]
            else
              acc
              @ [
                  {
                    name = x.name;
                    cards = x.cards;
                    bet = a;
                    money = x.money -. (a -. x.bet);
                    starting_pos = x.starting_pos;
                    left = x.left;
                  };
                ])
          [] t.players;
      pot = t.pot +. (a -. t.action.bet);
      current_bet = a;
      board = t.board;
      action = find_next_player t;
      deck = t.deck;
      small_blind = t.small_blind;
      big_blind = t.big_blind;
    }
  in
  let not_folded = not_folded_players t.players in
  let no_bets = for_all (fun x -> x.bet = 0.) not_folded in
  match length t.board with
  | 0 ->
      check_flop post_action_table
        (not_folded_players post_action_table.players)
  | 3 ->
      if no_bets && post_action_table.action = hd not_folded then
        check_turn post_action_table
      else if no_bets && post_action_table.action <> hd not_folded then
        post_action_table
      else check_turn post_action_table
  | 4 ->
      if no_bets && post_action_table.action = hd not_folded then
        check_river post_action_table
      else if no_bets && post_action_table.action <> hd not_folded then
        post_action_table
      else check_river post_action_table
  | _ -> post_action_table

let check t =
  let post_action_table =
    if t.current_bet = t.action.bet then
      {
        players = t.players;
        current_bet = t.current_bet;
        pot = t.pot;
        action = find_next_player t;
        board = t.board;
        deck = t.deck;
        small_blind = t.small_blind;
        big_blind = t.big_blind;
      }
    else
      failwith ("Cannot check. " ^ t.action.name ^ " must fold, call, or raise.")
  in
  let not_folded = not_folded_players t.players in
  let no_bets = for_all (fun x -> x.bet = 0.) t.players in
  match length t.board with
  | 0 ->
      check_flop post_action_table
        (not_folded_players post_action_table.players)
  | 3 ->
      if no_bets && post_action_table.action = hd not_folded then
        check_turn post_action_table
      else if no_bets && post_action_table.action <> hd not_folded then
        post_action_table
      else check_turn post_action_table
  | 4 ->
      if no_bets && post_action_table.action = hd not_folded then
        check_river post_action_table
      else if no_bets && post_action_table.action <> hd not_folded then
        post_action_table
      else check_river post_action_table
  | _ -> post_action_table

let fold t =
  let p =
    map
      (fun x -> if x.name = t.action.name then { x with left = true } else x)
      t.players
  in
  let post_action_table =
    {
      players = p;
      pot = t.pot;
      current_bet = t.current_bet;
      action = find_next_player { t with players = p };
      board = t.board;
      deck = t.deck;
      small_blind = t.small_blind;
      big_blind = t.big_blind;
    }
  in
  let not_folded = not_folded_players post_action_table.players in
  let possible_winner =
    if length not_folded < 2 then
      let updated_players =
        List.map
          (fun x ->
            if List.exists (fun a -> x = a) not_folded then
              { x with money = x.money +. post_action_table.pot }
            else x)
          post_action_table.players
      in
      { post_action_table with players = updated_players }
    else post_action_table
  in
  if possible_winner <> post_action_table then (
    print_string ("Round over. Winner is " ^ (hd not_folded).name);
    possible_winner)
  else
    let no_bets = for_all (fun x -> x.bet = 0.) t.players in
    match length t.board with
    | 0 ->
        check_flop post_action_table
          (not_folded_players post_action_table.players)
    | 3 ->
        if no_bets && post_action_table.action = hd not_folded then
          check_turn post_action_table
        else if no_bets && post_action_table.action <> hd not_folded then
          post_action_table
        else check_turn post_action_table
    | 4 ->
        if no_bets && post_action_table.action = hd not_folded then
          check_river post_action_table
        else if no_bets && post_action_table.action <> hd not_folded then
          post_action_table
        else check_river post_action_table
    | _ -> post_action_table

let rec call_helper t lst =
  match lst with
  | [] -> []
  | h :: tl ->
      if h.name = t.action.name then
        {
          name = h.name;
          cards = h.cards;
          bet =
            (if t.current_bet > t.action.money +. t.action.bet then
             t.action.money +. t.action.bet
            else t.current_bet);
          money =
            (if t.current_bet > t.action.money +. t.action.bet then 0.
            else t.action.money -. (t.current_bet -. t.action.bet));
          starting_pos = h.starting_pos;
          left = h.left;
        }
        :: call_helper t tl
      else h :: call_helper t tl

let call t =
  let post_action_table =
    {
      players = call_helper t t.players;
      pot =
        (if t.current_bet > t.action.money +. t.action.bet then
         t.pot +. (t.action.money +. t.action.bet)
        else t.pot +. (t.current_bet -. t.action.bet));
      current_bet = t.current_bet;
      action =
        find_next_player
          {
            pot = t.pot +. (t.current_bet -. t.action.bet);
            current_bet = t.current_bet;
            action = t.action;
            players = call_helper t t.players;
            board = t.board;
            deck = t.deck;
            small_blind = t.small_blind;
            big_blind = t.big_blind;
          };
      board = t.board;
      deck = t.deck;
      small_blind = t.small_blind;
      big_blind = t.big_blind;
    }
  in
  let not_folded = not_folded_players t.players in
  let no_bets = for_all (fun x -> x.bet = 0.) t.players in
  match length t.board with
  | 0 ->
      check_flop post_action_table
        (not_folded_players post_action_table.players)
  | 3 ->
      if no_bets && post_action_table.action = hd not_folded then
        check_turn post_action_table
      else if no_bets && post_action_table.action <> hd not_folded then
        post_action_table
      else check_turn post_action_table
  | 4 ->
      if no_bets && post_action_table.action = hd not_folded then
        check_river post_action_table
      else if no_bets && post_action_table.action <> hd not_folded then
        post_action_table
      else check_river post_action_table
  | _ -> post_action_table

let best_matches cards =
  let sorted = sort (fun x y -> x.value - y.value) cards in
  let high_card = HighCard (nth sorted (length sorted - 1)).value in
  let best_hand = ref high_card in
  for i = 2 to 14 do
    let temp = filter (fun x -> x.value = i) sorted in
    match length temp with
    | 1 -> ()
    | 2 -> (
        match !best_hand with
        | HighCard _ -> best_hand := Pair i
        | Pair _ -> best_hand := TwoPair i
        | TwoPair _ -> best_hand := TwoPair i
        | Trips t -> best_hand := FullHouse (i, t)
        | FullHouse (_, t) -> best_hand := FullHouse (i, t)
        | _ -> ())
    | 3 -> (
        match !best_hand with
        | Pair p -> best_hand := FullHouse (p, i)
        | FullHouse (p, _) -> best_hand := FullHouse (p, i)
        | Quads _ -> ()
        | _ -> best_hand := Trips i)
    | 4 -> best_hand := Quads i
    | _ -> best_hand := !best_hand
  done;
  !best_hand

let straight sorted_uniq =
  let counter = ref 1 in
  let found_straight = ref false in
  let array = Array.of_list sorted_uniq in
  let temp = ref array.(0).value in
  let straight = ref (Some (Straight !temp)) in
  let _ =
    if !temp = 2 && array.(Array.length array - 1).value = 14 then
      counter := !counter + 1
  in
  let _ =
    for i = 1 to Array.length array - 1 do
      if array.(i).value = !temp + 1 then (
        counter := !counter + 1;
        straight := Some (Straight array.(i).value))
      else if !counter > 4 then found_straight := true
      else counter := 1;
      temp := array.(i).value
    done
  in
  if !counter > 4 || !found_straight then !straight else None

let flush cards =
  let sorted = sort (fun x y -> x.value - y.value) cards in
  let clubs = filter (fun x -> x.suit = Clubs) sorted in
  let diamonds = filter (fun x -> x.suit = Diamonds) sorted in
  let hearts = filter (fun x -> x.suit = Hearts) sorted in
  let spades = filter (fun x -> x.suit = Spades) sorted in
  let flush list = Some (Flush (nth list (length list - 1)).value) in
  let straight_flush list =
    straight (sort_uniq (fun x y -> x.value - y.value) list)
  in
  if length clubs > 4 then
    match straight_flush clubs with
    | Some (Straight v) -> Some (StraightFlush v)
    | None -> flush clubs
    | _ -> failwith "straight error"
  else if length diamonds > 4 then
    match straight_flush diamonds with
    | Some (Straight v) -> Some (StraightFlush v)
    | None -> flush diamonds
    | _ -> failwith "straight error"
  else if length hearts > 4 then
    match straight_flush hearts with
    | Some (Straight v) -> Some (StraightFlush v)
    | None -> flush hearts
    | _ -> failwith "straight error"
  else if length spades > 4 then
    match straight_flush spades with
    | Some (Straight v) -> Some (StraightFlush v)
    | None -> flush spades
    | _ -> failwith "straight error"
  else None

let best_hand cards =
  let matches = best_matches cards in
  let straight = straight (sort_uniq (fun x y -> x.value - y.value) cards) in
  let flush = flush cards in
  match flush with
  | Some (StraightFlush v) -> StraightFlush v
  | Some (Flush v) -> (
      match matches with
      | FullHouse v -> FullHouse v
      | Quads v -> Quads v
      | _ -> Flush v)
  | _ -> (
      match matches with
      | FullHouse v -> FullHouse v
      | Quads v -> Quads v
      | _ -> (
          match straight with Some (Straight v) -> Straight v | _ -> matches))

let create_hands p_list board =
  map (fun x -> (x, best_hand (x.cards @ board))) p_list

let compare_hands h1 h2 =
  match h1 with
  | HighCard v1 -> ( match h2 with HighCard v2 -> v1 - v2 | _ -> -1)
  | Pair v1 -> ( match h2 with HighCard _ -> 1 | Pair v2 -> v1 - v2 | _ -> -1)
  | TwoPair v1 -> (
      match h2 with
      | HighCard _ -> 1
      | Pair _ -> 1
      | TwoPair v2 -> v1 - v2
      | _ -> -1)
  | Trips v1 -> (
      match h2 with
      | HighCard _ -> 1
      | Pair _ -> 1
      | TwoPair _ -> 1
      | Trips v2 -> v1 - v2
      | _ -> -1)
  | Straight v1 -> (
      match h2 with
      | HighCard _ -> 1
      | Pair _ -> 1
      | TwoPair _ -> 1
      | Trips _ -> 1
      | Straight v2 -> v1 - v2
      | _ -> -1)
  | Flush v1 -> (
      match h2 with
      | Flush v2 -> v1 - v2
      | FullHouse _ -> -1
      | Quads _ -> -1
      | StraightFlush _ -> -1
      | _ -> 1)
  | FullHouse (_, t1) -> (
      match h2 with
      | FullHouse (_, t2) -> t1 - t2
      | Quads _ -> -1
      | StraightFlush _ -> -1
      | _ -> 1)
  | Quads v1 -> (
      match h2 with Quads v2 -> v1 - v2 | StraightFlush _ -> -1 | _ -> 1)
  | StraightFlush v1 -> ( match h2 with StraightFlush v2 -> v1 - v2 | _ -> 1)

let stand_off t =
  let rank_players =
    sort
      (fun x y -> match (x, y) with (_, h), (_, h') -> compare_hands h h')
      (create_hands t.players t.board)
  in
  let best_hand =
    match nth rank_players (length rank_players - 1) with _, h -> h
  in
  let winners =
    map
      (fun x ->
        match x with
        | p, h ->
            if compare_hands best_hand h <> 0 && Bool.not p.left then
              { p with left = true }
            else p)
      rank_players
  in
  let stand_off =
    {
      players = winners;
      pot = t.pot;
      current_bet = t.current_bet;
      action = t.action;
      board = t.board;
      deck = t.deck;
      small_blind = t.small_blind;
      big_blind = t.big_blind;
    }
  in
  let number_of_winners =
    List.fold_left
      (fun acc x -> if x.left then acc else acc + 1)
      0 stand_off.players
  in
  let winnings =
    List.map
      (fun x ->
        if x.left = false then
          {
            x with
            money = x.money +. (stand_off.pot /. float_of_int number_of_winners);
          }
        else x)
      stand_off.players
  in
  { stand_off with players = winnings; pot = 0.0 }

let show_money name t =
  match List.filter (fun x -> x.name = name) t.players with
  | [] -> failwith "invalid player"
  | h :: _ -> h.money

let show_board (t : table) = t.board

let rec show_cards_helper l =
  match l with
  | [] -> ()
  | h :: t -> (
      match h.suit with
      | Hearts ->
          print_string h.name;
          print_newline ();
          print_string "hearts";
          print_newline ();
          print_newline ();
          show_cards_helper t
      | Spades ->
          print_string h.name;
          print_newline ();
          print_string "spades";
          print_newline ();
          print_newline ();
          show_cards_helper t
      | Clubs ->
          print_string h.name;
          print_newline ();
          print_string "clubs";
          print_newline ();
          print_newline ();
          show_cards_helper t
      | Diamonds ->
          print_string h.name;
          print_newline ();
          print_string "diamonds";
          print_newline ();
          print_newline ();
          show_cards_helper t)

let show_cards (t : table) = show_cards_helper t.action.cards

let check_game (t : table) =
  print_string ("pot: " ^ string_of_float t.pot);
  print_newline ();
  print_string ("current bet: " ^ string_of_float t.current_bet);
  print_newline ();
  print_newline ();
  print_string ("action: " ^ t.action.name);
  print_newline ();
  print_newline ();
  List.map
    (fun x ->
      print_string ("player: " ^ x.name);
      print_newline ();
      print_string ("bet: " ^ string_of_float x.bet);
      print_newline ();
      print_string ("money: " ^ string_of_float x.money);
      print_newline ();
      print_newline ())
    t.players

let winner_names stand_off =
  let winners = List.filter (fun x -> Bool.not x.left) stand_off.players in
  List.map (fun x -> x.name) winners

let create_custom_card v suit =
  let s =
    match suit with
    | 'c' -> Clubs
    | 'd' -> Diamonds
    | 'h' -> Hearts
    | 's' -> Spades
    | _ -> failwith "not a valid char"
  in
  let n =
    match v with
    | 2 -> "two"
    | 3 -> "three"
    | 4 -> "four"
    | 5 -> "five"
    | 6 -> "six"
    | 7 -> "seven"
    | 8 -> "eight"
    | 9 -> "nine"
    | 10 -> "ten"
    | 11 -> "jack"
    | 12 -> "queen"
    | 13 -> "king"
    | 14 -> "ace"
    | _ -> failwith "not a valid int"
  in
  let card = { name = n; suit = s; value = v } in
  assert (card.name = card.name);
  card

let create_custom_player n m c =
  { name = n; cards = c; bet = 0.0; money = m; starting_pos = 0; left = false }

let create_custom_table p_list custom_board custom_pot custom_action =
  {
    players = p_list;
    current_bet = 0.0;
    board = custom_board;
    pot = custom_pot;
    action = custom_action;
    deck = update_deck custom_board full_deck;
    small_blind = 0.0;
    big_blind = 0.0;
  }

let get_current_bet t = t.current_bet

let get_bet t n =
  List.fold_left
    (fun acc x -> if x.name = n then acc +. x.bet else acc)
    0. t.players

let get_players t = t.players
let get_player_name p = p.name

let fold_status t n =
  List.fold_left
    (fun acc x -> if x.name = n then x.left else acc)
    false t.players

let new_round t =
  let update_players = List.map (fun x -> { x with left = false }) t.players in
  start update_players 0. 0.
