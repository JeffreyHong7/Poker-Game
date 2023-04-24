exception PlayerSize
exception EndOfHand of string

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

let check_win t =
  if length t.players = 1 then
    raise
      (EndOfHand
         ("Winner: "
         ^
         match t.players with
         | h :: _ -> h.name ^ " !"
         | _ -> failwith "Impossible"))
  else t

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

let create_player n m =
  { name = n; cards = []; bet = 0.; money = m; starting_pos = 0 }

(* helper methods for start *)
let rec num_players = function [] -> 0 | _ :: t -> 1 + num_players t

let rec action_helper lst assign_index acc =
  match lst with
  | [] -> failwith "unable to assign player"
  | h :: t ->
      if acc = assign_index then h else action_helper t assign_index (acc + 1)

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
        }
        :: blind_assign t s b
      else if tl t = [] then
        {
          name = h.name;
          cards = h.cards;
          bet = s;
          money = h.money -. s;
          starting_pos = h.starting_pos;
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
    assign_cards
      {
        players = new_player_list;
        current_bet = b;
        pot = s +. b;
        board = [];
        action =
          (match new_player_list with h :: _ -> h | _ -> raise PlayerSize);
        deck = full_deck;
        small_blind = s;
        big_blind = b;
      }
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

let check_flop t =
  let big_blind = last_to_act t.players in
  if
    length t.board <> 0
    || (for_all (fun x -> x.bet = t.big_blind) t.players && t.action = big_blind)
  then t
  else if for_all (fun x -> x.bet = t.current_bet) t.players then
    let table = deal_cards t in
    {
      players = post_flop_order t t.players;
      pot = t.pot;
      current_bet = 0.;
      board = table.board;
      action = hd (post_flop_order t t.players);
      deck = table.deck;
      small_blind = t.small_blind;
      big_blind = t.big_blind;
    }
  else t

let check_turn t =
  if for_all (fun x -> x.bet = t.current_bet) t.players then
    let table = deal_cards t in
    {
      players = t.players;
      current_bet = 0.;
      pot = t.pot;
      board = table.board;
      action = hd t.players;
      deck = table.deck;
      small_blind = t.small_blind;
      big_blind = t.big_blind;
    }
  else t

let check_river t =
  if for_all (fun x -> x.bet = t.current_bet) t.players then
    let table = deal_cards t in
    {
      players = t.players;
      current_bet = 0.;
      pot = t.pot;
      board = table.board;
      action = hd t.players;
      deck = table.deck;
      small_blind = t.small_blind;
      big_blind = t.big_blind;
    }
  else t

let pot_size t = t.pot
let turn t = t.action.name

let rec find_next_helper table lst =
  match lst with
  | [] -> failwith "no player"
  | h :: t ->
      if h.name = table.action.name then
        match t with h' :: _ -> h' | [] -> hd table.players
      else find_next_helper table t

let rec find_next_player t = find_next_helper t t.players

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
                    money = x.money -. a;
                    starting_pos = x.starting_pos;
                  };
                ])
          [] t.players;
      pot = t.pot +. a;
      current_bet = a;
      board = t.board;
      action = find_next_player t;
      deck = t.deck;
      small_blind = t.small_blind;
      big_blind = t.big_blind;
    }
  in
  let no_bets = for_all (fun x -> x.bet = 0.) t.players in
  match length t.board with
  | 0 -> check_flop post_action_table
  | 3 ->
      if no_bets && post_action_table.action = hd t.players then
        check_turn post_action_table
      else if no_bets && post_action_table.action <> hd t.players then
        post_action_table
      else check_turn post_action_table
  | 4 ->
      if no_bets && post_action_table.action = hd t.players then
        check_river post_action_table
      else if no_bets && post_action_table.action <> hd t.players then
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
  let no_bets = for_all (fun x -> x.bet = 0.) t.players in
  match length t.board with
  | 0 -> check_flop post_action_table
  | 3 ->
      if no_bets && post_action_table.action = hd t.players then
        check_turn post_action_table
      else if no_bets && post_action_table.action <> hd t.players then
        post_action_table
      else check_turn post_action_table
  | 4 ->
      if no_bets && post_action_table.action = hd t.players then
        check_river post_action_table
      else if no_bets && post_action_table.action <> hd t.players then
        post_action_table
      else check_river post_action_table
  | _ -> post_action_table

let fold t =
  let post_action_table =
    {
      players = filter (fun x -> x <> t.action) t.players;
      pot = t.pot;
      current_bet = t.current_bet;
      action = find_next_player t;
      board = t.board;
      deck = t.deck;
      small_blind = t.small_blind;
      big_blind = t.big_blind;
    }
  in
  let no_bets = for_all (fun x -> x.bet = 0.) t.players in
  match length t.board with
  | 0 -> check_flop post_action_table
  | 3 ->
      if no_bets && post_action_table.action = hd t.players then
        check_turn post_action_table
      else if no_bets && post_action_table.action <> hd t.players then
        post_action_table
      else check_turn post_action_table
  | 4 ->
      if no_bets && post_action_table.action = hd t.players then
        check_river post_action_table
      else if no_bets && post_action_table.action <> hd t.players then
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
          bet = t.current_bet;
          money = h.money -. (t.current_bet -. h.bet);
          starting_pos = h.starting_pos;
        }
        :: call_helper t tl
      else h :: call_helper t tl

let call t =
  let post_action_table =
    {
      players = call_helper t t.players;
      pot = t.pot +. (t.current_bet -. t.action.bet);
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
  let no_bets = for_all (fun x -> x.bet = 0.) t.players in
  match length t.board with
  | 0 -> check_flop post_action_table
  | 3 ->
      if no_bets && post_action_table.action = hd t.players then
        check_turn post_action_table
      else if no_bets && post_action_table.action <> hd t.players then
        post_action_table
      else check_turn post_action_table
  | 4 ->
      if no_bets && post_action_table.action = hd t.players then
        check_river post_action_table
      else if no_bets && post_action_table.action <> hd t.players then
        post_action_table
      else check_river post_action_table
  | _ -> post_action_table

let rec best_matches cards =
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
    | _ -> best_hand := Quads i
  done;
  !best_hand

let rec straight sorted_uniq acc high =
  match sorted_uniq with
  | [] -> if acc > 4 then Some (Straight high) else None
  | h :: t ->
      let temp = filter (fun x -> x.value = h.value + 1) t in
      if length temp = 1 then straight t (acc + 1) (nth temp 0).value
      else straight t acc high

let flush cards =
  let sorted = sort (fun x y -> x.value - y.value) cards in
  let clubs = filter (fun x -> x.suit = Clubs) sorted in
  let diamonds = filter (fun x -> x.suit = Diamonds) sorted in
  let hearts = filter (fun x -> x.suit = Hearts) sorted in
  let spades = filter (fun x -> x.suit = Spades) sorted in
  let flush list = Some (Flush (nth list (length list - 1)).value) in
  let straight_flush list =
    straight (sort_uniq (fun x y -> x.value - y.value) list) 0 0
  in
  if length clubs > 4 then
    match straight_flush clubs with Some s -> Some s | None -> flush clubs
  else if length diamonds > 4 then
    match straight_flush diamonds with
    | Some s -> Some s
    | None -> flush diamonds
  else if length hearts > 4 then
    match straight_flush hearts with Some s -> Some s | None -> flush hearts
  else if length spades > 4 then
    match straight_flush spades with Some s -> Some s | None -> flush spades
  else None

let best_hand cards =
  let matches = best_matches cards in
  let straight =
    straight (sort_uniq (fun x y -> x.value - y.value) cards) 0 0
  in
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

let rec compare_hands h1 h2 =
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
  | Quads _ -> ( match h2 with StraightFlush _ -> -1 | _ -> 1)
  | StraightFlush _ -> 1

let stand_off t =
  let rank_players =
    sort
      (fun x y -> match (x, y) with (_, h), (_, h') -> compare_hands h h')
      (create_hands t.players t.board)
  in
  let winners =
    filter
      (fun x -> x = nth rank_players (length rank_players - 1))
      rank_players
  in
  {
    players = map (fun x -> match x with p, _ -> p) winners @ [];
    pot = t.pot;
    current_bet = t.current_bet;
    action = t.action;
    board = t.board;
    deck = t.deck;
    small_blind = t.small_blind;
    big_blind = t.big_blind;
  }
