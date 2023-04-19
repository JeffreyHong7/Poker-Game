exception PlayerSize
exception EndOfHand of string

open Random
open List

type card = { name : string; suit : string; value : int }

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
  pot : float;
  action : player;
  table_deck : card list;
  small_blind : float;
  big_blind : float;
}

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

let deck =
  create_deck card_info "Spades"
  @ create_deck card_info "Hearts"
  @ create_deck card_info "Diamonds"
  @ create_deck card_info "Clubs"
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
  if
    for_all (fun x -> x.bet = t.current_bet) t.players
    && t.action = hd t.players
  then
    {
      players = post_flop_order t t.players;
      current_bet = 0.;
      pot = t.pot;
      action = hd (post_flop_order t t.players);
      table_deck = t.table_deck;
      small_blind = t.small_blind;
      big_blind = t.big_blind;
    }
  else t

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
  | [] -> []
  | h :: t -> update_deck t (List.filter (fun x -> x <> h) deck)

let assign_cards t =
  let updated_player_list = update_player_cards t.players deck in
  let rec player_cards players =
    match players with [] -> [] | h :: t -> h.cards @ player_cards t
  in
  {
    players = updated_player_list;
    current_bet = t.current_bet;
    pot = t.pot;
    action = t.action;
    table_deck = update_deck (player_cards updated_player_list) t.table_deck;
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
        action =
          (match new_player_list with h :: _ -> h | _ -> raise PlayerSize);
        table_deck = deck;
        small_blind = s;
        big_blind = b;
      }
  else raise PlayerSize

let pot_size t = t.pot
let turn t = t.action.name

let rec raise_helper t lst =
  match lst with
  | [] -> raise PlayerSize
  | h :: tl ->
      if h.name = t.action.name then
        match tl with [] -> hd t.players | temp :: _ -> temp
      else raise_helper t tl

let raise t a =
  check_flop
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
      action = raise_helper t t.players;
      table_deck = t.table_deck;
      small_blind = t.small_blind;
      big_blind = t.big_blind;
    }

let rec find_next_helper lst current =
  match lst with
  | [] -> failwith "no player"
  | h :: t ->
      if h = current then match t with h :: _ -> h | [] -> hd lst
      else find_next_helper t current

let rec find_next_player t = find_next_helper t.players t.action

let check t =
  check_flop
    (if t.current_bet = t.action.bet then
     {
       players = t.players;
       current_bet = t.current_bet;
       pot = t.pot;
       action = find_next_player t;
       table_deck = t.table_deck;
       small_blind = t.small_blind;
       big_blind = t.big_blind;
     }
    else
      failwith ("Cannot check. " ^ t.action.name ^ " must fold, call, or raise."))

let fold t =
  check_win
    (check_flop
       {
         players = filter (fun x -> x <> t.action) t.players;
         pot = t.pot;
         current_bet = t.current_bet;
         action = find_next_player t;
         table_deck = t.table_deck;
         small_blind = t.small_blind;
         big_blind = t.big_blind;
       })

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
  check_flop
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
            table_deck = t.table_deck;
            small_blind = t.small_blind;
            big_blind = t.big_blind;
          };
      table_deck = t.table_deck;
      small_blind = t.small_blind;
      big_blind = t.big_blind;
    }
