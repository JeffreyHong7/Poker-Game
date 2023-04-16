exception PlayerSize

open Random
open List

type card = { name : string; suit : string; value : int }
type player = { name : string; cards : card list; bet : int; money : int }

type table = {
  players : player list;
  pot : int;
  action : player;
  table_deck : card list;
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

let create_player n m = { name = n; cards = []; bet = 0; money = m }

(* helper methods for start *)
let rec start_helper = function [] -> 0 | _ :: t -> 1 + start_helper t

let rec action_helper lst assign_index acc =
  match lst with
  | [] -> failwith "unable to assign player"
  | h :: t ->
      if acc = assign_index then h else action_helper t assign_index (acc + 1)

let start p_list =
  let length = start_helper p_list in
  if length >= 2 && length <= 10 then
    {
      players = p_list;
      pot = 0;
      action = action_helper p_list (int length) 0;
      table_deck = deck;
    }
  else raise PlayerSize

let rec update_player_cards p_list d =
  self_init ();
  match p_list with
  | [] -> []
  | h :: t ->
      let card_one = nth d (int (start_helper d)) in
      let card_two =
        nth (filter (fun x -> x <> card_one) d) (int (start_helper d - 1))
      in
      [
        {
          name = h.name;
          cards = [ card_one; card_two ];
          bet = h.bet;
          money = h.money;
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
    pot = t.pot;
    action = t.action;
    table_deck = update_deck (player_cards updated_player_list) t.table_deck;
  }

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
  {
    players =
      fold_left
        (fun acc x ->
          if x.name <> t.action.name then acc @ [ x ]
          else
            acc
            @ [
                { name = x.name; cards = x.cards; bet = a; money = x.money - a };
              ])
        [] t.players;
    pot = t.pot + a;
    action = raise_helper t t.players;
    table_deck = t.table_deck;
  }

let rec find_next_helper lst current =
  match lst with
  | [] -> failwith "no player"
  | h :: t ->
      if h = current then match t with h :: _ -> h | [] -> hd lst
      else find_next_helper t current

let rec find_next_player t = find_next_helper t.players t.action

let fold t =
  {
    players = filter (fun x -> x <> t.action) t.players;
    pot = t.pot;
    action = find_next_player t;
    table_deck = t.table_deck;
  }
