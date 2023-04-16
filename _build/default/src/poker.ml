exception PlayerSize

open Random
open List

type card = { name : string; suit : string; value : int }
type player = { name : string; cards : card list; bet : int; money : int }
type table = { players : player list; pot : int; action : player }

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
let rec start_helper = function [] -> 0 | _ :: t -> 1 + start_helper t

let start p_list =
  let length = start_helper p_list in
  if length >= 2 && length <= 10 then
    {
      players = p_list;
      pot = 0;
      action = (match p_list with h :: _ -> h | _ -> raise PlayerSize);
    }
  else raise PlayerSize

let rec assign_helper p_list d =
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
      @ assign_helper t (filter (fun x -> x <> card_one && x <> card_two) d)

let assign_cards t =
  { players = assign_helper t.players deck; pot = t.pot; action = t.action }

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
  }

let rec find_next_player lst current_player =
  match lst with
  | [] -> failwith "no player"
  | h :: t ->
      if h = current_player then match t with h :: _ -> h | [] -> hd lst
      else find_next_player t current_player

let fold t =
  {
    players = filter (fun x -> x <> t.action) t.players;
    pot = t.pot;
    action = find_next_player t.players t.action;
  }
