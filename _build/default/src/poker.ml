exception PlayerSize

type card = {
  name : string;
  suit : string;
  value : int;
}

type player = {
  name : string;
  cards : card list;
  bet : int;
  money : int;
}

type table = {
  players : player list;
  current_bet : int;
  pot : int;
  action : player;
}

let create_player n m = {
  name = n;
  cards = [];
  bet = 0;
  money = m;
}