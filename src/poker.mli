(** Representation of poker data *)

type card
(** the abstract type representing a playing card *)

type player
(** the abstract type representing a player *)

type table
(** the abstract type representing the table or the game state *)

exception PlayerSize
(** raised if invalid number of players n<2 or n>10 *)

val create_player : string -> float -> player
(** [create_player n m] creates a player with name n with m money.
          Requires: [n] must not already be the name of a current player. *)

val start : player list -> float -> float -> table
(** creates an empty table with an empty pot and no bets. The player who acts
      first is assigned by standard poker rules in which each player at the 
      table is assigned a card and the player with the highest card is 
      positioned as button, small blind, or big blind. 
      Raises an exception PlayerSize if player size is less than 2 or greater 
         than 10 *)

val assign_cards : table -> table
(** Each player in table will receive their cards. Requires: no player has
          their cards already *)

val pot_size : table -> float
(** returns float representing table pot size *)

val turn : table -> string
(** returns the name of the player that needs to act *)

val raise : table -> float -> table
(** raises the action player's bet for the current round *)

val find_next_player : table -> player

val check : table -> table
(** does nothing, moves action to the next player *)

val fold : table -> table
(** removes the player folding from the current round *)

val call : table -> table
(** matches the bet of the player whose turn it is to the table's current bet*)

val deal_cards : table -> table
(** deals the cards for the table, removes from deck *)

val stand_off : table -> table
(** determines the winner of the poker hand and their moneys adjusted 
    accordingly *)

val winner_names : table -> string list
(** returns the names of the winners with their updated money*)

val show_money : string -> table -> float
(** [show_money name table] returns the amount of money that a player in table
      whose name matches the name argument has *)

val show_board : table -> card list
(** prints the current board of the game *)

val show_cards : table -> unit
(** prints the acting player's hand *)

val check_game : table -> unit list
(** prints the pot size, current bet, and for each player, their name,
          current bet, and stack size *)

val create_custom_card : int -> char -> card
(** creates any card where create_card with the name corresponding to the first
      int and the char corresponding to the first letter of the desired suit *)

val create_custom_player : string -> float -> card list -> player
(** creates a player with hand of user's choice*)

val create_custom_table : player list -> card list -> float -> player -> table
(** creates a table with a custom list of players, custom player turn, custom
         pot size, and custom board *)

val get_current_bet : table -> float
(** returns the current bet of a table*)

val get_bet : table -> string -> float
(** returns the bet of a player given a table and the player name as a string *)

val get_players : table -> player list
(** returns the list of players in the game*)

val get_player_name : player -> string
(** returns the name of a player [p] *)

val fold_status : table -> string -> bool
(* returns the folded field of a given player give their name [n] *)

val new_round : table -> table
(** after round is finished, pass in the completed table state to restart 
    new round *)
