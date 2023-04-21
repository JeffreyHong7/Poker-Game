(** Representation of poker data

     This module represents the types and functions that a player can proceed
   with *)

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

val start : player list -> table
(** creates an empty table with an empty pot and no bets. The player who acts 
first is assigned by standard poker rules in which each player at the table
    is assigned a card and the player with the highest card is positioned as 
    button, small blind, or big blind. Raises an exception PlayerSize if player size is less 
      than 2 or greater than 10 *)

val assign_cards : table -> table
(** Each player in table will receive their cards. Requires: no player has 
    their cards already *)

val pot_size : table -> float
(** returns table pot size *)

val turn : table -> string
(** returns the name of the player that needs to act *)

val raise : table -> float -> table
(** raises the action player's bet for the current round *)

val find_next_player : table -> player

val fold : table -> table
(** removes the player folding from the current round *)

val deal_cards : table -> table
(** deals the cards for the table, removes from deck *)

val stand_off : table -> table
(** determines the winner of the pokker hand *)
