(** Slay the Spire-like card battle game logic *)
open! Core

(** Card types and utilities *)
module Card : sig
  type t =
    | Strike  (** Deal 6 damage *)
    | Defend  (** Gain 5 block *)
    | Heal    (** Restore 5 health *)
    | Special of string  (** Special abilities *)
  [@@deriving sexp, compare, equal]

  (** Convert card to string representation *)
  val to_string : t -> string

  (** Get the energy cost of a card *)
  val energy_cost : t -> int

  (** Get the description of what a card does *)
  val description : t -> string
end

(** Player state and operations *)
module Player_state : sig
  type t =
    { name : string
    ; health : int
    ; max_hp : int
    ; energy : int
    ; max_energy : int
    ; block : int  (** Temporary defense that resets each turn *)
    ; hand : Card.t list
    ; draw_pile : Card.t list
    ; discard_pile : Card.t list
    }
  [@@deriving sexp, compare, equal]

  (** Create a new player with starting deck *)
  val create : name:string -> max_hp:int -> max_energy:int -> starting_deck:Card.t list -> t

  (** Check if player is still alive *)
  val is_alive : t -> bool

  (** Apply damage to player (reduced by block) *)
  val take_damage : t -> int -> t

  (** Restore health (up to max_hp) *)
  val heal : t -> int -> t

  (** Add temporary block *)
  val gain_block : t -> int -> t

  (** Draw cards from draw pile to hand *)
  val draw_cards : t -> int -> t

  (** Spend energy if available *)
  val spend_energy : t -> int -> (t, string) Result.t

  (** Reset energy and block at start of turn *)
  val start_turn : t -> t
end

(** Enemy state and behavior *)
module Enemy_state : sig
  type t =
    { kind : string
    ; health : int
    ; max_hp : int
    ; intent : string  (** e.g., "attack", "defend" *)
    ; damage_intent : int  (** How much damage the enemy plans to deal *)
    }
  [@@deriving sexp, compare, equal]

  (** Create a new enemy *)
  val create : kind:string -> max_hp:int -> intent:string -> damage_intent:int -> t

  (** Check if enemy is still alive *)
  val is_alive : t -> bool

  (** Apply damage to enemy *)
  val take_damage : t -> int -> t

  (** Get the action the enemy will perform *)
  val get_action : t -> [ `Attack of int | `Defend | `Wait ]
end

(** Game decision/status *)
module Decision : sig
  type t =
    | In_progress of { whose_turn : [ `Player1 | `Player2 | `Enemy ] }
    | Victory  (** All enemies defeated *)
    | Defeat   (** All players defeated *)
  [@@deriving sexp, compare, equal]

  (** Check if the game has ended *)
  val is_game_over : t -> bool
end

(** Complete game state and operations *)
module Game_state : sig
  type t =
    { player1 : Player_state.t
    ; player2 : Player_state.t
    ; enemies : Enemy_state.t list
    ; floor : int
    ; decision : Decision.t
    ; turn_count : int
    }
  [@@deriving sexp, compare, equal]

  (** Errors that can occur during game creation *)
  module Create_error : sig
    type t =
      | Invalid_floor
      | Empty_deck
      | Invalid_player_count
    [@@deriving sexp, compare, equal]
  end

  (** Create a new game state *)
  val create :
    floor:int ->
    player1_deck:Card.t list ->
    player2_deck:Card.t list ->
    enemies:Enemy_state.t list ->
    (t, Create_error.t list) Result.t

  (** Check if the game should end (victory or defeat) *)
  val check_game_over : t -> Decision.t

  (** Errors that can occur when making a move *)
  module Move_error : sig
    type t =
      | Game_is_over
      | Not_player_turn
      | Invalid_card
      | Invalid_target
      | Not_enough_energy
      | Card_not_in_hand
    [@@deriving sexp, compare]
  end

  (** A player's move *)
  module Move : sig
    type target =
      [ `Enemy of int
      | `Player1
      | `Player2
      ]
    [@@deriving sexp, compare, equal]

    type t =
      { card : Card.t
      ; target : target
      }
    [@@deriving sexp, compare, equal]
  end

  (** Execute a player's move *)
  val make_move : t -> Move.t -> (t, Move_error.t) Result.t

  (** End the current player's turn and advance to the next player/enemy *)
  val end_turn : t -> t

  (** Process the enemy's turn (AI attacks) *)
  val process_enemy_turn : t -> t

  (** Get all valid moves for the current player *)
  val get_available_moves : t -> Move.t list
end
