open! Core
(** A card-based battle game interface *)

(** A single card in a deck/hand *)
module Card : sig
  type t =
    | Strike
    | Defend
    | Heal
    | Special of string
end

(** One player's state *)
module Player_state : sig
  type t =
    { name : string
    ; health : int
    ; max_hp : int
    ; energy : int
    ; hand : Card.t list
    ; draw_pile : Card.t list
    ; discard_pile : Card.t list
    }
end

(** Enemy or boss state *)
module Enemy_state : sig
  type t =
    { kind : string
    ; health : int
    ; intent : string  (** e.g. "attack 10", "buff", etc. *)
    }
end

(** Overall battle/game status *)
module Decision : sig
  type t =
    | In_progress of { whose_turn : [ `Player1 | `Player2 | `Enemy ] }
    | Victory
    | Defeat
end

(** Complete game state *)
module Game_state : sig
  type t =
    { player1 : Player_state.t
    ; player2 : Player_state.t
    ; enemies : Enemy_state.t list
    ; floor : int
    ; decision : Decision.t
    }
end

(** A move: which card is played and on which target *)
module Move : sig
  type target = [ `Enemy of int | `Player1 | `Player2 ]
  
  type t =
    { card : Card.t
    ; target : target
    }
end

(** An initial example state *)
val initial_state : Game_state.t
