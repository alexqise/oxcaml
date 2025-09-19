(* A single card in a deck/hand *)
module Card = struct
  type t =
    | Strike
    | Defend
    | Heal
    | Special of string
end

(* One player's state *)
module Player_state = struct
  type t =
    { name    : string
    ; health  : int
    ; max_hp  : int
    ; energy  : int
    ; hand    : Card.t list
    ; draw_pile : Card.t list
    ; discard_pile : Card.t list
    }
end

(* Enemy or boss state *)
module Enemy_state = struct
  type t =
    { kind   : string
    ; health : int
    ; intent : string  (* e.g. "attack 10", "buff", etc. *)
    }
end

(* Overall battle/game status *)
module Decision = struct
  type t =
    | In_progress of { whose_turn : [ `Player1 | `Player2 | `Enemy ] }
    | Victory
    | Defeat
end

(* Complete game state *)
module Game_state = struct
  type t =
    { player1  : Player_state.t
    ; player2  : Player_state.t
    ; enemies  : Enemy_state.t list
    ; floor    : int
    ; decision : Decision.t
    }
end

(* A move: which card is played and on which target *)
module Move = struct
  type target = [ `Enemy of int | `Player1 | `Player2 ]
  type t =
    { card   : Card.t
    ; target : target
    }
end

(* An initial example state *)
let initial_state : Game_state.t =
  { player1 =
      { name = "Player 1"
      ; health = 80
      ; max_hp = 80
      ; energy = 3
      ; hand = [ Card.Strike; Card.Defend ]
      ; draw_pile = [ Card.Strike; Card.Strike; Card.Defend; Card.Heal ]
      ; discard_pile = []
      }
  ; player2 =
      { name = "Player 2"
      ; health = 75
      ; max_hp = 75
      ; energy = 3
      ; hand = [ Card.Strike; Card.Heal ]
      ; draw_pile = [ Card.Strike; Card.Defend; Card.Heal ]
      ; discard_pile = []
      }
  ; enemies =
      [ { kind = "Slime"; health = 50; intent = "attack 8" } ]
  ; floor = 1
  ; decision = In_progress { whose_turn = `Player1 }
  }
