open! Core

module Card = struct
  type t =
    | Strike
    | Defend
    | Heal
    | Special of string
  [@@deriving sexp, to_string, compare, equal]

  let energy_cost = function
    | Strike -> 1
    | Defend -> 1
    | Heal -> 1
    | Special _ -> 2
  ;;

  let description = function
    | Strike -> "Deal 6 damage"
    | Defend -> "Gain 5 block"
    | Heal -> "Restore 5 health"
    | Special name -> Printf.sprintf "Special ability: %s" name
  ;;
end

module Player_state = struct
  type t =
    { name : string
    ; health : int
    ; max_hp : int
    ; energy : int
    ; max_energy : int
    ; block : int  (* Temporary defense that resets each turn *)
    ; hand : Card.t list
    ; draw_pile : Card.t list
    ; discard_pile : Card.t list
    }
  [@@deriving sexp, compare, equal]

  let create ~name ~max_hp ~max_energy ~starting_deck =
    { name
    ; health = max_hp
    ; max_hp
    ; energy = max_energy
    ; max_energy
    ; block = 0
    ; hand = []
    ; draw_pile = starting_deck
    ; discard_pile = []
    }
  ;;

  let is_alive t = t.health > 0

  let take_damage t damage =
    let actual_damage = Int.max 0 (damage - t.block) in
    let remaining_block = Int.max 0 (t.block - damage) in
    { t with health = Int.max 0 (t.health - actual_damage); block = remaining_block }
  ;;

  let heal t amount =
    { t with health = Int.min t.max_hp (t.health + amount) }
  ;;

  let gain_block t amount =
    { t with block = t.block + amount }
  ;;

  let spend_energy t cost =
    if t.energy >= cost
    then Ok { t with energy = t.energy - cost }
    else Error "Not enough energy"
  ;;

  let start_turn t =
    { t with energy = t.max_energy; block = 0 }
  ;;
end

module Enemy_state = struct
  type t =
    { kind : string
    ; health : int
    ; max_hp : int
    ; intent : string
    ; damage_intent : int  (* How much damage the enemy plans to deal *)
    }
  [@@deriving sexp, compare, equal]

  let create ~kind ~max_hp ~intent ~damage_intent =
    { kind; health = max_hp; max_hp; intent; damage_intent }
  ;;

  let is_alive t = t.health > 0

  let take_damage t damage =
    { t with health = Int.max 0 (t.health - damage) }
  ;;

  let get_action t =
    match t.intent with
    | "attack" -> `Attack t.damage_intent
    | "defend" -> `Defend
    | _ -> `Wait
  ;;
end

module Decision = struct
  type t =
    | In_progress of { whose_turn : [ `Player1 | `Player2 | `Enemy ] }
    | Victory
    | Defeat
  [@@deriving sexp, compare, equal]

  let is_game_over = function
    | Victory | Defeat -> true
    | In_progress _ -> false
  ;;
end

module Game_state = struct
  type t =
    { player1 : Player_state.t
    ; player2 : Player_state.t
    ; enemies : Enemy_state.t list
    ; floor : int
    ; decision : Decision.t
    ; turn_count : int
    }
  [@@deriving sexp, compare, equal]

  module Create_error = struct
    type t =
      | Invalid_floor
      | Empty_deck
      | Invalid_player_count
    [@@deriving sexp, compare]
  end

  let create ~floor ~player1_deck ~player2_deck ~enemies : (t, Create_error.t list) Result.t =
    let errors = [] in
    let errors = if floor <= 0 then Create_error.Invalid_floor :: errors else errors in
    let errors = if List.is_empty player1_deck then Create_error.Empty_deck :: errors else errors in
    let errors = if List.is_empty player2_deck then Create_error.Empty_deck :: errors else errors in
    
    match errors with
    | [] ->
      let player1 = Player_state.create ~name:"Player 1" ~max_hp:80 ~max_energy:3 ~starting_deck:player1_deck in
      let player2 = Player_state.create ~name:"Player 2" ~max_hp:75 ~max_energy:3 ~starting_deck:player2_deck in
      Ok { player1; player2; enemies; floor; decision = In_progress { whose_turn = `Player1 }; turn_count = 1 }
    | _ -> Error errors
  ;;

  let check_game_over t =
    let players_alive = Player_state.is_alive t.player1 || Player_state.is_alive t.player2 in
    let enemies_alive = List.exists t.enemies ~f:Enemy_state.is_alive in
    
    match players_alive, enemies_alive with
    | false, _ -> Decision.Defeat
    | true, false -> Decision.Victory
    | true, true -> t.decision
  ;;

  module Move_error = struct
    type t =
      | Game_is_over
      | Not_player_turn
      | Invalid_card
      | Invalid_target
      | Not_enough_energy
      | Card_not_in_hand
    [@@deriving sexp, compare]
  end

  module Move = struct
    type target = [ `Enemy of int | `Player1 | `Player2 ]
    
    type t =
      { card : Card.t
      ; target : target
      }
    [@@deriving sexp, compare, equal]
  end

  let get_current_player t =
    match t.decision with
    | In_progress { whose_turn = `Player1 } -> Some t.player1
    | In_progress { whose_turn = `Player2 } -> Some t.player2
    | _ -> None
  ;;

  let update_current_player t updated_player =
    match t.decision with
    | In_progress { whose_turn = `Player1 } -> { t with player1 = updated_player }
    | In_progress { whose_turn = `Player2 } -> { t with player2 = updated_player }
    | _ -> t
  ;;

  let get_target t = function
    | `Player1 -> Some (`Player t.player1)
    | `Player2 -> Some (`Player t.player2)
    | `Enemy index ->
      (match List.nth t.enemies index with
       | Some enemy -> Some (`Enemy enemy)
       | None -> None)
  ;;

  let apply_card_effect card target_entity =
    match card, target_entity with
    | Card.Strike, `Enemy enemy -> `Enemy (Enemy_state.take_damage enemy 6)
    | Card.Strike, `Player player -> `Player (Player_state.take_damage player 6)
    | Card.Defend, `Player player -> `Player (Player_state.gain_block player 5)
    | Card.Heal, `Player player -> `Player (Player_state.heal player 5)
    | Card.Special "Fireball", `Enemy enemy -> `Enemy (Enemy_state.take_damage enemy 12)
    | Card.Special "Shield", `Player player -> `Player (Player_state.gain_block player 10)
    | _ -> target_entity  (* Invalid combinations do nothing *)
  ;;

  let update_target_in_game_state t target updated_entity =
    match target, updated_entity with
    | `Player1, `Player updated_player -> { t with player1 = updated_player }
    | `Player2, `Player updated_player -> { t with player2 = updated_player }
    | `Enemy index, `Enemy updated_enemy ->
      let updated_enemies = List.mapi t.enemies ~f:(fun i enemy ->
        if i = index then updated_enemy else enemy) in
      { t with enemies = updated_enemies }
    | _ -> t  (* Mismatched target/entity type *)
  ;;

  let remove_card_from_hand hand card =
    let rec remove_first acc = function
      | [] -> None
      | hd :: tl when Card.equal hd card -> Some (List.rev acc @ tl)
      | hd :: tl -> remove_first (hd :: acc) tl
    in
    remove_first [] hand
  ;;

  let make_move t (move : Move.t) : (t, Move_error.t) Result.t =
    match t.decision with
    | Victory | Defeat -> Error Game_is_over
    | In_progress { whose_turn = `Enemy } -> Error Not_player_turn
    | In_progress { whose_turn } ->
      (match get_current_player t with
       | None -> Error Not_player_turn
       | Some current_player ->
         (* Check if card is in hand *)
         (match List.find current_player.hand ~f:(Card.equal move.card) with
          | None -> Error Card_not_in_hand
          | Some _ ->
            (* Check energy cost *)
            let cost = Card.energy_cost move.card in
            (match Player_state.spend_energy current_player cost with
             | Error _ -> Error Not_enough_energy
             | Ok player_after_energy ->
               (* Remove card from hand *)
               (match remove_card_from_hand player_after_energy.hand move.card with
                | None -> Error Invalid_card
                | Some updated_hand ->
                  let player_with_updated_hand = { player_after_energy with hand = updated_hand } in
                  (* Get target *)
                  (match get_target t move.target with
                   | None -> Error Invalid_target
                   | Some target_entity ->
                     (* Apply card effect *)
                     let updated_entity = apply_card_effect move.card target_entity in
                     (* Update game state with new player and target *)
                     let t_with_updated_player = update_current_player t player_with_updated_hand in
                     let t_with_updated_target = update_target_in_game_state t_with_updated_player move.target updated_entity in
                     (* Check for game over and advance turn *)
                     let new_decision = check_game_over t_with_updated_target in
                     let final_decision = 
                       if Decision.is_game_over new_decision 
                       then new_decision
                       else (
                         match whose_turn with
                         | `Player1 -> Decision.In_progress { whose_turn = `Player2 }
                         | `Player2 -> Decision.In_progress { whose_turn = `Enemy }
                         | `Enemy -> Decision.In_progress { whose_turn = `Player1 }
                       )
                     in
                     Ok { t_with_updated_target with decision = final_decision })))))
  ;;

  let process_enemy_turn t : t =
    match t.decision with
    | In_progress { whose_turn = `Enemy } ->
      (* Simple AI: each alive enemy attacks player1 *)
      let updated_player1 = 
        List.fold t.enemies ~init:t.player1 ~f:(fun acc_player enemy ->
          if Enemy_state.is_alive enemy
          then (
            match Enemy_state.get_action enemy with
            | `Attack damage -> Player_state.take_damage acc_player damage
            | _ -> acc_player
          )
          else acc_player
        )
      in
      let updated_t = { t with player1 = updated_player1 } in
      let new_decision = check_game_over updated_t in
      let final_decision = 
        if Decision.is_game_over new_decision 
        then new_decision
        else Decision.In_progress { whose_turn = `Player1 }
      in
      { updated_t with decision = final_decision; turn_count = updated_t.turn_count + 1 }
    | _ -> t
  ;;

  let get_available_moves t : Move.t list =
    match get_current_player t with
    | None -> []
    | Some player ->
      let targets = 
        [ `Player1; `Player2 ] @ 
        (List.mapi t.enemies ~f:(fun i _ -> `Enemy i))
      in
      List.cartesian_product player.hand targets
      |> List.map ~f:(fun (card, target) -> { Move.card; target })
      |> List.filter ~f:(fun move ->
        let cost = Card.energy_cost move.card in
        player.energy >= cost)
  ;;
end
