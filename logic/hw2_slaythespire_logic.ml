open! Core

module Card = struct
  type t =
    | Strike
    | Defend
    | Heal
    | Special of string
  [@@deriving sexp, compare, equal]

  let to_string = function
    | Strike -> "Strike"
    | Defend -> "Defend"
    | Heal -> "Heal"
    | Special s -> Printf.sprintf "Special(%s)" s

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
    ; block : int (* Temporary defense that resets each turn *)
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

  let heal t amount = { t with health = Int.min t.max_hp (t.health + amount) }
  let gain_block t amount = { t with block = t.block + amount }

  let spend_energy t cost =
    if t.energy >= cost
    then Ok { t with energy = t.energy - cost }
    else Error "Not enough energy"
  ;;

  (* Draw cards from draw pile to hand *)
  let draw_cards t num_cards =
    let rec draw acc_hand acc_draw_pile acc_discard remaining =
      if remaining <= 0 then
        { t with hand = acc_hand; draw_pile = acc_draw_pile; discard_pile = acc_discard }
      else (
        match acc_draw_pile with
        | [] ->
          (* No more cards in draw pile, try shuffling discard *)
          (match acc_discard with
          | [] -> 
            (* No cards left anywhere, stop drawing *)
            { t with hand = acc_hand; draw_pile = []; discard_pile = [] }
          | _ ->
            (* Shuffle discard into draw pile *)
            let shuffled_discard = List.permute acc_discard in
            draw acc_hand shuffled_discard [] remaining)
        | card :: rest_draw ->
          draw (card :: acc_hand) rest_draw acc_discard (remaining - 1))
    in
    draw t.hand t.draw_pile t.discard_pile num_cards
  ;;

  (* Start turn: reset energy/block and draw cards *)
  let start_turn t = 
    let t_with_reset = { t with energy = t.max_energy; block = 0 } in
    draw_cards t_with_reset 5  (* Draw 5 cards at start of turn, like Slay the Spire *)
end

module Enemy_state = struct
  (* Enemy intent variants - no polymorphic strings *)
  type intent =
    | Attack of int  (* Attack with damage amount *)
    | Defend of int  (* Gain block amount *)
    | Wait           (* Do nothing *)
  [@@deriving sexp, compare, equal]

  type t =
    { kind : string
    ; health : int
    ; max_hp : int
    ; block : int    (* Defensive block, like players *)
    ; intent : intent (* What the enemy plans to do *)
    }
  [@@deriving sexp, compare, equal]

  (* Create a new enemy with intent variant *)
  let create ~kind ~max_hp ~intent =
    { kind; health = max_hp; max_hp; block = 0; intent }
  ;;

  let is_alive t = t.health > 0
  
  (* Take damage with block protection, like players *)
  let take_damage t damage =
    let actual_damage = Int.max 0 (damage - t.block) in
    let remaining_block = Int.max 0 (t.block - damage) in
    { t with health = Int.max 0 (t.health - actual_damage); block = remaining_block }
  ;;

  (* Add block to enemy for defense *)
  let gain_block t amount = { t with block = t.block + amount }

  (* Get the action the enemy will perform - returns intent directly, no polymorphic variants *)
  let get_action t = t.intent
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
    [@@deriving sexp, compare, equal]
  end

  let create ~floor ~player1_deck ~player2_deck ~enemies
    : (t, Create_error.t list) Result.t
    =
    let errors = [] in
    let errors = if floor <= 0 then Create_error.Invalid_floor :: errors else errors in
    let errors =
      if List.is_empty player1_deck then Create_error.Empty_deck :: errors else errors
    in
    let errors =
      if List.is_empty player2_deck then Create_error.Empty_deck :: errors else errors
    in
    match errors with
    | [] ->
      let player1 =
        Player_state.create
          ~name:"Player 1"
          ~max_hp:80
          ~max_energy:3
          ~starting_deck:player1_deck
      in
      let player2 =
        Player_state.create
          ~name:"Player 2"
          ~max_hp:75
          ~max_energy:3
          ~starting_deck:player2_deck
      in
      Ok
        { player1
        ; player2
        ; enemies
        ; floor
        ; decision = In_progress { whose_turn = `Player1 }
        ; turn_count = 1
        }
    | _ -> Error errors
  ;;

  let check_game_over t =
    let players_alive =
      Player_state.is_alive t.player1 || Player_state.is_alive t.player2
    in
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
    | _ -> target_entity (* Invalid combinations do nothing *)
  ;;

  let update_target_in_game_state t target updated_entity =
    match target, updated_entity with
    | `Player1, `Player updated_player -> { t with player1 = updated_player }
    | `Player2, `Player updated_player -> { t with player2 = updated_player }
    | `Enemy index, `Enemy updated_enemy ->
      let updated_enemies =
        List.mapi t.enemies ~f:(fun i enemy -> if i = index then updated_enemy else enemy)
      in
      { t with enemies = updated_enemies }
    | _ -> t (* Mismatched target/entity type *)
  ;;

  let remove_card_from_hand hand card =
    let rec remove_first acc = function
      | [] -> None
      | hd :: tl when Card.equal hd card -> Some (List.rev acc @ tl)
      | hd :: tl -> remove_first (hd :: acc) tl
    in
    remove_first [] hand
  ;;

  (* Helper: validate game state and get current player *)
  let validate_and_get_player t : (Player_state.t, Move_error.t) Result.t =
    match t.decision with
    | Victory | Defeat -> Error Game_is_over
    | In_progress { whose_turn = `Enemy } -> Error Not_player_turn
    | In_progress _ ->
      (match get_current_player t with
       | None -> Error Not_player_turn
       | Some player -> Ok player)
  ;;

  (* Helper: validate card is in hand *)
  let validate_card_in_hand (player : Player_state.t) (card : Card.t) 
    : (unit, Move_error.t) Result.t =
    match List.find player.Player_state.hand ~f:(Card.equal card) with
    | None -> Error Card_not_in_hand
    | Some _ -> Ok ()
  ;;

  (* Helper: spend energy and remove card from hand *)
  let spend_energy_and_remove_card (player : Player_state.t) (card : Card.t)
    : (Player_state.t, Move_error.t) Result.t =
    let cost = Card.energy_cost card in
    let open Result.Let_syntax in
    (* Use bind to chain operations *)
    let%bind player_after_energy = 
      Player_state.spend_energy player cost
      |> Result.map_error ~f:(fun _ -> Move_error.Not_enough_energy)
    in
    let%bind updated_hand = 
      match remove_card_from_hand player_after_energy.Player_state.hand card with
      | None -> Error Move_error.Invalid_card
      | Some hand -> Ok hand
    in
    Ok { player_after_energy with Player_state.hand = updated_hand }
  ;;

  (* Helper: apply move to target and update game state *)
  let apply_move_to_target (t : t) (move : Move.t) (player_with_card_played : Player_state.t)
    : (t, Move_error.t) Result.t =
    match get_target t move.Move.target with
    | None -> Error Move_error.Invalid_target
    | Some target_entity ->
      (* Apply card effect to target *)
      let updated_entity = apply_card_effect move.Move.card target_entity in
      (* Update game state with new player and target *)
      let t_with_updated_player = update_current_player t player_with_card_played in
      let t_with_updated_target =
        update_target_in_game_state t_with_updated_player move.Move.target updated_entity
      in
      (* Check for game over but keep same turn *)
      let new_decision = check_game_over t_with_updated_target in
      let final_decision =
        if Decision.is_game_over new_decision
        then new_decision
        else t_with_updated_target.decision
      in
      Ok { t_with_updated_target with decision = final_decision }
  ;;

  (* Main make_move function using Result.bind to reduce nesting *)
  let make_move t (move : Move.t) : (t, Move_error.t) Result.t =
    let open Result.Let_syntax in
    (* Chain all validations and updates using bind *)
    let%bind current_player = validate_and_get_player t in
    let%bind () = validate_card_in_hand current_player move.card in
    let%bind player_with_card_played = 
      spend_energy_and_remove_card current_player move.card 
    in
    apply_move_to_target t move player_with_card_played
  ;;

  (* End the current player's turn and advance to next player/enemy *)
  let end_turn t : t =
    match t.decision with
    | Victory | Defeat -> t  (* Game over, can't end turn *)
    | In_progress { whose_turn } ->
      let new_decision: Decision.t = match whose_turn with
        | `Player1 -> In_progress { whose_turn = `Player2 }
        | `Player2 -> In_progress { whose_turn = `Enemy }
        | `Enemy -> In_progress { whose_turn = `Player1 }
      in
      (* Reset player energy/block at start of their turn *)
      let updated_t = match new_decision with
        | In_progress { whose_turn = `Player1 } -> 
          { t with player1 = Player_state.start_turn t.player1 }
        | In_progress { whose_turn = `Player2 } -> 
          { t with player2 = Player_state.start_turn t.player2 }
        | _ -> t
      in
      { updated_t with decision = new_decision }
  ;;

  let process_enemy_turn t : t =
    match t.decision with
    | In_progress { whose_turn = `Enemy } ->
      (* Process each enemy action using intent enum, not polymorphic variants *)
      let updated_player1, updated_enemies =
        List.fold t.enemies ~init:(t.player1, []) ~f:(fun (acc_player, acc_enemies) enemy ->
          if Enemy_state.is_alive enemy
          then (
            match Enemy_state.get_action enemy with
            | Enemy_state.Attack damage -> 
              (* Enemy attacks player *)
              (Player_state.take_damage acc_player damage, enemy :: acc_enemies)
            | Enemy_state.Defend block_amount -> 
              (* Enemy gains block *)
              (acc_player, Enemy_state.gain_block enemy block_amount :: acc_enemies)
            | Enemy_state.Wait -> (acc_player, enemy :: acc_enemies))
          else (acc_player, enemy :: acc_enemies))
      in
      let updated_t = { t with player1 = updated_player1; enemies = List.rev updated_enemies } in
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
        [ `Player1; `Player2 ] @ List.mapi t.enemies ~f:(fun i _ -> `Enemy i)
      in
      List.cartesian_product player.hand targets
      |> List.map ~f:(fun (card, target) -> { Move.card; target })
      |> List.filter ~f:(fun move ->
        let cost = Card.energy_cost move.card in
        player.energy >= cost)
  ;;
end
