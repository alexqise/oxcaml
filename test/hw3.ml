open! Core

(* Import the modules we're testing *)
open Tictactoe_logic_library
(* open Hw2_tictactoe_logic *)  (* Not used in this file *)
(* Use module aliases to avoid name conflicts *)
module STS = Hw2_slaythespire_logic
(* open Hw1_slaythespire *)  (* Not used in this file *)

(* Re-export STS modules for convenience in tests *)
module Card = STS.Card
module Player_state = STS.Player_state
module Enemy_state = STS.Enemy_state
module Decision = STS.Decision
module Game_state = STS.Game_state

let ok_exn result = Result.ok result |> Option.value_exn

(* Helper functions for debugging *)
let print_game_state state =
  print_s [%sexp (state : Game_state.t)]

let print_player_state state =
  print_s [%sexp (state : Player_state.t)]

(* Helper to create test games quickly *)
let create_test_game () =
  let basic_deck = [Card.Strike; Card.Defend; Card.Heal] in
  let goblin = Enemy_state.create ~kind:"Goblin" ~max_hp:20 ~intent:"attack" ~damage_intent:5 in
  match Game_state.create ~floor:1 ~player1_deck:basic_deck ~player2_deck:basic_deck ~enemies:[goblin] with
  | Ok game -> game
  | Error _ -> failwith "Failed to create test game"

(* ==================== *
 * CARD MODULE TESTS   *
 * ==================== *)

let%test "Card energy costs are correct" =
  Card.energy_cost Card.Strike = 1 &&
  Card.energy_cost Card.Defend = 1 &&  
  Card.energy_cost Card.Heal = 1 &&
  Card.energy_cost (Card.Special "Fireball") = 2

let%test "Card descriptions are accurate" =
  String.equal (Card.description Card.Strike) "Deal 6 damage" &&
  String.equal (Card.description Card.Defend) "Gain 5 block" &&
  String.equal (Card.description Card.Heal) "Restore 5 health"

let%expect_test "Card equality and special cards" =
  print_s [%sexp (Card.equal Card.Strike Card.Strike : bool)];
  [%expect {| true |}];
  
  let fireball = Card.Special "Fireball" in
  print_s [%message "Fireball details"
    ~energy_cost:(Card.energy_cost fireball : int)
    ~description:(Card.description fireball : string)];
  [%expect {| ("Fireball details" (energy_cost 2) (description "Special ability: Fireball")) |}]

(* ==================== *
 * PLAYER STATE TESTS   *
 * ==================== *)

let%test "Player_state.create initializes correctly" =
  let player = Player_state.create ~name:"Hero" ~max_hp:100 ~max_energy:5 ~starting_deck:[Card.Strike] in
  String.equal player.name "Hero" &&
  Int.equal player.health 100 &&
  Int.equal player.max_hp 100 &&
  Int.equal player.energy 5 &&
  Int.equal player.block 0 &&
  List.equal Card.equal player.hand [] &&
  List.equal Card.equal player.draw_pile [Card.Strike]

let%test "Player_state.is_alive works correctly" =
  let living = Player_state.create ~name:"Alive" ~max_hp:100 ~max_energy:3 ~starting_deck:[] in
  let dead = { (Player_state.create ~name:"Dead" ~max_hp:100 ~max_energy:3 ~starting_deck:[]) with health = 0 } in
  
  Bool.equal (Player_state.is_alive living) true &&
  Bool.equal (Player_state.is_alive dead) false

let%test "Player_state.take_damage respects block" =
  let player = Player_state.create ~name:"Defender" ~max_hp:100 ~max_energy:3 ~starting_deck:[] in
  let player_with_block = Player_state.gain_block player 10 in
  
  (* Damage less than block - should preserve health *)
  let damaged1 = Player_state.take_damage player_with_block 5 in
  let block_case1 = damaged1.health = 100 && damaged1.block = 5 in
  
  (* Damage more than block - should reduce health *)
  let damaged2 = Player_state.take_damage player_with_block 15 in
  let block_case2 = damaged2.health = 95 && damaged2.block = 0 in
  
  block_case1 && block_case2

let%test "Player_state energy spending works" =
  let player = Player_state.create ~name:"Mighty" ~max_hp:100 ~max_energy:5 ~starting_deck:[] in
  
  (* Sufficient energy *)
  let spend_ok = Player_state.spend_energy player 3 in
  let success_case = 
    match spend_ok with
    | Ok updated_player -> updated_player.energy = 2
    | Error _ -> false
  in
  
  (* Insufficient energy *)
  let spend_fail = Player_state.spend_energy player 10 in
  let fail_case =
    match spend_fail with
    | Ok _ -> false
    | Error msg -> String.equal msg "Not enough energy"
  in
  
  success_case && fail_case

let%expect_test "Player_state healing and health cap" =
  let wounded_player = { (Player_state.create ~name:"Wounded" ~max_hp:80 ~max_energy:3 ~starting_deck:[]) with health = 70 } in
  let healed_player = Player_state.heal wounded_player 20 in
  
  print_s [%message "Healing test"
    ~original_health:(wounded_player.health : int)
    ~healed_health:(healed_player.health : int)
    ~max_hp:(wounded_player.max_hp : int)];
  [%expect {| ("Healing test" (original_health 70) (healed_health 80) (max_hp 80)) |}]

(* ==================== *
 * ENEMY STATE TESTS    *
 * ==================== *)

let%test "Enemy_state.create works correctly" =
  let dragon = Enemy_state.create ~kind:"Dragon" ~max_hp:150 ~intent:"attack" ~damage_intent:20 in
  
  String.equal dragon.kind "Dragon" &&
  Int.equal dragon.health 150 &&  
  Int.equal dragon.max_hp 150 &&
  String.equal dragon.intent "attack" &&
  Int.equal dragon.damage_intent 20

let%test "Enemy_state damage and survival" =
  let goblin = Enemy_state.create ~kind:"Goblin" ~max_hp:25 ~intent:"attack" ~damage_intent:5 in
  
  (* Take damage *)
  let wounded_goblin = Enemy_state.take_damage goblin 10 in
  let damage_case = Int.equal wounded_goblin.health 15 && Bool.equal (Enemy_state.is_alive wounded_goblin) true in
  
  (* Take lethal damage *)
  let dead_goblin = Enemy_state.take_damage goblin 30 in
  let death_case = Int.equal dead_goblin.health 0 && Bool.equal (Enemy_state.is_alive dead_goblin) false in
  
  damage_case && death_case

let%test "Enemy_state.get_action works" =
  let attacker = Enemy_state.create ~kind:"Orc" ~max_hp:50 ~intent:"attack" ~damage_intent:8 in
  let defender = Enemy_state.create ~kind:"Guard" ~max_hp:40 ~intent:"defend" ~damage_intent:0 in
  
  (* Test attack action *)
  let attack_action = 
    match Enemy_state.get_action attacker with
    | `Attack 8 -> true
    | _ -> false
  in
  
  (* Test defend action *)
  let defend_action =
    match Enemy_state.get_action defender with
    | `Defend -> true
    | _ -> false
  in
  
  attack_action && defend_action

(* ==================== *
 * GAME STATE TESTS     *
 * ==================== *)

let%test "Game_state.create validates inputs" =
  let valid_deck = [Card.Strike; Card.Defend] in
  let enemies = [Enemy_state.create ~kind:"Slime" ~max_hp:30 ~intent:"attack" ~damage_intent:4] in
  
  (* Valid creation *)
  let valid_game = Game_state.create ~floor:2 ~player1_deck:valid_deck ~player2_deck:valid_deck ~enemies in
  let valid_case = 
    match valid_game with
    | Ok _ -> true
    | Error _ -> false
  in
  
  (* Invalid floor *)
  let invalid_floor = Game_state.create ~floor:0 ~player1_deck:valid_deck ~player2_deck:valid_deck ~enemies in
  let floor_error = 
    match invalid_floor with
    | Error errors when List.mem errors ~equal:Game_state.Create_error.equal Game_state.Create_error.Invalid_floor -> true
    | _ -> false
  in
  
  valid_case && floor_error

let%test "Game_state.make_move validates moves" =
  let game = create_test_game () in
  
  (* Give player1 a hand to work with *)
  let game_with_hand = { game with player1 = { game.player1 with 
      energy = 3; 
      hand = [Card.Strike; Card.Defend] 
    } } in
  
  (* Valid move *)
  let valid_move = Game_state.make_move game_with_hand { Game_state.Move.card = Card.Strike; target = `Enemy 0 } in
  let valid_result = 
    match valid_move with
    | Ok updated_game -> updated_game.player1.energy = 2  (* Energy spent *)
    | Error _ -> false
  in
  
  (* Invalid move - no energy *)
  let exhausted_game = { game_with_hand with player1 = { game_with_hand.player1 with energy = 0 } } in
  let invalid_move = Game_state.make_move exhausted_game { Game_state.Move.card = Card.Strike; target = `Enemy 0 } in
  let invalid_result =
    match invalid_move with
    | Error Game_state.Move_error.Not_enough_energy -> true
    | _ -> false
  in
  
  valid_result && invalid_result

let%expect_test "Complete move scenario" =
  let game = create_test_game () in
  let game_with_hand = { game with player1 = { game.player1 with 
      energy = 3; 
      hand = [Card.Strike] 
    } } in
  
  let move = { Game_state.Move.card = Card.Strike; target = `Enemy 0 } in
  let result = Game_state.make_move game_with_hand move in
  
  match result with
  | Ok final_game ->
      print_s [%message "Move successful"
        ~player_energy:(final_game.player1.energy : int)
        ~enemy_health:((List.nth_exn final_game.enemies 0).health : int)
        ~whose_turn:(final_game.decision : Decision.t)];
      [%expect {|
        ("Move successful"
         (player_energy 2) (enemy_health 14)
         (whose_turn (In_progress (whose_turn Player2))))
        |}]
  | Error error ->
      print_s [%sexp (error : Game_state.Move_error.t)];
      [%expect {| (Error "Move failed") |}]

(* ==================== *
 * INTEGRATION TESTS    *
 * ==================== *)

let%test "Full battle sequence" =
  let basic_deck = [Card.Strike; Card.Defend; Card.Strike] in
  let monster = Enemy_state.create ~kind:"Beast" ~max_hp:20 ~intent:"attack" ~damage_intent:6 in
  let game = match Game_state.create ~floor:1 ~player1_deck:basic_deck ~player2_deck:basic_deck ~enemies:[monster] with
  | Ok game -> game
  | Error _ -> failwith "Failed to create test game"
  in
  
  (* Setup battle *)
  let battle_game = { game with player1 = { game.player1 with 
      energy = 4; 
      hand = [Card.Strike; Card.Strike];
      health = 30
    } } in
  
  (* Execute moves - strike twice *)
  let move1 = match Game_state.make_move battle_game { Game_state.Move.card = Card.Strike; target = `Enemy 0 } with
  | Ok move1 -> move1
  | Error _ -> failwith "Failed to make move"
  in
  let move2 = match Game_state.make_move move1 { Game_state.Move.card = Card.Strike; target = `Enemy 0 } with
  | Ok move2 -> move2
  | Error _ -> failwith "Failed to make move"
  in
  
  (* Check enemy is defeated: 20 HP - 6 damage - 6 damage = 8 HP *)
  let enemy_alive = Enemy_state.is_alive (List.nth_exn move2.enemies 0) in
  
  (* Enemy should have 8 HP left, so still alive *)
  Bool.equal enemy_alive true

let%test "Enemy turn processing" =
  let deck = [Card.Strike] in
  let enemies = [
    Enemy_state.create ~kind:"Orc" ~max_hp:30 ~intent:"attack" ~damage_intent:8
  ] in
  let game = match Game_state.create ~floor:1 ~player1_deck:deck ~player2_deck:deck ~enemies:enemies with
  | Ok game -> game
  | Error _ -> failwith "Failed to create test game"
  in
  
  let wounded_game = { game with player1 = { game.player1 with health = 50 } } in
  let enemy_turn = { wounded_game with decision = Decision.In_progress { whose_turn = `Enemy } } in
  let result = Game_state.process_enemy_turn enemy_turn in
  
  (* Player should take 8 damage *)
  let health_check = result.player1.health = 42 in
  
  (* Should advance to player turn *)
  let turn_check = match result.decision with
    | Decision.In_progress { whose_turn = `Player1 } -> true
    | _ -> false
  in
  
  health_check && turn_check

(* ==================== *
 * EDGE CASE TESTS     *
 * ==================== *)

let%test "Edge cases and error handling" =
  (* Empty enemy list *)
  let no_enemies = match Game_state.create ~floor:1 
    ~player1_deck:[Card.Strike] ~player2_deck:[Card.Strike] ~enemies:[] 
    with
    | Ok game -> game
    | Error _ -> failwith "Failed to create test game"
  in
  let victory_empty = match Game_state.check_game_over no_enemies with
    | Decision.Victory -> true
    | _ -> false
  in
  
  (* Player at exactly 0 HP *)
  let near_death = { no_enemies with player1 = { no_enemies.player1 with health = 0 } } in
  let player_dead = not (Player_state.is_alive near_death.player1) in
  
  (* Zero energy scenarios *)
  let exhausted = { no_enemies with player1 = { near_death.player1 with energy = 0 } } in
  let no_moves = List.is_empty (Game_state.get_available_moves exhausted) in
  
  victory_empty && player_dead && no_moves


(* random state walk tests *)

let random_walk (initial_state : Game_state.t) ~random_seed =
  let rec walk (state : Game_state.t) =
    match Game_state.check_game_over state with 
    | Decision.Victory | Decision.Defeat -> state  (* Game over, return final state *)
    | Decision.In_progress { whose_turn } -> 
      let available_moves = Game_state.get_available_moves state in

      let valid_moves = List.filter_map available_moves ~f:(fun move ->
        Game_state.make_move state move |> Result.ok) in

      (* Special case: if no valid moves, try enemy turn *)
      match valid_moves with
      | [] -> 
          (* Process enemy turn if it's enemy turn OR if no moves available *)
          (match whose_turn with
          | `Enemy -> walk (Game_state.process_enemy_turn state)
          | _ -> state (* Game stuck - return current state *))
      | _ ->
          (* Pick a random valid move *)
          let next_state = List.random_element valid_moves |> Option.value_exn in
          walk next_state
  in

  Random.init random_seed;
  walk initial_state
;;

let%expect_test "Random card game walk - various outcomes" =
  (* Create a test game *)
  let basic_deck = [Card.Strike; Card.Defend; Card.Heal; Card.Strike] in
  let enemy = Enemy_state.create ~kind:"Orc" ~max_hp:25 ~intent:"attack" ~damage_intent:8 in
  let game = match Game_state.create ~floor:1 ~player1_deck:basic_deck ~player2_deck:basic_deck ~enemies:[enemy] with
    | Ok g -> g
    | Error _ -> failwith "Failed to create test game"
  in
  
  (* Set up battle-ready state *)
  let battle_game = { game with player1 = { game.player1 with 
      energy = 3; 
      hand = [Card.Strike; Card.Strike; Card.Defend];
      health = 40
    } } in
  
  (* Test different random seeds *)
  print_s [%sexp (random_walk battle_game ~random_seed:1 : Game_state.t)];
  [%expect {|
    ((player1
      ((name Player 1) (health 0) (max_hp 80) (energy 0)
       (max_energy 3) (block 0) (hand []) (draw_pile [ Heal Strike ])
       (discard_pile [ Defend Strike ])))
     (player2
      ((name Player 2) (health 75) (max_hp 75) (energy 3)
       (max_energy 3) (block 0) (hand [ Strike Strike Defend Heal ])
       (draw_pile []) (discard_pile [])))
     (enemies (((kind Orc) (health 5) (max_hp 25) (intent attack) (damage_intent 8))))
     (floor 1) (decision Defeat) (turn_count 4))
    |}];
  
  (* Different seed should give different outcome *)
  print_s [%sexp (random_walk battle_game ~random_seed:42 : Game_state.t)];
  [%expect {|
    ((player1
      ((name Player 1) (health 32) (max_hp 80) (energy 2)
       (max_energy 3) (block 0) (hand [ Defend ]) (draw_pile [ Heal Strike ])
       (discard_pile [ Strike ]))
     (player2
      ((name Player 2) (health 75) (max_hp 75) (energy 3)
       (max_energy 3) (block 0) (hand [ Strike Strike Defend Heal ])
       (draw_pile []) (discard_pile [])))
     (enemies (((kind Orc) (health 0) (max_hp 25) (intent attack) (damage_intent 8))))
     (floor 1) (decision Victory) (turn_count 3))
    |}]
;;

(* === STRESS TESTING WITH MULTIPLE WALKS === *)

let%test "Random walks reach terminal states consistently" =
  (* Test that random walks always reach some terminal state *)
  let deck = [Card.Strike; Card.Defend] in 
  let weak_enemy = Enemy_state.create ~kind:"Goblin" ~max_hp:10 ~intent:"attack" ~damage_intent:3 in
  let game = match Game_state.create ~floor:1 ~player1_deck:deck ~player2_deck:deck ~enemies:[weak_enemy] with
    | Ok g -> g
    | Error _ -> failwith "Failed to create test game"
  in
  
  let battle_ready = { game with player1 = { game.player1 with 
      energy = 3; 
      hand = [Card.Strike; Card.Defend];
      health = 50
    } } in
  
  (* Test multiple random seeds *)
  let seeds = [1; 42; 123; 1000; 9999] in
  
  List.for_all seeds ~f:(fun seed ->
    let final_state = random_walk battle_ready ~random_seed:seed in
    (* Game should always be over *)
    Decision.is_game_over final_state.decision
  )
;;

(* === MEASURING GAME CHARACTERISTICS === *)

let analyze_random_game (game : Game_state.t) ~random_seed =
  let final_state = random_walk game ~random_seed in
  
  (* Calculate game statistics *)
  let turns_played = final_state.turn_count in
  let player1_final_health = final_state.player1.health in
  let enemies_remaining = List.count final_state.enemies ~f:Enemy_state.is_alive in
  
  let outcome = match final_state.decision with
  | Decision.Victory -> "Victory"
  | Decision.Defeat -> "Defeat"  
  | Decision.In_progress _ -> "Still Playing"
  in
  
  (outcome, turns_played, player1_final_health, enemies_remaining)
;;

let%expect_test "Random game analysis" =
  let competitive_deck = [Card.Strike; Card.Defend; Card.Heal; Card.Special "Fireball"] in
  let boss = Enemy_state.create ~kind:"Boss" ~max_hp:40 ~intent:"attack" ~damage_intent:12 in
  let game = match Game_state.create ~floor:3 ~player1_deck:competitive_deck ~player2_deck:competitive_deck ~enemies:[boss] with
    | Ok g -> g
    | Error _ -> failwith "Failed to create test game"
  in
  
  let epic_battle = { game with player1 = { game.player1 with 
      energy = 4; 
      hand = competitive_deck;
      health = 60
    } } in
  
  let outcome, turns, health, enemies = analyze_random_game epic_battle ~random_seed:1337 in
  
  print_s [%message "Game analysis" 
    ~outcome:(outcome : string)
    ~turns_played:(turns : int)
    ~player_final_health:(health : int)
    ~enemies_alive:(enemies : int)];
  [%expect
    {|
    ("Game analysis"
     ("outcome" "Victory") ("turns_played" 5)
     ("player_final_health" 35) ("enemies_alive" 0))
    |}]
