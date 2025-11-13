open! Core
open Tictactoe_logic_library
open Hw2_slaythespire_logic

(* Serialize and deserialize Game_state.t to/from JSON for Firebase *)
module Serializer = struct
  (* Convert Card.t to JSON string value *)
  let card_to_json_string = function
    | Card.Strike -> "Strike"
    | Card.Defend -> "Defend"
    | Card.Heal -> "Heal"
    | Card.Special s -> sprintf "Special:%s" s
  
  (* Convert Card.t from JSON string value *)
  let card_of_json_string = function
    | "Strike" -> Card.Strike
    | "Defend" -> Card.Defend
    | "Heal" -> Card.Heal
    | s when String.is_prefix s ~prefix:"Special:" ->
      Card.Special (String.drop_prefix s (String.length "Special:"))
    | _ -> failwith "Invalid card JSON"
  
  (* Convert Player_state.t to Firestore JSON format *)
  let player_state_to_firestore_json (player : Player_state.t) =
    let hand_values = List.map player.hand ~f:(fun card ->
      sprintf {|{"stringValue": "%s"}|} (card_to_json_string card))
    in
    let draw_pile_values = List.map player.draw_pile ~f:(fun card ->
      sprintf {|{"stringValue": "%s"}|} (card_to_json_string card))
    in
    let discard_pile_values = List.map player.discard_pile ~f:(fun card ->
      sprintf {|{"stringValue": "%s"}|} (card_to_json_string card))
    in
    sprintf {|
      "name": {"stringValue": "%s"},
      "health": {"integerValue": "%d"},
      "max_hp": {"integerValue": "%d"},
      "energy": {"integerValue": "%d"},
      "max_energy": {"integerValue": "%d"},
      "block": {"integerValue": "%d"},
      "hand": {"arrayValue": {"values": [%s]}},
      "draw_pile": {"arrayValue": {"values": [%s]}},
      "discard_pile": {"arrayValue": {"values": [%s]}}
    |}
      (String.escaped player.name)
      player.health
      player.max_hp
      player.energy
      player.max_energy
      player.block
      (String.concat ~sep:", " hand_values)
      (String.concat ~sep:", " draw_pile_values)
      (String.concat ~sep:", " discard_pile_values)
  
  (* Convert Enemy_state.t to Firestore JSON format *)
  let enemy_state_to_firestore_json (enemy : Enemy_state.t) =
    let intent_str = match enemy.intent with
      | Enemy_state.Attack dmg -> sprintf "Attack:%d" dmg
      | Enemy_state.Defend block -> sprintf "Defend:%d" block
      | Enemy_state.Wait -> "Wait"
    in
    sprintf {|
      "kind": {"stringValue": "%s"},
      "health": {"integerValue": "%d"},
      "max_hp": {"integerValue": "%d"},
      "block": {"integerValue": "%d"},
      "intent": {"stringValue": "%s"}
    |}
      (String.escaped enemy.kind)
      enemy.health
      enemy.max_hp
      enemy.block
      intent_str
  
  (* Convert Decision.t to Firestore JSON format *)
  let decision_to_firestore_json (decision : Decision.t) =
    let decision_str = match decision with
      | Decision.Victory -> "Victory"
      | Decision.Defeat -> "Defeat"
      | Decision.In_progress { whose_turn } ->
        (match whose_turn with
         | `Player1 -> "InProgress:Player1"
         | `Player2 -> "InProgress:Player2"
         | `Enemy -> "InProgress:Enemy")
    in
    sprintf {|"stringValue": "%s"|} decision_str
  
  (* Convert Game_state.t to Firestore document JSON *)
  let game_state_to_firestore_json (state : Game_state.t) =
    let enemies_json = List.map state.enemies ~f:enemy_state_to_firestore_json in
    let enemies_array = sprintf {|"arrayValue": {"values": [%s]}|}
      (String.concat ~sep:", " (List.map enemies_json ~f:(fun e -> sprintf "{%s}" e)))
    in
    sprintf {|{
      "fields": {
        "player1": {"mapValue": {"fields": {%s}}},
        "player2": {"mapValue": {"fields": {%s}}},
        "enemies": {%s},
        "floor": {"integerValue": "%d"},
        "decision": {%s},
        "turn_count": {"integerValue": "%d"}
      }
    }|}
      (player_state_to_firestore_json state.player1)
      (player_state_to_firestore_json state.player2)
      enemies_array
      state.floor
      (decision_to_firestore_json state.decision)
      state.turn_count
  
  (* Parse Firestore JSON response to extract game state *)
  (* This is a simplified parser - in production you'd want a proper JSON parser *)
  let parse_firestore_response _json_str =
    (* For now, we'll use a simple approach - extract the fields we need *)
    (* In a real implementation, you'd use a JSON parsing library *)
    (* This is a placeholder that shows the structure *)
    failwith "parse_firestore_response: Implement JSON parsing"
  
  (* Helper to extract string value from Firestore field *)
  let extract_string_value _field_json =
    (* Placeholder - would parse JSON to extract stringValue *)
    failwith "extract_string_value: Implement JSON parsing"
  
  (* Helper to extract integer value from Firestore field *)
  let extract_integer_value _field_json =
    (* Placeholder - would parse JSON to extract integerValue *)
    failwith "extract_integer_value: Implement JSON parsing"
  
  (* Helper to extract array value from Firestore field *)
  let extract_array_value _field_json =
    (* Placeholder - would parse JSON to extract arrayValue *)
    failwith "extract_array_value: Implement JSON parsing"
end

