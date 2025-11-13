open! Core
open Async_kernel
open Tictactoe_logic_library
open Hw2_slaythespire_logic
open Firebase_http
open Game_state_serializer

(* Simple JSON parsing helpers *)
module Json_parser = struct
  (* Check if player2_joined field exists and is true in Firestore JSON *)
  let check_player2_joined json_str : bool =
    (* Look for the pattern: "player2_joined" followed by "booleanValue": true *)
    let has_field = String.is_substring json_str ~substring:"\"player2_joined\"" in
    if has_field then (
      (* Find the position of player2_joined *)
      match String.substr_index json_str ~pattern:"\"player2_joined\"" with
      | None -> false
      | Some pos ->
        (* Extract substring after player2_joined to find booleanValue *)
        let after_field = String.drop_prefix json_str pos in
        String.is_substring after_field ~substring:"\"booleanValue\": true" ||
        String.is_substring after_field ~substring:"\"booleanValue\":true"
    ) else
      false
  
  (* Check if status field is "in_progress" *)
  let check_status_in_progress json_str : bool =
    (* Look for status field with value "in_progress" *)
    match String.substr_index json_str ~pattern:"\"status\"" with
    | None -> false
    | Some pos ->
      let after_field = String.drop_prefix json_str pos in
      String.is_substring after_field ~substring:"\"stringValue\": \"in_progress\"" ||
      String.is_substring after_field ~substring:"\"stringValue\":\"in_progress\""
end

(* Async wrappers for Firebase operations *)
module Firebase_async = struct
  (* Create a new game lobby *)
  let create_lobby_async () : (string * string list, string) Result.t Deferred.t =
    let open Deferred.Let_syntax in
    (* Generate a unique game ID using random number *)
    let game_id = sprintf "game_%d" (Random.int 1000000000) in
    let players = ["Player 1"] in
    
    (* Create Firestore document with game_id and players *)
    let body_json = sprintf {|{
      "fields": {
        "game_id": {"stringValue": "%s"},
        "players": {
          "arrayValue": {
            "values": [
              {"stringValue": "Player 1"},
              {"stringValue": "Player 2"}
            ]
          }
        },
        "status": {"stringValue": "waiting"}
      }
    }|} game_id in
    
    let%bind result = Firebase_http.post_to_firestore 
      ~collection:"games" 
      ~document_id:game_id 
      ~body_json
    in
    match result with
    | Ok _ -> Deferred.return (Ok (game_id, players))
    | Error err -> Deferred.return (Error err)
  
  (* Join an existing game lobby *)
  let join_lobby_async ~game_id () : (string * string list, string) Result.t Deferred.t =
    let open Deferred.Let_syntax in
    (* First, get the current game state *)
    let%bind result = Firebase_http.get_from_firestore 
      ~collection:"games" 
      ~document_id:game_id
    in
    match result with
    | Ok _json_str ->
      (* Parse the response to check if game exists and has space *)
      (* For now, assume it works if we get a response *)
      let players = ["Player 1"; "Player 2"] in
      (* Update the game to add player 2 *)
      let update_json = sprintf {|{
        "fields": {
          "status": {"stringValue": "in_progress"},
          "player2_joined": {"booleanValue": true}
        }
      }|} in
      let%bind update_result = Firebase_http.patch_firestore 
        ~collection:"games" 
        ~document_id:game_id 
        ~body_json:update_json
      in
      (match update_result with
       | Ok _ -> Deferred.return (Ok (game_id, players))
       | Error err -> Deferred.return (Error err))
    | Error err -> Deferred.return (Error err)
  
  (* Fetch game state from Firebase *)
  let fetch_game_state_async ~game_id () : Game_state.t option Deferred.t =
    let open Deferred.Let_syntax in
    let%bind result = Firebase_http.get_from_firestore 
      ~collection:"games" 
      ~document_id:game_id
    in
    match result with
    | Ok _json_str ->
      (* Parse JSON and convert to Game_state.t *)
      (* For now, return None - full implementation would parse JSON *)
      Deferred.return None
    | Error _ -> Deferred.return None
  
  (* Save game state to Firebase *)
  let save_game_state_async ~game_id ~game_state () : (unit, string) Result.t Deferred.t =
    let open Deferred.Let_syntax in
    let firestore_json = Serializer.game_state_to_firestore_json game_state in
    let body_json = sprintf {|{
      "fields": {
        "game_state": {"mapValue": {"fields": %s}},
        "updated_at": {"timestampValue": "%s"}
      }
    }|} firestore_json (Time_ns.now () |> Time_ns.to_string_utc) in
    
    let%bind result = Firebase_http.patch_firestore 
      ~collection:"games" 
      ~document_id:game_id 
      ~body_json
    in
    match result with
    | Ok _ -> Deferred.return (Ok ())
    | Error err -> Deferred.return (Error err)
end

