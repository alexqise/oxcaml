open! Core
open Async_kernel
open Js_of_ocaml
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
  let create_lobby_async ~user_id ~user_email () : (string * string list, string) Result.t Deferred.t =
    let open Deferred.Let_syntax in
    (* Generate a unique game ID using random number *)
    let game_id = sprintf "game_%d" (Random.int 1000000000) in
    let players = [user_email] in
    
    (* Create Firestore document with game_id, players, player_ids, and host_user_id *)
    let body_json = sprintf {|{
      "fields": {
        "game_id": {"stringValue": "%s"},
        "players": {
          "arrayValue": {
            "values": [
              {"stringValue": "%s"},
              {"stringValue": "Player 2"}
            ]
          }
        },
        "player_ids": {
          "arrayValue": {
            "values": [
              {"stringValue": "%s"},
              {"stringValue": ""}
            ]
          }
        },
        "host_user_id": {"stringValue": "%s"},
        "status": {"stringValue": "waiting"}
      }
    }|} game_id (String.escaped user_email) (String.escaped user_id) (String.escaped user_id) in
    
    let%bind result = Firebase_http.post_to_firestore 
      ~collection:"games" 
      ~document_id:game_id 
      ~body_json
    in
    match result with
    | Ok _ -> Deferred.return (Ok (game_id, players))
    | Error err -> Deferred.return (Error err)
  
  (* Join an existing game lobby *)
  let join_lobby_async ~game_id ~user_id ~user_email () : (string * string list, string) Result.t Deferred.t =
    let open Deferred.Let_syntax in
    (* First, get the current game state *)
    let%bind result = Firebase_http.get_from_firestore 
      ~collection:"games" 
      ~document_id:game_id
    in
    match result with
    | Ok json_str ->
      (* Parse the response to get existing players *)
      (* Extract player1 email from the JSON *)
      let player1_email = match Serializer.extract_string_value json_str "players" with
        | Some _ -> 
          (* Try to extract from array - for now use a simple approach *)
          "Player 1"
        | None -> "Player 1"
      in
      let players = [player1_email; user_email] in
      (* Update the game to add player 2's user_id and email *)
      let update_json = sprintf {|{
        "fields": {
          "status": {"stringValue": "in_progress"},
          "player2_joined": {"booleanValue": true},
          "players": {
            "arrayValue": {
              "values": [
                {"stringValue": "%s"},
                {"stringValue": "%s"}
              ]
            }
          },
          "player_ids": {
            "arrayValue": {
              "values": [
                {"stringValue": ""},
                {"stringValue": "%s"}
              ]
            }
          }
        }
      }|} (String.escaped player1_email) (String.escaped user_email) (String.escaped user_id) in
      let%bind update_result = Firebase_http.patch_firestore 
        ~collection:"games" 
        ~document_id:game_id 
        ~body_json:update_json
        ~update_mask:["status"; "player2_joined"; "players"; "player_ids"]
        ()
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
    | Ok json_str ->
      let parsed_state = Serializer.parse_firestore_response json_str in
      Deferred.return parsed_state
    | Error _ -> 
      Deferred.return None
  
  (* Fetch lobby status from Firebase to check if player2 has joined *)
  let fetch_lobby_status_async ~game_id () : (bool * bool, string) Result.t Deferred.t =
    let open Deferred.Let_syntax in
    let%bind result = Firebase_http.get_from_firestore 
      ~collection:"games" 
      ~document_id:game_id
    in
    match result with
    | Ok json_str ->
      (* Parse JSON to check player2_joined and status *)
      let player2_joined = Json_parser.check_player2_joined json_str in
      let status_in_progress = Json_parser.check_status_in_progress json_str in
      Deferred.return (Ok (player2_joined, status_in_progress))
    | Error err -> Deferred.return (Error err)
  
  (* Save game state to Firebase *)
  let save_game_state_async ~game_id ~game_state () : (unit, string) Result.t Deferred.t =
    let open Deferred.Let_syntax in
    let game_state_fields = Serializer.game_state_to_firestore_json game_state in
    (* Format timestamp in RFC3339 format for Firestore *)
    (* Use JavaScript Date to get ISO string format *)
    let timestamp = 
      (* Create a new Date object using JavaScript's Date constructor *)
      let date_constructor = Js.Unsafe.get Js.Unsafe.global "Date" in
      let date_obj = Js.Unsafe.new_obj date_constructor [||] in
      (* Call toISOString method *)
      Js.Unsafe.meth_call date_obj "toISOString" [||] |> Js.to_string
    in
    let body_json = sprintf {|{"fields":{"game_state":{"mapValue":{"fields":{%s}}},"updated_at":{"timestampValue":"%s"}}}|} game_state_fields timestamp in
    
    let%bind result = Firebase_http.patch_firestore 
      ~collection:"games" 
      ~document_id:game_id 
      ~body_json
      ~update_mask:["game_state"; "updated_at"]
      ()
    in
    match result with
    | Ok _ -> Deferred.return (Ok ())
    | Error err -> Deferred.return (Error err)
  
  (* Get user stats from Firestore *)
  let fetch_user_stats_async ~user_id () : (int * int, string) Result.t Deferred.t =
    let open Deferred.Let_syntax in
    let%bind result = Firebase_http.get_from_firestore 
      ~collection:"users" 
      ~document_id:user_id
    in
    match result with
    | Ok json_str ->
      let wins = match Serializer.extract_integer_value json_str "wins" with
        | Some w -> w 
        | None -> 0
      in
      let losses = match Serializer.extract_integer_value json_str "losses" with
        | Some l -> l 
        | None -> 0
      in
      Deferred.return (Ok (wins, losses))
    | Error err -> Deferred.return (Error err)
  
  (* Update user stats after game ends *)
  let update_user_stats_async ~user_id ~won () : (unit, string) Result.t Deferred.t =
    let open Deferred.Let_syntax in
    (* First get current stats *)
    let%bind current_stats = fetch_user_stats_async ~user_id () in
    let wins, losses, needs_create = match current_stats with
      | Ok (w, l) -> (w, l, false)
      | Error _ -> (0, 0, true)  (* Document doesn't exist, need to create it *)
    in
    let new_wins, new_losses = if won then (wins + 1, losses) else (wins, losses + 1) in
    if needs_create then (
      (* Create the document with initial stats *)
      let body_json = sprintf {|{
        "fields": {
          "wins": {"integerValue": "%d"},
          "losses": {"integerValue": "%d"}
        }
      }|} new_wins new_losses in
      let%bind result = Firebase_http.post_to_firestore 
        ~collection:"users" 
        ~document_id:user_id 
        ~body_json
      in
      match result with
      | Ok _ -> Deferred.return (Ok ())
      | Error err -> Deferred.return (Error err)
    ) else (
      (* Update existing document *)
      let body_json = sprintf {|{
        "fields": {
          "wins": {"integerValue": "%d"},
          "losses": {"integerValue": "%d"}
        }
      }|} new_wins new_losses in
      let%bind result = Firebase_http.patch_firestore 
        ~collection:"users" 
        ~document_id:user_id 
        ~body_json
        ~update_mask:["wins"; "losses"]
        ()
      in
      match result with
      | Ok _ -> Deferred.return (Ok ())
      | Error err -> Deferred.return (Error err)
    )
  
  (* Create user profile if it doesn't exist *)
  let create_user_profile_async ~user_id ~email () : (unit, string) Result.t Deferred.t =
    let open Deferred.Let_syntax in
    let body_json = sprintf {|{
      "fields": {
        "email": {"stringValue": "%s"},
        "wins": {"integerValue": "0"},
        "losses": {"integerValue": "0"}
      }
    }|} (String.escaped email) in
    let%bind result = Firebase_http.post_to_firestore 
      ~collection:"users" 
      ~document_id:user_id 
      ~body_json
    in
    match result with
    | Ok _ -> Deferred.return (Ok ())
    | Error err -> Deferred.return (Error err)
end

