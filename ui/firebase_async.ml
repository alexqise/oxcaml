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
        ~update_mask:["status"; "player2_joined"]
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
      (* Check if game_state field exists in the response *)
      let has_game_state = String.is_substring json_str ~substring:"\"game_state\"" in
      (* Parse JSON and convert to Game_state.t *)
      let parsed_state = Serializer.parse_firestore_response json_str in
      (* Log for debugging *)
      (match parsed_state with
       | Some _ -> 
         let _ = 
           let console = Js.Unsafe.get Js.Unsafe.global "console" in
           Js.Unsafe.meth_call console "log" [| Js.Unsafe.inject (Js.string "Successfully parsed game state from Firebase") |]
         in
         ()
       | None ->
         let _ = 
           let console = Js.Unsafe.get Js.Unsafe.global "console" in
           let json_preview = if String.length json_str > 200 then 
             String.slice json_str 0 200 ^ "..."
           else json_str in
           let error_msg = if has_game_state then
             sprintf "Failed to parse game state (game_state field exists). JSON length: %d, preview: %s" 
               (String.length json_str) json_preview
           else
             sprintf "Game state field not found in Firebase response. JSON length: %d, preview: %s" 
               (String.length json_str) json_preview
           in
           Js.Unsafe.meth_call console "warn" [| Js.Unsafe.inject (Js.string error_msg) |]
         in
         ());
      Deferred.return parsed_state
    | Error err -> 
      (* Log the error *)
      let _ = 
        let console = Js.Unsafe.get Js.Unsafe.global "console" in
        Js.Unsafe.meth_call console "error" [| Js.Unsafe.inject (Js.string (sprintf "Firebase fetch error: %s" err)) |]
      in
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
    
    (* Log the save attempt *)
    let console = Js.Unsafe.get Js.Unsafe.global "console" in
    let _ = 
      let preview = if String.length body_json > 500 then 
        String.slice body_json 0 500 ^ "..."
      else body_json in
      Js.Unsafe.meth_call console "log" 
        [| Js.Unsafe.inject (Js.string (sprintf "Player 1: Saving game state to Firebase (game_id: %s, JSON length: %d): %s" game_id (String.length body_json) preview)) |]
    in
    
    let%bind result = Firebase_http.patch_firestore 
      ~collection:"games" 
      ~document_id:game_id 
      ~body_json
      ~update_mask:["game_state"; "updated_at"]
      ()
    in
    match result with
    | Ok _ -> 
      (* Log success *)
      let _ = Js.Unsafe.meth_call console "log" 
        [| Js.Unsafe.inject (Js.string (sprintf "Player 1: Successfully saved game state to Firebase (game_id: %s)" game_id)) |]
      in
      Deferred.return (Ok ())
    | Error err -> 
      (* Log the error for debugging *)
      let _ = Js.Unsafe.meth_call console "error" 
        [| Js.Unsafe.inject (Js.string (sprintf "Player 1: Firebase save error (game_id: %s): %s" game_id err)) |]
      in
      Deferred.return (Error err)
end

