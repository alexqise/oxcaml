open! Core
open Js_of_ocaml
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
    sprintf {|"name": {"stringValue": "%s"},"health": {"integerValue": "%d"},"max_hp": {"integerValue": "%d"},"energy": {"integerValue": "%d"},"max_energy": {"integerValue": "%d"},"block": {"integerValue": "%d"},"hand": {"arrayValue": {"values": [%s]}},"draw_pile": {"arrayValue": {"values": [%s]}},"discard_pile": {"arrayValue": {"values": [%s]}}|}
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
    sprintf {|"kind": {"stringValue": "%s"},"health": {"integerValue": "%d"},"max_hp": {"integerValue": "%d"},"block": {"integerValue": "%d"},"intent": {"stringValue": "%s"}|}
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
  
  (* Convert Game_state.t to Firestore fields JSON (returns just the fields content, not wrapped) *)
  let game_state_to_firestore_json (state : Game_state.t) =
    (* Each enemy needs to be wrapped in mapValue with fields *)
    let enemies_values = if List.is_empty state.enemies then
      ""
    else
      List.map state.enemies ~f:(fun enemy ->
        let enemy_fields = enemy_state_to_firestore_json enemy in
        sprintf {|{"mapValue":{"fields":{%s}}}|} enemy_fields
      ) |> String.concat ~sep:", "
    in
    let enemies_array = if List.is_empty state.enemies then
      {|"arrayValue": {"values": []}|}
    else
      sprintf {|"arrayValue": {"values": [%s]}|} enemies_values
    in
    (* Return just the fields content, not wrapped in {"fields": ...} *)
    (* Remove extra whitespace to ensure valid JSON *)
    let player1_fields = player_state_to_firestore_json state.player1 |> String.strip in
    let player2_fields = player_state_to_firestore_json state.player2 |> String.strip in
    let decision_json = decision_to_firestore_json state.decision |> String.strip in
    sprintf {|"player1": {"mapValue": {"fields": {%s}}},"player2": {"mapValue": {"fields": {%s}}},"enemies": {%s},"floor": {"integerValue": "%d"},"decision": {%s},"turn_count": {"integerValue": "%d"}|}
      player1_fields
      player2_fields
      enemies_array
      state.floor
      decision_json
      state.turn_count
  
  (* Simple JSON parsing helpers using string matching *)
  (* Extract string value from Firestore JSON: "field": {"stringValue": "value"} *)
  let extract_string_value json_str field_name : string option =
    let pattern = sprintf {|"%s"\s*:\s*\{\s*"stringValue"\s*:\s*"([^"]+)"|} field_name in
    try
      let re = Re.Pcre.regexp pattern in
      match Re.exec_opt re json_str with
      | Some groups -> Some (Re.Group.get groups 1)
      | None -> None
    with _ -> None
  
  (* Extract integer value from Firestore JSON: "field": {"integerValue": "123"} *)
  let extract_integer_value json_str field_name : int option =
    let pattern = sprintf {|"%s"\s*:\s*\{\s*"integerValue"\s*:\s*"(\d+)"|} field_name in
    try
      let re = Re.Pcre.regexp pattern in
      match Re.exec_opt re json_str with
      | Some groups -> 
        (try Some (Int.of_string (Re.Group.get groups 1)) with _ -> None)
      | None -> None
    with _ -> None
  
  (* Extract card from array value: {"stringValue": "Strike"} *)
  (* This is a simple JSON object, so we can directly extract the stringValue *)
  let extract_card_from_array_value card_json : Card.t option =
    (* Try multiple approaches to extract the stringValue *)
    (* First, try to find "stringValue" and extract the value after the colon *)
    try
      (* Find "stringValue" in the JSON *)
      match String.substr_index card_json ~pattern:"\"stringValue\"" with
      | None -> 
        (* Try alternative pattern with escaped quotes *)
        (match String.substr_index card_json ~pattern:"stringValue" with
         | None -> None
         | Some pos ->
           let after_stringvalue = String.drop_prefix card_json pos in
           (* Find the colon *)
           (match String.substr_index after_stringvalue ~pattern:":" with
            | None -> None
            | Some colon_pos ->
              let after_colon = String.drop_prefix after_stringvalue (colon_pos + 1) in
              (* Skip whitespace *)
              let after_colon = String.strip after_colon in
              (* Find the opening quote *)
              (match String.substr_index after_colon ~pattern:"\"" with
               | None -> None
               | Some quote_pos ->
                 let value_start = quote_pos + 1 in
                 (* Find the closing quote, handling escaped quotes *)
                 let rec find_closing_quote str pos =
                   if pos >= String.length str then None
                   else
                     let ch = String.get str pos in
                     match ch with
                     | '\\' when pos + 1 < String.length str -> find_closing_quote str (pos + 2)
                     | '"' -> Some pos
                     | _ -> find_closing_quote str (pos + 1)
                 in
                 (match find_closing_quote after_colon value_start with
                  | None -> None
                  | Some end_quote ->
                    let card_str = String.slice after_colon value_start end_quote in
                    (try 
                      let card = card_of_json_string card_str in
                      Some card
                    with _ -> None)))))
      | Some pos ->
        let after_stringvalue = String.drop_prefix card_json pos in
        (* Find the colon *)
        (match String.substr_index after_stringvalue ~pattern:":" with
         | None -> None
         | Some colon_pos ->
           let after_colon = String.drop_prefix after_stringvalue (colon_pos + 1) in
           (* Skip whitespace *)
           let after_colon = String.strip after_colon in
           (* Find the opening quote *)
           (match String.substr_index after_colon ~pattern:"\"" with
            | None -> None
            | Some quote_pos ->
              let value_start = quote_pos + 1 in
              (* Find the closing quote, handling escaped quotes *)
              let rec find_closing_quote str pos =
                if pos >= String.length str then None
                else
                  let ch = String.get str pos in
                  match ch with
                  | '\\' when pos + 1 < String.length str -> find_closing_quote str (pos + 2)
                  | '"' -> Some pos
                  | _ -> find_closing_quote str (pos + 1)
              in
              (match find_closing_quote after_colon value_start with
               | None -> None
               | Some end_quote ->
                 let card_str = String.slice after_colon value_start end_quote in
                 (try 
                   let card = card_of_json_string card_str in
                   Some card
                 with exn -> 
                   (* Log the failure for debugging *)
                   let _ = 
                     try
                       let console = Js.Unsafe.get Js.Unsafe.global "console" in
                       Js.Unsafe.meth_call console "warn" 
                         [| Js.Unsafe.inject (Js.string (sprintf "Failed to parse card from string '%s' (JSON: %s): %s" card_str card_json (Exn.to_string exn))) |]
                     with _ -> ()
                   in
                   None))))
    with exn -> 
      (* Log exception for debugging *)
      let _ = 
        try
          let console = Js.Unsafe.get Js.Unsafe.global "console" in
          Js.Unsafe.meth_call console "warn" 
            [| Js.Unsafe.inject (Js.string (sprintf "Exception parsing card JSON '%s': %s" card_json (Exn.to_string exn))) |]
        with _ -> ()
      in
      None
  
  (* Extract cards array from Firestore JSON *)
  let extract_cards_array json_str field_name : Card.t list =
    (* Find the arrayValue for this field using proper parsing *)
    try
      (* Find the field name *)
      let field_pattern = sprintf {|"%s"|} field_name in
      match String.substr_index json_str ~pattern:field_pattern with
      | None -> 
        (* Log that field was not found *)
        let _ = 
          try
            let console = Js.Unsafe.get Js.Unsafe.global "console" in
            Js.Unsafe.meth_call console "warn" 
              [| Js.Unsafe.inject (Js.string (sprintf "Card array field '%s' not found in JSON" field_name)) |]
          with _ -> ()
        in
        []
      | Some pos ->
        let after_field = String.drop_prefix json_str pos in
        (* Look for "arrayValue": pattern *)
        match String.substr_index after_field ~pattern:"\"arrayValue\"" with
        | None -> []
        | Some arrayvalue_pos ->
          let after_arrayvalue = String.drop_prefix after_field arrayvalue_pos in
          (* Look for "values":[ pattern *)
          match String.substr_index after_arrayvalue ~pattern:"\"values\"" with
          | None -> []
          | Some values_pos ->
            let after_values = String.drop_prefix after_arrayvalue values_pos in
            (* Find the opening bracket *)
            match String.substr_index after_values ~pattern:"[" with
            | None -> []
            | Some bracket_pos ->
              let start_pos = bracket_pos + 1 in
              (* Find matching closing bracket, accounting for nested structures *)
              let rec find_matching_bracket str pos depth =
                if pos >= String.length str then None
                else
                  let ch = String.get str pos in
                  match ch with
                  | '[' -> find_matching_bracket str (pos + 1) (depth + 1)
                  | ']' -> if depth = 1 then Some pos else find_matching_bracket str (pos + 1) (depth - 1)
                  | '{' -> 
                    (* Skip nested objects by finding matching brace *)
                    let rec find_matching_brace str pos depth =
                      if pos >= String.length str then pos
                      else
                        let ch = String.get str pos in
                        match ch with
                        | '{' -> find_matching_brace str (pos + 1) (depth + 1)
                        | '}' -> if depth = 1 then pos + 1 else find_matching_brace str (pos + 1) (depth - 1)
                        | '"' -> 
                          let rec skip_string str pos =
                            if pos >= String.length str then pos
                            else
                              let ch = String.get str pos in
                              match ch with
                              | '\\' when pos + 1 < String.length str -> skip_string str (pos + 2)
                              | '"' -> pos + 1
                              | _ -> skip_string str (pos + 1)
                          in
                          find_matching_brace str (skip_string str (pos + 1)) depth
                        | _ -> find_matching_brace str (pos + 1) depth
                    in
                    find_matching_bracket str (find_matching_brace str (pos + 1) 1) depth
                  | '"' -> 
                    (* Skip string literals *)
                    let rec skip_string str pos =
                      if pos >= String.length str then pos
                      else
                        let ch = String.get str pos in
                        match ch with
                        | '\\' when pos + 1 < String.length str -> skip_string str (pos + 2)
                        | '"' -> pos + 1
                        | _ -> skip_string str (pos + 1)
                    in
                    find_matching_bracket str (skip_string str (pos + 1)) depth
                  | _ -> find_matching_bracket str (pos + 1) depth
              in
              (match find_matching_bracket after_values start_pos 1 with
               | None -> 
                 (* Log bracket matching failure *)
                 let _ = 
                   try
                     let console = Js.Unsafe.get Js.Unsafe.global "console" in
                     Js.Unsafe.meth_call console "warn" 
                       [| Js.Unsafe.inject (Js.string (sprintf "Failed to find matching bracket for '%s' array" field_name)) |]
                   with _ -> ()
                 in
                 []
               | Some end_pos -> 
                 let array_content = String.slice after_values start_pos end_pos in
                 (* Log array content for debugging *)
                 let _ = 
                   try
                     let console = Js.Unsafe.get Js.Unsafe.global "console" in
                     let preview = if String.length array_content > 200 then 
                       String.slice array_content 0 200 ^ "..."
                     else array_content in
                     Js.Unsafe.meth_call console "log" 
                       [| Js.Unsafe.inject (Js.string (sprintf "Extracted '%s' array content (length: %d): %s" field_name (String.length array_content) preview)) |]
                   with _ -> ()
                 in
                 (* Check if array is empty *)
                 let trimmed_content = String.strip array_content in
                 if String.is_empty trimmed_content then (
                   let _ = 
                     try
                       let console = Js.Unsafe.get Js.Unsafe.global "console" in
                       Js.Unsafe.meth_call console "log" 
                         [| Js.Unsafe.inject (Js.string (sprintf "'%s' array is empty" field_name)) |]
                     with _ -> ()
                   in
                   []
                 ) else (
                   (* Parse individual card objects from the array *)
                   (* Cards are in format: {"stringValue": "Strike"},{"stringValue": "Defend"},... *)
                   (* Log the start of parsing *)
                   let _ = 
                     try
                       let console = Js.Unsafe.get Js.Unsafe.global "console" in
                       Js.Unsafe.meth_call console "log" 
                         [| Js.Unsafe.inject (Js.string (sprintf "Starting to parse cards from '%s' array, content length: %d" field_name (String.length array_content))) |]
                     with _ -> ()
                   in
                   let rec parse_cards_from_array str pos acc =
                   if pos >= String.length str then (
                     let _ = 
                       try
                         let console = Js.Unsafe.get Js.Unsafe.global "console" in
                         Js.Unsafe.meth_call console "log" 
                           [| Js.Unsafe.inject (Js.string (sprintf "Reached end of array content at position %d, parsed %d cards so far" pos (List.length acc))) |]
                       with _ -> ()
                     in
                     List.rev acc)
                   else
                     (* Skip whitespace and commas *)
                     let rec skip_whitespace_and_commas str pos =
                       if pos >= String.length str then pos
                       else
                         let ch = String.get str pos in
                         match ch with
                         | ' ' | '\t' | '\n' | '\r' | ',' -> skip_whitespace_and_commas str (pos + 1)
                         | _ -> pos
                     in
                     let pos = skip_whitespace_and_commas str pos in
                     if pos >= String.length str then List.rev acc
                     else
                       (* Find the next card object *)
                       let ch = String.get str pos in
                       match ch with
                       | '{' ->
                         (* Log what we're trying to parse *)
                         let _ = 
                           try
                             let console = Js.Unsafe.get Js.Unsafe.global "console" in
                             let snippet = if pos + 100 < String.length str then
                               String.slice str pos (pos + 100)
                             else
                               String.slice str pos (String.length str)
                             in
                             Js.Unsafe.meth_call console "log" 
                               [| Js.Unsafe.inject (Js.string (sprintf "Looking for closing brace starting at pos %d, snippet: %s" pos snippet)) |]
                           with _ -> ()
                         in
                         (* Find matching closing brace *)
                         let rec find_closing_brace str pos depth =
                           if pos >= String.length str then None
                           else
                             let ch = String.get str pos in
                             match ch with
                             | '{' -> find_closing_brace str (pos + 1) (depth + 1)
                             | '}' -> if depth = 1 then Some pos else find_closing_brace str (pos + 1) (depth - 1)
                             | '"' -> 
                               let rec skip_string str pos =
                                 if pos >= String.length str then pos
                                 else
                                   let ch = String.get str pos in
                                   match ch with
                                   | '\\' when pos + 1 < String.length str -> skip_string str (pos + 2)
                                   | '"' -> pos + 1
                                   | _ -> skip_string str (pos + 1)
                               in
                               find_closing_brace str (skip_string str (pos + 1)) depth
                             | _ -> find_closing_brace str (pos + 1) depth
                         in
                         (match find_closing_brace str (pos + 1) 1 with
                          | None -> 
                            let _ = 
                              try
                                let console = Js.Unsafe.get Js.Unsafe.global "console" in
                                let full_snippet = if pos + 200 < String.length str then
                                  String.slice str pos (pos + 200)
                                else
                                  String.slice str pos (String.length str)
                                in
                                Js.Unsafe.meth_call console "warn" 
                                  [| Js.Unsafe.inject (Js.string (sprintf "Failed to find closing brace for card object starting at position %d. Full snippet: %s" pos full_snippet)) |]
                              with _ -> ()
                            in
                            List.rev acc
                          | Some end_brace ->
                            let card_json = String.slice str pos (end_brace + 1) in
                            (* Log the card JSON being parsed for debugging *)
                            let _ = 
                              try
                                let console = Js.Unsafe.get Js.Unsafe.global "console" in
                                Js.Unsafe.meth_call console "log" 
                                  [| Js.Unsafe.inject (Js.string (sprintf "Found card object at pos %d-%d: %s" pos (end_brace + 1) card_json)) |]
                              with _ -> ()
                            in
                            let card_opt = extract_card_from_array_value card_json in
                            (* Log the result *)
                            let _ = 
                              try
                                let console = Js.Unsafe.get Js.Unsafe.global "console" in
                                match card_opt with
                                | Some card -> 
                                  Js.Unsafe.meth_call console "log" 
                                    [| Js.Unsafe.inject (Js.string (sprintf "✓ Successfully parsed card: %s" (Card.to_string card))) |]
                                | None -> 
                                  Js.Unsafe.meth_call console "warn" 
                                    [| Js.Unsafe.inject (Js.string (sprintf "✗ Failed to parse card from JSON: %s" card_json)) |]
                              with _ -> ()
                            in
                            parse_cards_from_array str (end_brace + 1) 
                              (match card_opt with Some c -> c :: acc | None -> acc))
                       | _ -> 
                         let _ = 
                           try
                             let console = Js.Unsafe.get Js.Unsafe.global "console" in
                             Js.Unsafe.meth_call console "warn" 
                               [| Js.Unsafe.inject (Js.string (sprintf "Unexpected character '%c' at position %d, expected '{'" ch pos)) |]
                           with _ -> ()
                         in
                         List.rev acc
                   in
                   let cards = parse_cards_from_array array_content 0 [] in
                   (* Log final result *)
                   let _ = 
                     try
                       let console = Js.Unsafe.get Js.Unsafe.global "console" in
                       Js.Unsafe.meth_call console "log" 
                         [| Js.Unsafe.inject (Js.string (sprintf "Parsed %d cards from '%s' array" (List.length cards) field_name)) |]
                     with _ -> ()
                   in
                   cards))
    with _ -> []
  
  (* Parse player state from Firestore JSON *)
  let parse_player_state json_str player_field : Player_state.t option =
    (* Extract player mapValue fields using proper brace matching *)
    try
      (* Find the player field *)
      let field_pattern = sprintf {|"%s"|} player_field in
      match String.substr_index json_str ~pattern:field_pattern with
      | None -> 
        (* Log that field was not found *)
        let _ = 
          try
            let console = Js.Unsafe.get Js.Unsafe.global "console" in
            Js.Unsafe.meth_call console "warn" 
              [| Js.Unsafe.inject (Js.string (sprintf "Field '%s' not found in JSON" player_field)) |]
          with _ -> ()
        in
        None
      | Some pos ->
        let after_field = String.drop_prefix json_str pos in
        (* Look for "mapValue": pattern *)
        match String.substr_index after_field ~pattern:"\"mapValue\"" with
        | None -> None
        | Some mapvalue_pos ->
          let after_mapvalue = String.drop_prefix after_field mapvalue_pos in
          (* Look for "fields":{ pattern *)
          match String.substr_index after_mapvalue ~pattern:"\"fields\"" with
          | None -> None
          | Some fields_pos ->
            let after_fields_label = String.drop_prefix after_mapvalue fields_pos in
            (* Find the opening brace *)
            match String.substr_index after_fields_label ~pattern:"{" with
            | None -> None
            | Some brace_pos ->
              let start_pos = brace_pos + 1 in
              (* Find matching closing brace, accounting for nested braces and strings *)
              let rec find_matching_brace str pos depth =
                if pos >= String.length str then None
                else
                  let ch = String.get str pos in
                  match ch with
                  | '{' -> find_matching_brace str (pos + 1) (depth + 1)
                  | '}' -> if depth = 1 then Some pos else find_matching_brace str (pos + 1) (depth - 1)
                  | '"' -> 
                    (* Skip string literals *)
                    let rec skip_string str pos =
                      if pos >= String.length str then pos
                      else
                        let ch = String.get str pos in
                        match ch with
                        | '\\' when pos + 1 < String.length str -> skip_string str (pos + 2)
                        | '"' -> pos + 1
                        | _ -> skip_string str (pos + 1)
                    in
                    find_matching_brace str (skip_string str (pos + 1)) depth
                  | _ -> find_matching_brace str (pos + 1) depth
              in
              (match find_matching_brace after_fields_label start_pos 1 with
               | None -> 
                 (* Log that brace matching failed *)
                 let _ = 
                   try
                     let console = Js.Unsafe.get Js.Unsafe.global "console" in
                     Js.Unsafe.meth_call console "warn" 
                       [| Js.Unsafe.inject (Js.string (sprintf "Failed to find matching brace for '%s' fields" player_field)) |]
                   with _ -> ()
                 in
                 None
               | Some end_pos -> 
                 let fields_json = String.slice after_fields_label start_pos end_pos in
                 (* Log fields_json for debugging *)
                 let _ = 
                   try
                     let console = Js.Unsafe.get Js.Unsafe.global "console" in
                     let preview = if String.length fields_json > 300 then 
                       String.slice fields_json 0 300 ^ "..."
                     else fields_json in
                     Js.Unsafe.meth_call console "log" 
                       [| Js.Unsafe.inject (Js.string (sprintf "Extracted fields for '%s' (length: %d): %s" player_field (String.length fields_json) preview)) |]
                   with _ -> ()
                 in
                 let name = extract_string_value fields_json "name" |> Option.value ~default:"Unknown" in
                 let health = extract_integer_value fields_json "health" |> Option.value ~default:80 in
                 let max_hp = extract_integer_value fields_json "max_hp" |> Option.value ~default:80 in
                 let energy = extract_integer_value fields_json "energy" |> Option.value ~default:3 in
                 let max_energy = extract_integer_value fields_json "max_energy" |> Option.value ~default:3 in
                 let block = extract_integer_value fields_json "block" |> Option.value ~default:0 in
                 (* Debug: Check if hand field exists *)
                 let has_hand_field = String.is_substring fields_json ~substring:"\"hand\"" in
                 let _ = 
                   try
                     let console = Js.Unsafe.get Js.Unsafe.global "console" in
                     if has_hand_field then (
                       (* Find hand field and show preview *)
                       match String.substr_index fields_json ~pattern:"\"hand\"" with
                       | None -> ()
                       | Some hand_pos ->
                         let hand_snippet = if hand_pos + 300 < String.length fields_json then
                           String.slice fields_json hand_pos (hand_pos + 300)
                         else
                           String.slice fields_json hand_pos (String.length fields_json)
                         in
                         Js.Unsafe.meth_call console "log" 
                           [| Js.Unsafe.inject (Js.string (sprintf "Hand field found for '%s': %s" player_field hand_snippet)) |]
                     ) else (
                       Js.Unsafe.meth_call console "warn" 
                         [| Js.Unsafe.inject (Js.string (sprintf "Hand field NOT found for '%s'" player_field)) |]
                     )
                   with _ -> ()
                 in
                 let hand = extract_cards_array fields_json "hand" in
                 let draw_pile = extract_cards_array fields_json "draw_pile" in
                 let discard_pile = extract_cards_array fields_json "discard_pile" in
                 (* Log parsed values for debugging *)
                 let _ = 
                   try
                     let console = Js.Unsafe.get Js.Unsafe.global "console" in
                     Js.Unsafe.meth_call console "log" 
                       [| Js.Unsafe.inject (Js.string (sprintf "Parsed '%s': name=%s, health=%d, hand_size=%d, draw_pile_size=%d" player_field name health (List.length hand) (List.length draw_pile))) |]
                   with _ -> ()
                 in
                 Some { Player_state.name; health; max_hp; energy; max_energy; block; hand; draw_pile; discard_pile })
    with _ -> None
  
  (* Parse enemy intent from string *)
  let parse_enemy_intent intent_str : Enemy_state.intent =
    if String.equal intent_str "Wait" then
      Enemy_state.Wait
    else if String.is_prefix intent_str ~prefix:"Attack:" then
      let dmg_str = String.drop_prefix intent_str (String.length "Attack:") in
      (try Enemy_state.Attack (Int.of_string dmg_str) with _ -> Enemy_state.Wait)
    else if String.is_prefix intent_str ~prefix:"Defend:" then
      let block_str = String.drop_prefix intent_str (String.length "Defend:") in
      (try Enemy_state.Defend (Int.of_string block_str) with _ -> Enemy_state.Wait)
    else
      Enemy_state.Wait  (* Default *)
  
  (* Parse a single enemy from Firestore JSON *)
  let parse_enemy_state enemy_json : Enemy_state.t option =
    try
      (* Extract enemy fields from mapValue structure *)
      match String.substr_index enemy_json ~pattern:"\"fields\"" with
      | None -> None
      | Some fields_pos ->
        let after_fields = String.drop_prefix enemy_json fields_pos in
        match String.substr_index after_fields ~pattern:"{" with
        | None -> None
        | Some brace_pos ->
          let start_pos = brace_pos + 1 in
          (* Find matching closing brace *)
          let rec find_matching_brace str pos depth =
            if pos >= String.length str then None
            else
              let ch = String.get str pos in
              match ch with
              | '{' -> find_matching_brace str (pos + 1) (depth + 1)
              | '}' -> if depth = 1 then Some pos else find_matching_brace str (pos + 1) (depth - 1)
              | '"' -> 
                let rec skip_string str pos =
                  if pos >= String.length str then pos
                  else
                    let ch = String.get str pos in
                    match ch with
                    | '\\' when pos + 1 < String.length str -> skip_string str (pos + 2)
                    | '"' -> pos + 1
                    | _ -> skip_string str (pos + 1)
                in
                find_matching_brace str (skip_string str (pos + 1)) depth
              | _ -> find_matching_brace str (pos + 1) depth
          in
          (match find_matching_brace after_fields start_pos 1 with
           | None -> None
           | Some end_pos ->
             let fields_json = String.slice after_fields start_pos end_pos in
             let kind = extract_string_value fields_json "kind" |> Option.value ~default:"Unknown" in
             let health = extract_integer_value fields_json "health" |> Option.value ~default:50 in
             let max_hp = extract_integer_value fields_json "max_hp" |> Option.value ~default:50 in
             let block = extract_integer_value fields_json "block" |> Option.value ~default:0 in
             let intent_str = extract_string_value fields_json "intent" |> Option.value ~default:"Wait" in
             let intent = parse_enemy_intent intent_str in
             Some { Enemy_state.kind; health; max_hp; block; intent })
    with _ -> None
  
  (* Parse enemies array from Firestore JSON *)
  let parse_enemies_array json_str : Enemy_state.t list =
    try
      match String.substr_index json_str ~pattern:"\"enemies\"" with
      | None -> []
      | Some pos ->
        let after_field = String.drop_prefix json_str pos in
        match String.substr_index after_field ~pattern:"\"arrayValue\"" with
        | None -> []
        | Some arrayvalue_pos ->
          let after_arrayvalue = String.drop_prefix after_field arrayvalue_pos in
          match String.substr_index after_arrayvalue ~pattern:"\"values\"" with
          | None -> []
          | Some values_pos ->
            let after_values = String.drop_prefix after_arrayvalue values_pos in
            match String.substr_index after_values ~pattern:"[" with
            | None -> []
            | Some bracket_pos ->
              let start_pos = bracket_pos + 1 in
              (* Find matching closing bracket *)
              let rec find_matching_bracket str pos depth =
                if pos >= String.length str then None
                else
                  let ch = String.get str pos in
                  match ch with
                  | '[' -> find_matching_bracket str (pos + 1) (depth + 1)
                  | ']' -> if depth = 1 then Some pos else find_matching_bracket str (pos + 1) (depth - 1)
                  | '{' -> 
                    let rec find_matching_brace str pos depth =
                      if pos >= String.length str then pos
                      else
                        let ch = String.get str pos in
                        match ch with
                        | '{' -> find_matching_brace str (pos + 1) (depth + 1)
                        | '}' -> if depth = 1 then pos + 1 else find_matching_brace str (pos + 1) (depth - 1)
                        | '"' -> 
                          let rec skip_string str pos =
                            if pos >= String.length str then pos
                            else
                              let ch = String.get str pos in
                              match ch with
                              | '\\' when pos + 1 < String.length str -> skip_string str (pos + 2)
                              | '"' -> pos + 1
                              | _ -> skip_string str (pos + 1)
                          in
                          find_matching_brace str (skip_string str (pos + 1)) depth
                        | _ -> find_matching_brace str (pos + 1) depth
                    in
                    find_matching_bracket str (find_matching_brace str (pos + 1) 1) depth
                  | '"' -> 
                    let rec skip_string str pos =
                      if pos >= String.length str then pos
                      else
                        let ch = String.get str pos in
                        match ch with
                        | '\\' when pos + 1 < String.length str -> skip_string str (pos + 2)
                        | '"' -> pos + 1
                        | _ -> skip_string str (pos + 1)
                    in
                    find_matching_bracket str (skip_string str (pos + 1)) depth
                  | _ -> find_matching_bracket str (pos + 1) depth
              in
              (match find_matching_bracket after_values start_pos 1 with
               | None -> 
                 let _ = 
                   try
                     let console = Js.Unsafe.get Js.Unsafe.global "console" in
                     Js.Unsafe.meth_call console "warn" 
                       [| Js.Unsafe.inject (Js.string "Failed to find matching bracket for enemies array") |]
                   with _ -> ()
                 in
                 []
               | Some end_pos ->
                 let array_content = String.slice after_values start_pos end_pos in
                 (* Log array content for debugging *)
                 let _ = 
                   try
                     let console = Js.Unsafe.get Js.Unsafe.global "console" in
                     let preview = if String.length array_content > 200 then 
                       String.slice array_content 0 200 ^ "..."
                     else array_content in
                     Js.Unsafe.meth_call console "log" 
                       [| Js.Unsafe.inject (Js.string (sprintf "Extracted enemies array content (length: %d): %s" (String.length array_content) preview)) |]
                   with _ -> ()
                 in
                 (* Check if array is empty *)
                 let trimmed_content = String.strip array_content in
                 if String.is_empty trimmed_content then (
                   let _ = 
                     try
                       let console = Js.Unsafe.get Js.Unsafe.global "console" in
                       Js.Unsafe.meth_call console "log" 
                         [| Js.Unsafe.inject (Js.string "Enemies array is empty") |]
                     with _ -> ()
                   in
                   []
                 ) else (
                   (* Parse individual enemy objects from the array *)
                   let _ = 
                     try
                       let console = Js.Unsafe.get Js.Unsafe.global "console" in
                       Js.Unsafe.meth_call console "log" 
                         [| Js.Unsafe.inject (Js.string (sprintf "Starting to parse enemies from array, content length: %d" (String.length array_content))) |]
                     with _ -> ()
                   in
                   let rec parse_enemies_from_array str pos acc =
                   if pos >= String.length str then List.rev acc
                   else
                     let rec skip_whitespace_and_commas str pos =
                       if pos >= String.length str then pos
                       else
                         let ch = String.get str pos in
                         match ch with
                         | ' ' | '\t' | '\n' | '\r' | ',' -> skip_whitespace_and_commas str (pos + 1)
                         | _ -> pos
                     in
                     let pos = skip_whitespace_and_commas str pos in
                     if pos >= String.length str then List.rev acc
                     else
                       match String.get str pos with
                       | '{' ->
                         let rec find_closing_brace str pos depth =
                           if pos >= String.length str then None
                           else
                             let ch = String.get str pos in
                             match ch with
                             | '{' -> find_closing_brace str (pos + 1) (depth + 1)
                             | '}' -> if depth = 1 then Some pos else find_closing_brace str (pos + 1) (depth - 1)
                             | '"' -> 
                               let rec skip_string str pos =
                                 if pos >= String.length str then pos
                                 else
                                   let ch = String.get str pos in
                                   match ch with
                                   | '\\' when pos + 1 < String.length str -> skip_string str (pos + 2)
                                   | '"' -> pos + 1
                                   | _ -> skip_string str (pos + 1)
                               in
                               find_closing_brace str (skip_string str (pos + 1)) depth
                             | _ -> find_closing_brace str (pos + 1) depth
                         in
                         (match find_closing_brace str (pos + 1) 1 with
                          | None -> 
                            let _ = 
                              try
                                let console = Js.Unsafe.get Js.Unsafe.global "console" in
                                Js.Unsafe.meth_call console "warn" 
                                  [| Js.Unsafe.inject (Js.string (sprintf "Failed to find closing brace for enemy object starting at position %d" pos)) |]
                              with _ -> ()
                            in
                            List.rev acc
                          | Some end_brace ->
                            let enemy_json = String.slice str pos (end_brace + 1) in
                            let enemy_opt = parse_enemy_state enemy_json in
                            let _ = 
                              try
                                let console = Js.Unsafe.get Js.Unsafe.global "console" in
                                match enemy_opt with
                                | Some enemy -> 
                                  Js.Unsafe.meth_call console "log" 
                                    [| Js.Unsafe.inject (Js.string (sprintf "✓ Successfully parsed enemy: %s (HP: %d/%d)" enemy.kind enemy.health enemy.max_hp)) |]
                                | None -> 
                                  Js.Unsafe.meth_call console "warn" 
                                    [| Js.Unsafe.inject (Js.string (sprintf "✗ Failed to parse enemy from JSON: %s" enemy_json)) |]
                              with _ -> ()
                            in
                            parse_enemies_from_array str (end_brace + 1) 
                              (match enemy_opt with Some e -> e :: acc | None -> acc))
                       | _ -> List.rev acc
                   in
                   let enemies = parse_enemies_from_array array_content 0 [] in
                   (* Log final result *)
                   let _ = 
                     try
                       let console = Js.Unsafe.get Js.Unsafe.global "console" in
                       Js.Unsafe.meth_call console "log" 
                         [| Js.Unsafe.inject (Js.string (sprintf "Parsed %d enemies from array" (List.length enemies))) |]
                     with _ -> ()
                   in
                   enemies))
    with _ -> []
  
  (* Parse decision from Firestore JSON *)
  let parse_decision json_str : Decision.t =
    match extract_string_value json_str "decision" with
    | Some "Victory" -> Decision.Victory
    | Some "Defeat" -> Decision.Defeat
    | Some s when String.is_prefix s ~prefix:"InProgress:Player1" -> 
      Decision.In_progress { whose_turn = `Player1 }
    | Some s when String.is_prefix s ~prefix:"InProgress:Player2" -> 
      Decision.In_progress { whose_turn = `Player2 }
    | Some s when String.is_prefix s ~prefix:"InProgress:Enemy" -> 
      Decision.In_progress { whose_turn = `Enemy }
    | _ -> Decision.In_progress { whose_turn = `Player1 } (* Default *)
  
  (* Parse game state from Firestore JSON response *)
  let parse_firestore_response json_str : Game_state.t option =
    try
      (* Firebase response structure:
         {
           "name": "...",
           "fields": {
             "game_state": {
               "mapValue": {
                 "fields": { ... actual game state fields ... }
               }
             },
             ...
           }
         }
      *)
      (* First, find the top-level "fields" object *)
      let top_level_fields_opt = 
        match String.substr_index json_str ~pattern:"\"fields\"" with
        | None -> None
        | Some pos ->
          let after_fields_label = String.drop_prefix json_str pos in
          (* Find the opening brace after "fields" *)
          match String.substr_index after_fields_label ~pattern:"{" with
          | None -> None
          | Some brace_pos ->
            let start_pos = brace_pos + 1 in
            (* Find matching closing brace for top-level fields *)
            let rec find_matching_brace str pos depth =
              if pos >= String.length str then None
              else
                let ch = String.get str pos in
                match ch with
                | '{' -> find_matching_brace str (pos + 1) (depth + 1)
                | '}' -> if depth = 1 then Some pos else find_matching_brace str (pos + 1) (depth - 1)
                | '"' -> 
                  (* Skip string literals *)
                  let rec skip_string str pos =
                    if pos >= String.length str then pos
                    else
                      let ch = String.get str pos in
                      match ch with
                      | '\\' when pos + 1 < String.length str -> skip_string str (pos + 2)
                      | '"' -> pos + 1
                      | _ -> skip_string str (pos + 1)
                  in
                  find_matching_brace str (skip_string str (pos + 1)) depth
                | _ -> find_matching_brace str (pos + 1) depth
            in
            (match find_matching_brace after_fields_label start_pos 1 with
             | None -> None
             | Some end_pos -> Some (String.slice after_fields_label start_pos end_pos))
      in
      (* Now find "game_state" inside the top-level fields *)
      let game_state_json_opt = 
        match top_level_fields_opt with
        | None -> None
        | Some top_fields ->
          match String.substr_index top_fields ~pattern:"\"game_state\"" with
          | None -> None
          | Some pos ->
            let after_game_state = String.drop_prefix top_fields pos in
            (* Look for "mapValue":{ pattern *)
            match String.substr_index after_game_state ~pattern:"\"mapValue\"" with
            | None -> None
            | Some mapvalue_pos ->
              let after_mapvalue = String.drop_prefix after_game_state mapvalue_pos in
              (* Look for "fields":{ pattern *)
              match String.substr_index after_mapvalue ~pattern:"\"fields\"" with
              | None -> None
              | Some fields_pos ->
                let after_fields_label = String.drop_prefix after_mapvalue fields_pos in
                (* Find the opening brace *)
                match String.substr_index after_fields_label ~pattern:"{" with
                | None -> None
                | Some brace_pos ->
                  let start_pos = brace_pos + 1 in
                  (* Find matching closing brace, accounting for nested braces and strings *)
                  let rec find_matching_brace str pos depth =
                    if pos >= String.length str then None
                    else
                      let ch = String.get str pos in
                      match ch with
                      | '{' -> find_matching_brace str (pos + 1) (depth + 1)
                      | '}' -> if depth = 1 then Some pos else find_matching_brace str (pos + 1) (depth - 1)
                      | '"' -> 
                        (* Skip string literals *)
                        let rec skip_string str pos =
                          if pos >= String.length str then pos
                          else
                            let ch = String.get str pos in
                            match ch with
                            | '\\' when pos + 1 < String.length str -> skip_string str (pos + 2)
                            | '"' -> pos + 1
                            | _ -> skip_string str (pos + 1)
                        in
                        find_matching_brace str (skip_string str (pos + 1)) depth
                      | _ -> find_matching_brace str (pos + 1) depth
                  in
                  (match find_matching_brace after_fields_label start_pos 1 with
                   | None -> None
                   | Some end_pos -> Some (String.slice after_fields_label start_pos end_pos))
      in
      match game_state_json_opt with
      | None -> 
        (* Log that game_state_json extraction failed *)
        let _ = 
          try
            let console = Js.Unsafe.get Js.Unsafe.global "console" in
            Js.Unsafe.meth_call console "warn" 
              [| Js.Unsafe.inject (Js.string "Failed to extract game_state JSON from Firebase response") |]
          with _ -> ()
        in
        None
      | Some json ->
        (* Log the extracted JSON for debugging *)
        let _ = 
          try
            let console = Js.Unsafe.get Js.Unsafe.global "console" in
            let json_preview = if String.length json > 500 then 
              String.slice json 0 500 ^ "..."
            else json in
            Js.Unsafe.meth_call console "log" 
              [| Js.Unsafe.inject (Js.string (sprintf "Extracted game_state JSON (length: %d): %s" (String.length json) json_preview)) |]
          with _ -> ()
        in
        (* Check if player2 field exists in JSON *)
        let has_player2_field = String.is_substring json ~substring:"\"player2\"" in
        let _ = 
          try
            let console = Js.Unsafe.get Js.Unsafe.global "console" in
            Js.Unsafe.meth_call console "log" 
              [| Js.Unsafe.inject (Js.string (sprintf "player2 field exists in JSON: %b" has_player2_field)) |]
          with _ -> ()
        in
        let player1 = parse_player_state json "player1" in
        let player2 = parse_player_state json "player2" in
        let floor = extract_integer_value json "floor" |> Option.value ~default:1 in
        let turn_count = extract_integer_value json "turn_count" |> Option.value ~default:1 in
        let decision = parse_decision json in
        let enemies = parse_enemies_array json in
        (* Log parsing results for debugging *)
        let _ = 
          try
            let console = Js.Unsafe.get Js.Unsafe.global "console" in
            let player1_info = match player1 with
              | Some p1 -> sprintf "player1=%s (hand:%d, draw:%d)" p1.name (List.length p1.hand) (List.length p1.draw_pile)
              | None -> "player1=None"
            in
            let player2_info = match player2 with
              | Some p2 -> sprintf "player2=%s (hand:%d, draw:%d)" p2.name (List.length p2.hand) (List.length p2.draw_pile)
              | None -> "player2=None"
            in
            Js.Unsafe.meth_call console "log" 
              [| Js.Unsafe.inject (Js.string (sprintf "Parsed: %s, %s, enemies=%d" player1_info player2_info (List.length enemies))) |]
          with _ -> ()
        in
        match player1, player2 with
        | Some p1, Some p2 ->
          let _ = 
            try
              let console = Js.Unsafe.get Js.Unsafe.global "console" in
              Js.Unsafe.meth_call console "log" 
                [| Js.Unsafe.inject (Js.string (sprintf "Successfully parsed both players: player1=%s (hand:%d), player2=%s (hand:%d), enemies=%d" p1.name (List.length p1.hand) p2.name (List.length p2.hand) (List.length enemies))) |]
            with _ -> ()
          in
          Some { Game_state.player1 = p1; player2 = p2; enemies; floor; decision; turn_count }
        | None, Some _ ->
          (* Log that player1 failed to parse *)
          let _ = 
            try
              let console = Js.Unsafe.get Js.Unsafe.global "console" in
              Js.Unsafe.meth_call console "warn" 
                [| Js.Unsafe.inject (Js.string "Failed to parse player1 from game state") |]
            with _ -> ()
          in
          None
        | Some _, None ->
          (* Log that player2 failed to parse with more details *)
          let _ = 
            try
              let console = Js.Unsafe.get Js.Unsafe.global "console" in
              (* Try to find where player2 should be in the JSON *)
              let player2_pos = String.substr_index json ~pattern:"\"player2\"" in
              let debug_msg = match player2_pos with
                | None -> "Failed to parse player2: field not found in JSON"
                | Some pos -> 
                  let snippet = if pos + 200 < String.length json then
                    String.slice json pos (pos + 200)
                  else
                    String.slice json pos (String.length json)
                  in
                  sprintf "Failed to parse player2: found at position %d, snippet: %s" pos snippet
              in
              Js.Unsafe.meth_call console "warn" 
                [| Js.Unsafe.inject (Js.string debug_msg) |]
            with _ -> ()
          in
          None
        | None, None -> 
          let _ = 
            try
              let console = Js.Unsafe.get Js.Unsafe.global "console" in
              Js.Unsafe.meth_call console "warn" 
                [| Js.Unsafe.inject (Js.string "Failed to parse both player1 and player2 from game state") |]
            with _ -> ()
          in
          None
    with _ -> None
end

