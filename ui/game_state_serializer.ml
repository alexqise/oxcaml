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
  
  (* Extract string value from Firestore JSON *)
  let extract_string_value json_str field_name : string option =
    let pattern = sprintf {|"%s"\s*:\s*\{\s*"stringValue"\s*:\s*"([^"]+)"|} field_name in
    try
      let re = Re.Pcre.regexp pattern in
      match Re.exec_opt re json_str with
      | Some groups -> Some (Re.Group.get groups 1)
      | None -> None
    with _ -> None
  
  (* Extract integer value from Firestore JSON *)
  let extract_integer_value json_str field_name : int option =
    let pattern = sprintf {|"%s"\s*:\s*\{\s*"integerValue"\s*:\s*"(\d+)"|} field_name in
    try
      let re = Re.Pcre.regexp pattern in
      match Re.exec_opt re json_str with
      | Some groups -> 
        (try Some (Int.of_string (Re.Group.get groups 1)) with _ -> None)
      | None -> None
    with _ -> None
  
  (* Extract card from array value *)
  let extract_card_from_array_value card_json : Card.t option =
    try
      match String.substr_index card_json ~pattern:"\"stringValue\"" with
      | None -> 
        (match String.substr_index card_json ~pattern:"stringValue" with
         | None -> None
         | Some pos ->
           let after_stringvalue = String.drop_prefix card_json pos in
           (match String.substr_index after_stringvalue ~pattern:":" with
            | None -> None
            | Some colon_pos ->
              let after_colon = String.drop_prefix after_stringvalue (colon_pos + 1) |> String.strip in
              (match String.substr_index after_colon ~pattern:"\"" with
               | None -> None
               | Some quote_pos ->
                 let value_start = quote_pos + 1 in
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
                    (try Some (card_of_json_string card_str) with _ -> None)))))
      | Some pos ->
        let after_stringvalue = String.drop_prefix card_json pos in
        (match String.substr_index after_stringvalue ~pattern:":" with
         | None -> None
         | Some colon_pos ->
           let after_colon = String.drop_prefix after_stringvalue (colon_pos + 1) |> String.strip in
           (match String.substr_index after_colon ~pattern:"\"" with
            | None -> None
            | Some quote_pos ->
              let value_start = quote_pos + 1 in
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
                 (try Some (card_of_json_string card_str) with _ -> None))))
    with _ -> None
  
  (* Extract cards array from Firestore JSON *)
  let extract_cards_array json_str field_name : Card.t list =
    try
      let field_pattern = sprintf {|"%s"|} field_name in
      match String.substr_index json_str ~pattern:field_pattern with
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
               | None -> []
               | Some end_pos -> 
                 let array_content = String.slice after_values start_pos end_pos in
                 let trimmed_content = String.strip array_content in
                 if String.is_empty trimmed_content then []
                 else
                   let rec parse_cards_from_array str pos acc =
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
                         let ch = String.get str pos in
                         match ch with
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
                            | None -> List.rev acc
                            | Some end_brace ->
                              let card_json = String.slice str pos (end_brace + 1) in
                              let card_opt = extract_card_from_array_value card_json in
                              parse_cards_from_array str (end_brace + 1) 
                                (match card_opt with Some c -> c :: acc | None -> acc))
                         | _ -> List.rev acc
                   in
                   parse_cards_from_array array_content 0 [])
    with _ -> []
  
  (* Parse player state from Firestore JSON *)
  let parse_player_state json_str player_field : Player_state.t option =
    try
      let field_pattern = sprintf {|"%s"|} player_field in
      match String.substr_index json_str ~pattern:field_pattern with
      | None -> None
      | Some pos ->
        let after_field = String.drop_prefix json_str pos in
        match String.substr_index after_field ~pattern:"\"mapValue\"" with
        | None -> None
        | Some mapvalue_pos ->
          let after_mapvalue = String.drop_prefix after_field mapvalue_pos in
          match String.substr_index after_mapvalue ~pattern:"\"fields\"" with
          | None -> None
          | Some fields_pos ->
            let after_fields_label = String.drop_prefix after_mapvalue fields_pos in
            match String.substr_index after_fields_label ~pattern:"{" with
            | None -> None
            | Some brace_pos ->
              let start_pos = brace_pos + 1 in
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
              (match find_matching_brace after_fields_label start_pos 1 with
               | None -> None
               | Some end_pos -> 
                 let fields_json = String.slice after_fields_label start_pos end_pos in
                 let name = extract_string_value fields_json "name" |> Option.value_exn in
                 let health = extract_integer_value fields_json "health" |> Option.value_exn in
                 let max_hp = extract_integer_value fields_json "max_hp" |> Option.value_exn in
                 let energy = extract_integer_value fields_json "energy" |> Option.value_exn in
                 let max_energy = extract_integer_value fields_json "max_energy" |> Option.value_exn in
                 let block = extract_integer_value fields_json "block" |> Option.value_exn in
                 let hand = extract_cards_array fields_json "hand" in
                 let draw_pile = extract_cards_array fields_json "draw_pile" in
                 let discard_pile = extract_cards_array fields_json "discard_pile" in
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
      Enemy_state.Wait
  
  (* Parse a single enemy from Firestore JSON *)
  let parse_enemy_state enemy_json : Enemy_state.t option =
    try
      match String.substr_index enemy_json ~pattern:"\"fields\"" with
      | None -> None
      | Some fields_pos ->
        let after_fields = String.drop_prefix enemy_json fields_pos in
        match String.substr_index after_fields ~pattern:"{" with
        | None -> None
        | Some brace_pos ->
          let start_pos = brace_pos + 1 in
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
             let kind = extract_string_value fields_json "kind" |> Option.value_exn in
             let health = extract_integer_value fields_json "health" |> Option.value_exn in
             let max_hp = extract_integer_value fields_json "max_hp" |> Option.value_exn in
             let block = extract_integer_value fields_json "block" |> Option.value_exn in
             let intent_str = extract_string_value fields_json "intent" |> Option.value_exn in
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
               | None -> []
               | Some end_pos ->
                 let array_content = String.slice after_values start_pos end_pos in
                 let trimmed_content = String.strip array_content in
                 if String.is_empty trimmed_content then []
                 else
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
                            | None -> List.rev acc
                            | Some end_brace ->
                              let enemy_json = String.slice str pos (end_brace + 1) in
                              let enemy_opt = parse_enemy_state enemy_json in
                              parse_enemies_from_array str (end_brace + 1) 
                                (match enemy_opt with Some e -> e :: acc | None -> acc))
                         | _ -> List.rev acc
                   in
                   parse_enemies_from_array array_content 0 [])
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
    | _ -> Decision.In_progress { whose_turn = `Player1 }
  
  (* Parse game state from Firestore JSON response *)
  let parse_firestore_response json_str : Game_state.t option =
    try
      let top_level_fields_opt = 
        match String.substr_index json_str ~pattern:"\"fields\"" with
        | None -> None
        | Some pos ->
          let after_fields_label = String.drop_prefix json_str pos in
          match String.substr_index after_fields_label ~pattern:"{" with
          | None -> None
          | Some brace_pos ->
            let start_pos = brace_pos + 1 in
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
            (match find_matching_brace after_fields_label start_pos 1 with
             | None -> None
             | Some end_pos -> Some (String.slice after_fields_label start_pos end_pos))
      in
      let game_state_json_opt = 
        match top_level_fields_opt with
        | None -> None
        | Some top_fields ->
          match String.substr_index top_fields ~pattern:"\"game_state\"" with
          | None -> None
          | Some pos ->
            let after_game_state = String.drop_prefix top_fields pos in
            match String.substr_index after_game_state ~pattern:"\"mapValue\"" with
            | None -> None
            | Some mapvalue_pos ->
              let after_mapvalue = String.drop_prefix after_game_state mapvalue_pos in
              match String.substr_index after_mapvalue ~pattern:"\"fields\"" with
              | None -> None
              | Some fields_pos ->
                let after_fields_label = String.drop_prefix after_mapvalue fields_pos in
                match String.substr_index after_fields_label ~pattern:"{" with
                | None -> None
                | Some brace_pos ->
                  let start_pos = brace_pos + 1 in
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
                  (match find_matching_brace after_fields_label start_pos 1 with
                   | None -> None
                   | Some end_pos -> Some (String.slice after_fields_label start_pos end_pos))
      in
      match game_state_json_opt with
      | None -> None
      | Some json ->
        let player1 = parse_player_state json "player1" in
        let player2 = parse_player_state json "player2" in
        let floor = extract_integer_value json "floor" |> Option.value_exn in
        let turn_count = extract_integer_value json "turn_count" |> Option.value_exn in
        let decision = parse_decision json in
        let enemies = parse_enemies_array json in
        match player1, player2 with
        | Some p1, Some p2 ->
          Some { Game_state.player1 = p1; player2 = p2; enemies; floor; decision; turn_count }
        | _ -> None
    with _ -> None
end

