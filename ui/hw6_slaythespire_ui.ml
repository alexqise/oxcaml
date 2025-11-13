open! Core
open Tictactoe_logic_library
open Hw2_slaythespire_logic
open Virtual_dom
open! Bonsai.Let_syntax
open Firebase_effects

(* define the helper methods here *)

let render_health_bar ~current ~max_health = 
  let percentage = Float.of_int current /. Float.of_int max_health *. 100.0 in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "health-bar" ]
    [ Vdom.Node.div
        ~attrs:
        [ Vdom.Attr.class_ "health-fill";
                 Vdom.Attr.style (Css_gen.width (`Percent (Percent.of_percentage percentage))) ]
        []
    ]
;;

(* Render energy bar with percentage fill *)
let render_energy_bar ~current ~max_energy = 
  let percentage = Float.of_int current /. Float.of_int max_energy *. 100.0 in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "energy-bar" ]
    [ Vdom.Node.div
        ~attrs:
          [ Vdom.Attr.class_ "energy-fill"
          ; Vdom.Attr.style (Css_gen.width (`Percent (Percent.of_percentage percentage)))
          ]
        []
    ]
;;

(* Render player stats section *)
let render_player_stats (player : Player_state.t) =
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "player-stats" ]
    [ (* Health stat *)
      Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "stat" ]
        [ Vdom.Node.div
            ~attrs:[ Vdom.Attr.class_ "stat-label" ]
            [ Vdom.Node.text "Health" ]
        ; Vdom.Node.div
            ~attrs:[ Vdom.Attr.class_ "stat-value health" ]
            [ Vdom.Node.text (sprintf "%d/%d" player.health player.max_hp) ]
        ]
    ; (* Energy stat *)
      Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "stat" ]
        [ Vdom.Node.div
            ~attrs:[ Vdom.Attr.class_ "stat-label" ]
            [ Vdom.Node.text "Energy" ]
        ; Vdom.Node.div
            ~attrs:[ Vdom.Attr.class_ "stat-value energy" ]
            [ Vdom.Node.text (sprintf "%d/%d" player.energy player.max_energy) ]
        ]
    ; (* Block stat *)
      Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "stat" ]
        [ Vdom.Node.div
            ~attrs:[ Vdom.Attr.class_ "stat-label" ]
            [ Vdom.Node.text "Block" ]
        ; Vdom.Node.div
            ~attrs:[ Vdom.Attr.class_ "stat-value block" ]
            [ Vdom.Node.text (Int.to_string player.block) ]
        ]
    ]
;;

(* Render a single player card *)
let render_player_card (player : Player_state.t) ~image_src =
  let is_dead = not (Player_state.is_alive player) in
  let card_classes = 
    if is_dead then ["player-card"; "dead-entity"] else ["player-card"]
  in
  let image_node = 
    match image_src with
    | Some src -> 
      [ Vdom.Node.create 
          "img" 
          ~attrs:[ Vdom.Attr.class_ "char-img"
                 ; Vdom.Attr.create "src" src
                 ; Vdom.Attr.create "alt" player.name
                 ] 
          [] 
      ]
    | None -> []
  in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ (String.concat ~sep:" " card_classes) ]
    ([ Vdom.Node.div
         ~attrs:[ Vdom.Attr.class_ "player-name" ]
         [ Vdom.Node.text player.name ]
     ]
     @ image_node
     @ [ render_player_stats player
       ; render_health_bar ~current:player.health ~max_health:player.max_hp
       ; render_energy_bar ~current:player.energy ~max_energy:player.max_energy
       ])
;;

(* Convert enemy intent to display string *)
let intent_to_string = function
  | Enemy_state.Attack dmg -> sprintf "Attack %d" dmg
  | Enemy_state.Defend block -> sprintf "Defend %d" block
  | Enemy_state.Wait -> "Wait"
;;

(* Render enemy stats *)
let render_enemy_stats (enemy : Enemy_state.t) =
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "player-stats" ]
    [ (* Health stat *)
      Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "stat" ]
        [ Vdom.Node.div
            ~attrs:[ Vdom.Attr.class_ "stat-label" ]
            [ Vdom.Node.text "Health" ]
        ; Vdom.Node.div
            ~attrs:[ Vdom.Attr.class_ "stat-value health" ]
            [ Vdom.Node.text (sprintf "%d/%d" enemy.health enemy.max_hp) ]
        ]
    ; (* Block stat *)
      Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "stat" ]
        [ Vdom.Node.div
            ~attrs:[ Vdom.Attr.class_ "stat-label" ]
            [ Vdom.Node.text "Block" ]
        ; Vdom.Node.div
            ~attrs:[ Vdom.Attr.class_ "stat-value block" ]
            [ Vdom.Node.text (Int.to_string enemy.block) ]
        ]
    ]
;;

(* Render a single enemy card *)
let render_enemy_card (enemy : Enemy_state.t) ~index:_ =
  let is_dead = not (Enemy_state.is_alive enemy) in
  let card_classes = 
    if is_dead then ["enemy-card"; "dead-entity"] else ["enemy-card"]
  in
  (* Choose image based on enemy kind *)
  let image_src = 
    match String.lowercase enemy.kind with
    | "orc" -> "assets/orc.png"
    | "dragon" -> "assets/dragon.png"
    | _ -> "assets/orc.png"  (* Default fallback *)
  in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ (String.concat ~sep:" " card_classes) ]
    [ Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "enemy-name" ]
        [ Vdom.Node.text enemy.kind ]
    ; Vdom.Node.create 
        "img" 
        ~attrs:[ Vdom.Attr.class_ "char-img"
               ; Vdom.Attr.create "src" image_src
               ; Vdom.Attr.create "alt" enemy.kind
               ] 
        []
    ; render_enemy_stats enemy
    ; render_health_bar ~current:enemy.health ~max_health:enemy.max_hp
    ; Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "enemy-intent" ]
        [ Vdom.Node.text (sprintf "Intent: %s" (intent_to_string enemy.intent)) ]
    ]
;;

(* need method to take in a game state and return a virtual dom tree that's updated with the game state *)

(* Get card image source based on card type *)
let card_image_src = function
  | Card.Strike -> "assets/Ironclad Strike Card.png"
  | Card.Defend -> "assets/Defend Card.png"
  | Card.Heal -> "assets/Heal Card.png"
  | Card.Special _ -> ""  (* Will render as text *)
;;

(* Render a single card - with selection support *)
let render_card 
    (card : Card.t) 
    ~selected 
    ~on_select 
    ~can_afford =
  let card_attrs = 
    [ Vdom.Attr.class_ "card" ]
    @ (if selected then [ Vdom.Attr.style (Css_gen.border ~width:(`Px 3) ~color:(`Name "yellow") ~style:`Solid ()) ] else [])
    @ (if can_afford then [ Vdom.Attr.on_click (fun _ -> on_select ()) ] else [])
    @ (if not can_afford then [ Vdom.Attr.style (Css_gen.opacity 0.5) ] else [])
  in
  
  (* Check if card has an image *)
  let has_image = match card with
    | Card.Strike | Card.Defend | Card.Heal -> true
    | Card.Special _ -> false
  in
  
  let content = 
    if has_image 
    then [ Vdom.Node.create 
             "img" 
             ~attrs:[ Vdom.Attr.create "src" (card_image_src card)
                    ; Vdom.Attr.create "alt" (Card.to_string card)
                    ] 
             [] 
         ]
    else 
      (* Render special cards with text *)
      [ Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "card-cost" ]
          [ Vdom.Node.text (Int.to_string (Card.energy_cost card)) ]
      ; Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "card-name" ]
          [ Vdom.Node.text (Card.to_string card) ]
      ; Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "card-description" ]
          [ Vdom.Node.text (Card.description card) ]
      ]
  in
  Vdom.Node.div ~attrs:card_attrs content
;;

(* Render the header with game info *)
let render_header () =
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "game-header" ]
    [ Vdom.Node.create "h1"
        ~attrs:[ Vdom.Attr.class_ "game-title" ]
        [ Vdom.Node.text "Slay the Spire" ]
    ]
;;

(* Lobby screen state *)
type lobby_screen =
  | Main_menu
  | Creating_lobby
  | Joining_lobby
  | In_lobby of { game_id : string; players : string list; is_host : bool }
  | In_game of { game_id : string; player_role : [`Player1 | `Player2] }
[@@deriving sexp, equal]

(* Render lobby UI *)
let render_lobby_ui 
    ~lobby_screen 
    ~game_id_input 
    ~set_game_id_input 
    ~on_create_lobby 
    ~on_join_lobby =
  match lobby_screen with
  | Main_menu ->
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "lobby-container" ]
      [ Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "lobby-menu" ]
          [ Vdom.Node.create "h2"
              ~attrs:[ Vdom.Attr.class_ "lobby-title" ]
              [ Vdom.Node.text "Multiplayer Lobby" ]
          ; Vdom.Node.create "button"
              ~attrs:
                [ Vdom.Attr.class_ "lobby-btn"
                ; Vdom.Attr.on_click (fun _ -> on_create_lobby ())
                ]
              [ Vdom.Node.text "Create Game" ]
          ; Vdom.Node.div
              ~attrs:[ Vdom.Attr.class_ "join-section" ]
              [ Vdom.Node.create "input"
                  ~attrs:
                    [ Vdom.Attr.type_ "text"
                    ; Vdom.Attr.placeholder "Enter Game ID"
                    ; Vdom.Attr.value game_id_input
                    ; Vdom.Attr.on_input (fun _ text -> set_game_id_input text)
                    ]
                  []
              ; Vdom.Node.create "button"
                  ~attrs:
                    [ Vdom.Attr.class_ "lobby-btn"
                    ; Vdom.Attr.on_click (fun _ -> 
                        if String.is_empty game_id_input 
                        then Ui_effect.Ignore
                        else on_join_lobby game_id_input)
                    ]
                  [ Vdom.Node.text "Join Game" ]
              ]
          ]
      ]
  | Creating_lobby ->
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "lobby-container" ]
      [ Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "lobby-status" ]
          [ Vdom.Node.text "Creating lobby..." ]
      ]
  | Joining_lobby ->
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "lobby-container" ]
      [ Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "lobby-status" ]
          [ Vdom.Node.text "Joining game..." ]
      ]
  | In_lobby { game_id; players; is_host } ->
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "lobby-container" ]
      [ Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "lobby-info" ]
          [ Vdom.Node.create "h2"
              ~attrs:[ Vdom.Attr.class_ "lobby-title" ]
              [ Vdom.Node.text (sprintf "Game ID: %s" game_id) ]
          ; Vdom.Node.div
              ~attrs:[ Vdom.Attr.class_ "players-list" ]
              (List.map players ~f:(fun player ->
                 Vdom.Node.div
                   ~attrs:[ Vdom.Attr.class_ "player-item" ]
                   [ Vdom.Node.text player ]))
          ; (if is_host then
              Vdom.Node.div
                ~attrs:[ Vdom.Attr.class_ "lobby-status" ]
                [ Vdom.Node.text "Waiting for Player 2..." ]
            else
              Vdom.Node.div
                ~attrs:[ Vdom.Attr.class_ "lobby-status" ]
                [ Vdom.Node.text "Waiting for game to start..." ])
          ]
      ]
  | In_game { game_id; player_role } ->
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "game-info-bar" ]
      [ Vdom.Node.text (sprintf "Game: %s | You are: %s" 
          game_id 
          (match player_role with `Player1 -> "Player 1" | `Player2 -> "Player 2")) ]
;;

(* Render game status message *)
let render_game_status (decision : Decision.t) =
  let status_class, message = match decision with
    | Decision.Victory -> "status-victory", "Victory! All enemies defeated!"
    | Decision.Defeat -> "status-defeat", "Defeat! All players fallen!"
    | Decision.In_progress { whose_turn } ->
      let turn_msg = match whose_turn with
        | `Player1 -> "Player 1's Turn"
        | `Player2 -> "Player 2's Turn"
        | `Enemy -> "Enemy's Turn"
      in
      "status-in-progress", turn_msg
  in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "game-status"; Vdom.Attr.class_ status_class ]
    [ Vdom.Node.text message ]
;;

(* Render the main battle area *)
let render_battle_area 
    (game_state : Game_state.t) 
    ~selected_card_index 
    ~on_target_click =
  (* Get the current player and their selected card *)
  let current_player = 
    match game_state.decision with
    | In_progress { whose_turn = `Player1 } -> game_state.player1
    | In_progress { whose_turn = `Player2 } -> game_state.player2
    | _ -> game_state.player1  (* fallback *)
  in
  let selected_card = 
    match selected_card_index with
    | None -> None
    | Some card_index -> List.nth current_player.hand card_index
  in

  (* Player area - show all players, but only alive ones are clickable *)
  let player_area =
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "player-area" ]
      (List.map 
         [ (game_state.player1, "assets/ironclad.png", `Player1)
         ; (game_state.player2, "assets/player2.png", `Player2)
         ] 
         ~f:(fun (player, image_src, target) ->
           let player_card = render_player_card player ~image_src:(Some image_src) in
           if Player_state.is_alive player && (match selected_card with
             | Some card when (match card with Card.Defend | Card.Heal -> true | _ -> false) -> true
             | _ -> false)
           then Vdom.Node.div
             ~attrs:[ Vdom.Attr.on_click (fun _ -> on_target_click target) ]
             [ player_card ]
           else player_card  (* Dead players or no valid card selected - no click handler *)
         ))
  in
  
  (* Enemy area - show all enemies, but only alive ones are clickable *)
  let enemy_area =
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "enemy-area" ]
      (List.mapi game_state.enemies ~f:(fun index enemy ->
         let enemy_card = render_enemy_card enemy ~index in
         if Enemy_state.is_alive enemy && (match selected_card with
           | Some card when (match card with
               | Card.Strike | Card.Special _ -> true
               | Card.Defend | Card.Heal -> false) -> true
           | _ -> false)
         then Vdom.Node.div
           ~attrs:[ Vdom.Attr.on_click (fun _ -> on_target_click (`Enemy index)) ]
           [ enemy_card ]
         else enemy_card  (* Dead enemies or no valid card selected - no click handler *)
       ))
  in
  
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "battle-area" ]
    [ player_area; enemy_area ]
;;

(* Render the player's hand *)
let render_hand 
    (game_state : Game_state.t) 
    ~selected_card_index 
    ~on_card_select
    ~on_end_turn =
  let current_player_opt = 
    match game_state.decision with
    | In_progress { whose_turn = `Player1 } -> Some game_state.player1
    | In_progress { whose_turn = `Player2 } -> Some game_state.player2
    | _ -> None
  in

  match current_player_opt with
  | None -> Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "cards-area" ] []
  | Some player ->
    let hand_title = sprintf "%s's Hand" player.name in
    
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "cards-area" ]
      [ Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "hand-container" ]
          [ Vdom.Node.div
              ~attrs:[ Vdom.Attr.class_ "cards-title" ]
              [ Vdom.Node.text hand_title ]
          ; Vdom.Node.create "button"
              ~attrs:
                [ Vdom.Attr.class_ "end-turn-btn"
                ; Vdom.Attr.on_click (fun _ -> on_end_turn ())
                ]
              [ Vdom.Node.text "End Turn" ]
          ]
      ; Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "hand" ]
          (List.mapi player.hand ~f:(fun index card ->
             let cost = Card.energy_cost card in
             let can_afford = player.energy >= cost in
             let is_selected = 
               match selected_card_index with
               | Some sel_index -> index = sel_index
               | None -> false
             in
             render_card 
               card 
               ~selected:is_selected 
               ~on_select:(fun () -> on_card_select index) 
               ~can_afford
           ))
      ]
;;

(* Main Bonsai app *)
let app =
  (* Create initial game state *)
  let initial_state =
    let player1_deck = 
      [ Card.Strike; Card.Strike; Card.Defend; Card.Heal; Card.Strike ]
    in
    let player2_deck = 
      [ Card.Strike; Card.Defend; Card.Heal; Card.Strike; Card.Defend ]
    in
    let enemies = 
      [ Enemy_state.create ~kind:"Orc" ~max_hp:50 ~intent:(Attack 8)
      ; Enemy_state.create ~kind:"Dragon" ~max_hp:120 ~intent:(Attack 12)
      ]
    in
    let game_state = 
      Game_state.create 
        ~floor:1 
        ~player1_deck 
        ~player2_deck 
        ~enemies
      |> Result.ok
      |> Option.value_exn
    in
    let player1_with_turn = Player_state.start_turn game_state.player1 in
    { game_state with player1 = player1_with_turn }
  in
  
  (* Lobby state *)
  let%sub lobby_screen, set_lobby_screen =
    Bonsai.state ~default_model:Main_menu (module struct
      type t = lobby_screen [@@deriving sexp, equal]
    end)
  in
  
  let%sub game_id_input, set_game_id_input =
    Bonsai.state ~default_model:"" (module String)
  in
  
  let%sub current_game_id, set_current_game_id =
    Bonsai.state ~default_model:None (module struct
      type t = string option [@@deriving sexp, equal]
    end)
  in
  
  let%sub player_role, set_player_role =
    Bonsai.state ~default_model:None (module struct
      type t = [`Player1 | `Player2] option [@@deriving sexp, equal]
    end)
  in
  
  (* Bonsai state: game state + selected card *)
  let%sub game_state, set_game_state =
    Bonsai.state ~default_model:initial_state (module Game_state)
  in
  
  let%sub selected_card_index, set_selected_card_index =
    Bonsai.state ~default_model:None (module struct
      type t = int option [@@deriving sexp, equal]
    end)
  in
  
  (* Create lobby effect handler *)
  let%sub create_lobby_effect =
    let%arr set_lobby_screen = set_lobby_screen
    and set_current_game_id = set_current_game_id
    and set_player_role = set_player_role in
    let open Ui_effect.Let_syntax in
    fun () ->
      (* Step 1: Show loading state immediately *)
      let%bind () = set_lobby_screen Creating_lobby in
      (* Step 2: Wait for async Firebase call *)
      let%bind result = Firebase_effects.create_lobby_effect () in
      (* Step 3: Update UI based on result *)
      match result with
      | Ok (game_id, players) ->
        Ui_effect.Many [
          set_current_game_id (Some game_id);
          set_player_role (Some `Player1);
          set_lobby_screen (In_lobby { game_id; players; is_host = true })
        ]
      | Error _err ->
        Ui_effect.Many [
          set_lobby_screen Main_menu;
          (* In a real app, you'd show error message *)
        ]
  in
  
  (* Join lobby effect handler *)
  let%sub join_lobby_effect =
    let%arr set_lobby_screen = set_lobby_screen
    and set_current_game_id = set_current_game_id
    and set_player_role = set_player_role in
    let open Ui_effect.Let_syntax in
    fun game_id ->
      (* Step 1: Show loading state immediately *)
      let%bind () = set_lobby_screen Joining_lobby in
      (* Step 2: Wait for async Firebase call *)
      let%bind result = Firebase_effects.join_lobby_effect ~game_id () in
      (* Step 3: Update UI based on result *)
      match result with
      | Ok (game_id, players) ->
        Ui_effect.Many [
          set_current_game_id (Some game_id);
          set_player_role (Some `Player2);
          set_lobby_screen (In_lobby { game_id; players; is_host = false })
        ]
      | Error _err ->
        Ui_effect.Many [
          set_lobby_screen Main_menu;
          (* In a real app, you'd show error message *)
        ]
  in
  
  (* Polling callback for game state updates *)
  let%sub poll_callback =
    let%arr game_state = game_state
    and set_game_state = set_game_state
    and current_game_id = current_game_id in
    let open Ui_effect.Let_syntax in
    match current_game_id with
    | None -> Ui_effect.Ignore
    | Some game_id ->
      (* Fetch latest state from Firebase *)
      let%bind result = Firebase_effects.fetch_game_state_effect ~game_id () in
      match result with
      | Some new_state ->
        (* Update UI if state changed *)
        if Game_state.equal new_state game_state then
          Ui_effect.Ignore
        else
          set_game_state new_state
      | None -> Ui_effect.Ignore
  in
  
  (* Schedule polling every 2 seconds *)
  let%sub () = 
    Bonsai.Clock.every 
      ~when_to_start_next_effect:`Every_multiple_of_period_blocking
      (Time_ns.Span.of_sec 2.0)
      poll_callback
  in
  
  (* Build the UI *)
  let%arr game_state = game_state
  and set_game_state = set_game_state
  and selected_card_index = selected_card_index
  and set_selected_card_index = set_selected_card_index
  and lobby_screen = lobby_screen
  and game_id_input = game_id_input
  and set_game_id_input = set_game_id_input
  and create_lobby_effect = create_lobby_effect
  and join_lobby_effect = join_lobby_effect
  and current_game_id = current_game_id
  and player_role = player_role in
  
  (* Card selection handler *)
  let on_card_select card_index =
    set_selected_card_index (Some card_index)
  in
  
  (* Target selection handler - also saves to Firebase *)
  let on_target_click target =
    match selected_card_index with
    | None -> Ui_effect.Ignore
    | Some card_index ->
      (* Get the current player and their hand *)
      let current_player = 
        match game_state.decision with
        | In_progress { whose_turn = `Player1 } -> game_state.player1
        | In_progress { whose_turn = `Player2 } -> game_state.player2
        | _ -> game_state.player1
      in
      (* Get the card at the selected index *)
      (match List.nth current_player.hand card_index with
       | None -> Ui_effect.Ignore
       | Some card ->
         (* Make the move *)
         let move = Game_state.Move.{ card; target } in
         (match Game_state.make_move game_state move with
          | Ok new_state -> 
            (* Save to Firebase if in multiplayer *)
            let save_effect = match current_game_id with
              | Some game_id -> 
                Ui_effect.map (Firebase_effects.save_game_state_effect ~game_id ~game_state:new_state ()) 
                  ~f:(fun _ -> ())
              | None -> Ui_effect.Ignore
            in
            Ui_effect.Many [
              set_game_state new_state;
              set_selected_card_index None;
              save_effect
            ]
          | Error _ -> 
            Ui_effect.Ignore))
  in
  
  (* End turn button handler - also saves to Firebase *)
  let on_end_turn () =
    let new_state = Game_state.end_turn game_state in
    (* If it's enemy turn, process enemy actions *)
    let final_state = 
      match new_state.decision with
      | In_progress { whose_turn = `Enemy } -> 
        Game_state.process_enemy_turn new_state
      | _ -> new_state
    in
    (* Save to Firebase if in multiplayer *)
    let save_effect = match current_game_id with
      | Some game_id -> 
        Ui_effect.map (Firebase_effects.save_game_state_effect ~game_id ~game_state:final_state ()) 
          ~f:(fun _ -> ())
      | None -> Ui_effect.Ignore
    in
    Ui_effect.Many [ set_game_state final_state; save_effect ]
  in
  
  (* Render based on lobby screen state *)
  match lobby_screen with
  | Main_menu | Creating_lobby | Joining_lobby | In_lobby _ ->
    render_lobby_ui
      ~lobby_screen
      ~game_id_input
      ~set_game_id_input
      ~on_create_lobby:create_lobby_effect
      ~on_join_lobby:join_lobby_effect
  | In_game { game_id = _; player_role = _ } ->
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "game-container" ]
      [ render_header ()
      ; (match current_game_id with
         | Some id -> render_lobby_ui
             ~lobby_screen:(In_game { game_id = id; player_role = Option.value_exn player_role })
             ~game_id_input
             ~set_game_id_input
             ~on_create_lobby:create_lobby_effect
             ~on_join_lobby:join_lobby_effect
         | None -> Vdom.Node.div ~attrs:[] [])
      ; render_game_status game_state.decision
      ; render_battle_area game_state ~selected_card_index ~on_target_click
      ; render_hand game_state ~selected_card_index ~on_card_select ~on_end_turn
      ]
;;

(* Start the Bonsai app *)
let () = Bonsai_web.Start.start app