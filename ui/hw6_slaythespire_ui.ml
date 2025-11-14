open! Core
open Tictactoe_logic_library
open Hw2_slaythespire_logic
open Virtual_dom
open! Bonsai.Let_syntax
open Firebase_effects
open Js_of_ocaml

(* Simple JS console logger for UI-level debugging.
   This helps verify that the Google sign-in button and handler run. *)
let js_log (msg : string) =
  try
    let console = Js.Unsafe.get Js.Unsafe.global "console" in
    ignore
      (Js.Unsafe.meth_call console "log"
         [| Js.Unsafe.inject (Js.string ("[UI] " ^ msg)) |])
  with _ -> ()

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

(* App screen state - includes auth, home, lobby, and game states *)
type app_screen =
  | Auth_screen of { email : string; password : string; is_signup : bool; error : string option }
  | Home_screen of { user_id : string; user_email : string; user_display_name : string option; user_photo_url : string option; wins : int; losses : int }
  | Lobby_screen of lobby_screen
  | In_game of { game_id : string; player_role : [`Player1 | `Player2] }
  | Game_over of { won : bool; user_id : string; user_email : string }
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
              [ Vdom.Node.text "Multiplayer Lobbdy" ]
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

(* Render authentication screen *)
let render_auth_screen 
    ~email 
    ~password 
    ~set_email 
    ~set_password 
    ~on_sign_in 
    ~on_sign_up
    ~on_google_sign_in
    ~error =
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "auth-container" ]
    [ Vdom.Node.create "h2" 
        ~attrs:[ Vdom.Attr.class_ "auth-title" ]
        [ Vdom.Node.text "Sign In to Play" ]
    ; Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "auth-form" ]
        [ (match error with
           | Some err_msg ->
             Vdom.Node.div
               ~attrs:[ Vdom.Attr.class_ "auth-error" ]
               [ Vdom.Node.text err_msg ]
           | None -> Vdom.Node.div ~attrs:[] [])
        ; Vdom.Node.create "input"
            ~attrs:[ Vdom.Attr.type_ "email"
                   ; Vdom.Attr.class_ "auth-input"
                   ; Vdom.Attr.placeholder "Email"
                   ; Vdom.Attr.value email
                   ; Vdom.Attr.on_input (fun _ text -> set_email text)
                   ]
            []
        ; Vdom.Node.create "input"
            ~attrs:[ Vdom.Attr.type_ "password"
                   ; Vdom.Attr.class_ "auth-input"
                   ; Vdom.Attr.placeholder "Password"
                   ; Vdom.Attr.value password
                   ; Vdom.Attr.on_input (fun _ text -> set_password text)
                   ]
            []
        ; Vdom.Node.create "button"
            ~attrs:[ Vdom.Attr.class_ "auth-btn"
                   ; Vdom.Attr.on_click (fun _ -> on_sign_in ())
                   ]
            [ Vdom.Node.text "Sign In" ]
        ; Vdom.Node.create "button"
            ~attrs:[ Vdom.Attr.class_ "auth-btn"
                   ; Vdom.Attr.on_click (fun _ -> on_sign_up ())
                   ]
            [ Vdom.Node.text "Sign Up" ]
        ; Vdom.Node.create "button"
            ~attrs:[ Vdom.Attr.class_ "auth-btn"
                   ; Vdom.Attr.on_click (fun _ -> on_google_sign_in ())
                   ]
            [ Vdom.Node.text "Sign in with Google" ]
        ]
    ]
;;

(* Render home/analytics screen *)
let render_home_screen 
    ~user_email 
    ~user_display_name
    ~user_photo_url
    ~wins 
    ~losses 
    ~on_create_lobby 
    ~on_join_lobby
    ~game_id_input
    ~set_game_id_input
    ~on_sign_out =
  let win_rate = if wins + losses > 0 then
    Float.of_int wins /. Float.of_int (wins + losses) *. 100.0
  else 0.0 in
  (* Use display name if available, otherwise fall back to email *)
  let display_text = match user_display_name with
    | Some name -> name
    | None -> user_email
  in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "home-container" ]
    [ (* User profile section with optional photo *)
      (match user_photo_url with
       | Some photo_url ->
         Vdom.Node.div
           ~attrs:[ Vdom.Attr.class_ "user-profile" ]
           [ Vdom.Node.create "img"
               ~attrs:[ Vdom.Attr.class_ "profile-photo"
                      ; Vdom.Attr.create "src" photo_url
                      ; Vdom.Attr.create "alt" "Profile"
                      ; Vdom.Attr.style (Css_gen.create
                          ~field:"border-radius" ~value:"50%")
                      ; Vdom.Attr.style (Css_gen.create
                          ~field:"width" ~value:"80px")
                      ; Vdom.Attr.style (Css_gen.create
                          ~field:"height" ~value:"80px")
                      ; Vdom.Attr.style (Css_gen.create
                          ~field:"object-fit" ~value:"cover")
                      ]
               []
           ]
       | None -> Vdom.Node.div ~attrs:[] [])
    ; Vdom.Node.create "h1" 
        ~attrs:[ Vdom.Attr.class_ "home-title" ]
        [ Vdom.Node.text (sprintf "Welcome, %s!" display_text) ]
    ; Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "stats-container" ]
        [ Vdom.Node.div 
            ~attrs:[ Vdom.Attr.class_ "stat-item" ]
            [ Vdom.Node.text (sprintf "Wins: %d" wins) ]
        ; Vdom.Node.div 
            ~attrs:[ Vdom.Attr.class_ "stat-item" ]
            [ Vdom.Node.text (sprintf "Losses: %d" losses) ]
        ; Vdom.Node.div 
            ~attrs:[ Vdom.Attr.class_ "stat-item" ]
            [ Vdom.Node.text (sprintf "Win Rate: %.1f%%" win_rate) ]
        ]
    ; Vdom.Node.create "button"
        ~attrs:[ Vdom.Attr.class_ "lobby-btn"
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
    ; Vdom.Node.create "button"
        ~attrs:[ Vdom.Attr.class_ "auth-btn"
               ; Vdom.Attr.on_click (fun _ -> on_sign_out ())
               ]
        [ Vdom.Node.text "Sign Out" ]
    ]
;;

(* Render game over screen *)
let render_game_over_screen 
    ~won 
    ~on_return_home =
  let title, message, title_class = if won then
    ("Victory!", "Congratulations! You defeated all enemies!", "victory-title")
  else
    ("Defeat", "All players have fallen. Better luck next time!", "defeat-title")
  in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "game-over-container" ]
    [ Vdom.Node.create "h1"
        ~attrs:[ Vdom.Attr.class_ title_class ]
        [ Vdom.Node.text title ]
    ; Vdom.Node.create "p"
        ~attrs:[ Vdom.Attr.class_ "game-over-message" ]
        [ Vdom.Node.text message ]
    ; Vdom.Node.create "button"
        ~attrs:[ Vdom.Attr.class_ "lobby-btn"
               ; Vdom.Attr.on_click (fun _ -> on_return_home ())
               ]
        [ Vdom.Node.text "Return to Home" ]
    ]
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
    ~on_end_turn
    ~is_my_turn
    ~player_role =
  (* Get YOUR player based on player_role, not whose turn it is *)
  let my_player = match player_role with
    | Some `Player1 -> Some (game_state.player1, "Player 1")
    | Some `Player2 -> Some (game_state.player2, "Player 2")
    | None -> None
  in
  
  (* Get whose turn it is *)
  let whose_turn = match game_state.decision with
    | In_progress { whose_turn } -> Some whose_turn
    | _ -> None
  in

  match my_player, whose_turn with
  | None, _ -> Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "cards-area" ] []
  | Some (player, player_name), Some turn ->
    (* Show your hand if it's your turn, or show waiting message if it's not *)
    let hand_title = 
      if is_my_turn then
        sprintf "%s's Hand" player_name
      else
        (match turn with
         | `Player1 -> "Waiting for Player 1..."
         | `Player2 -> "Waiting for Player 2..."
         | `Enemy -> "Enemy Turn...")
    in
    
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "cards-area" ]
      [ Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "hand-container" ]
          [ Vdom.Node.div
              ~attrs:[ Vdom.Attr.class_ "cards-title" ]
              [ Vdom.Node.text hand_title ]
          ; (if is_my_turn then
              Vdom.Node.create "button"
              ~attrs:
                [ Vdom.Attr.class_ "end-turn-btn"
                ; Vdom.Attr.on_click (fun _ -> on_end_turn ())
                ]
              [ Vdom.Node.text "End Turn" ]
            else
              Vdom.Node.div
                ~attrs:[ Vdom.Attr.class_ "waiting-message" ]
                [ Vdom.Node.text (match turn with
                   | `Player1 -> "Waiting for Player 1 to finish their turn..."
                   | `Player2 -> "Waiting for Player 2 to finish their turn..."
                   | `Enemy -> "Enemy turn in progress...") ])
          ]
      ; (if is_my_turn then
          (* Show YOUR hand when it's your turn *)
          Vdom.Node.div
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
        else
          (* Show waiting message when it's not your turn *)
          Vdom.Node.div
            ~attrs:[ Vdom.Attr.class_ "waiting-hand" ]
            [ Vdom.Node.text (match turn with
               | `Player1 -> "Player 1 is playing..."
               | `Player2 -> "Player 2 is playing..."
               | `Enemy -> "Enemy turn...") ])
      ]
  | Some (player, player_name), None ->
    (* Game over state *)
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "cards-area" ]
      [ Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "hand-container" ]
          [ Vdom.Node.div
              ~attrs:[ Vdom.Attr.class_ "cards-title" ]
              [ Vdom.Node.text (sprintf "%s's Hand" player_name) ]
          ]
      ; Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "hand" ]
          (List.mapi player.hand ~f:(fun _index card ->
             let cost = Card.energy_cost card in
             let can_afford = player.energy >= cost in
             render_card 
               card 
               ~selected:false 
               ~on_select:(fun () -> Ui_effect.Ignore) 
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
      [ Enemy_state.create ~kind:"Orc" ~max_hp:1 ~intent:(Attack 8)
      ; Enemy_state.create ~kind:"Dragon" ~max_hp:1 ~intent:(Attack 12)
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
  
  (* Auth and app state *)
  let%sub current_user, set_current_user =
    Bonsai.state ~default_model:None (module struct
      type t = (string * string) option [@@deriving sexp, equal] (* (user_id, email) *)
    end)
  in
  
  let%sub app_screen, set_app_screen =
    Bonsai.state ~default_model:(Auth_screen { email = ""; password = ""; is_signup = false; error = None }) (module struct
      type t = app_screen [@@deriving sexp, equal]
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
  
  (* Track if stats have been updated for current game *)
  let%sub stats_updated_for_game, set_stats_updated_for_game =
    Bonsai.state ~default_model:None (module struct
      type t = string option [@@deriving sexp, equal] (* game_id if stats updated *)
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
  
  (* Auth state for auth screen *)
  let%sub auth_email, set_auth_email =
    Bonsai.state ~default_model:"" (module String)
  in
  
  let%sub auth_password, set_auth_password =
    Bonsai.state ~default_model:"" (module String)
  in
  
  let%sub auth_error, set_auth_error =
    Bonsai.state ~default_model:None (module struct
      type t = string option [@@deriving sexp, equal]
    end)
  in
  
  (* Google sign-in effect handler *)
  let%sub google_sign_in_effect =
    let%arr set_current_user = set_current_user
    and set_app_screen = set_app_screen
    and set_auth_error = set_auth_error in
    let open Ui_effect.Let_syntax in
    fun () ->
      (* Trigger Google OAuth popup via Firebase Auth *)
      js_log "Google sign-in button clicked";
      let%bind result = Firebase_effects.google_sign_in_effect () in
      match result with
      | Ok user ->
        (* Debug: log the entire user object structure *)
        js_log "Google sign-in returned user object";
        (try
          let user_json = Js.Unsafe.meth_call 
            (Js.Unsafe.get Js.Unsafe.global "JSON") "stringify" 
            [| Js.Unsafe.inject user |] in
          js_log (sprintf "User object: %s" (Js.to_string user_json))
        with _ -> js_log "Could not stringify user object");
        
        let user_id = Firebase_auth.Firebase_auth.get_user_id user in
        (* Validate user_id is not empty *)
        if String.is_empty user_id then
          let error_msg = "Google sign-in succeeded but user ID is empty" in
          let () = js_log error_msg in
          Ui_effect.Many [ set_auth_error (Some error_msg) ]
        else
          let user_email = Firebase_auth.Firebase_auth.get_user_email user in
          (* Extract display name and photo URL from Google Auth *)
          let user_display_name = Firebase_auth.Firebase_auth.get_user_display_name user in
          let user_photo_url = Firebase_auth.Firebase_auth.get_user_photo_url user in
          js_log (sprintf "Extracted: uid=%s email=%s name=%s photo=%s" 
            user_id user_email 
            (Option.value user_display_name ~default:"(none)")
            (Option.value user_photo_url ~default:"(none)"));
          
          (* Explicitly check if user exists and create if not *)
          let%bind user_exists = Firebase_effects.user_exists_effect ~user_id () in
          let%bind wins, losses =
            if user_exists then
              (* User exists, fetch stats *)
              let%bind stats_result = Firebase_effects.fetch_user_stats_effect ~user_id () in
              match stats_result with
              | Ok (w, l) -> Ui_effect.return (w, l)
              | Error _ -> Ui_effect.return (0, 0)  (* Fallback if fetch fails *)
            else
              (* User doesn't exist, create profile *)
              let () = js_log (sprintf "Creating new user profile for: %s" user_id) in
              let%bind create_result = Firebase_effects.create_user_profile_effect ~user_id ~email:user_email () in
              match create_result with
              | Ok () -> 
                  let () = js_log (sprintf "Successfully created user profile for: %s" user_id) in
                  Ui_effect.return (0, 0)
              | Error err ->
                  let () = js_log (sprintf "Failed to create user profile: %s" err) in
                  Ui_effect.return (0, 0)  (* Continue anyway *)
          in
          (* Register FCM token and schedule reminder just like email/password *)
          let%bind _ = Firebase_effects.register_fcm_token_effect ~user_id () in
          let%bind _ = Firebase_effects.schedule_daily_reminder_effect ~user_id () in
          Ui_effect.Many
            [ set_current_user (Some (user_id, user_email))
            ; set_app_screen (Home_screen { user_id; user_email; user_display_name; user_photo_url; wins; losses })
            ; set_auth_error None
            ]
      | Error err ->
        js_log (sprintf "Google sign-in failed: %s" err);
        Ui_effect.Many [ set_auth_error (Some err) ]
  in
  
  (* Sign in effect handler *)
  let%sub sign_in_effect =
    let%arr set_current_user = set_current_user
    and set_app_screen = set_app_screen
    and auth_email = auth_email
    and auth_password = auth_password
    and set_auth_error = set_auth_error in
    let open Ui_effect.Let_syntax in
    fun () ->
      let%bind result = Firebase_effects.sign_in_effect ~email:auth_email ~password:auth_password () in
      match result with
      | Ok user ->
        let user_id = Firebase_auth.Firebase_auth.get_user_id user in
        (* Validate user_id is not empty *)
        if String.is_empty user_id then
          let error_msg = "Authentication succeeded but user ID is empty" in
          let () = js_log error_msg in
          Ui_effect.Many [ set_auth_error (Some error_msg) ]
        else
          let user_email = Firebase_auth.Firebase_auth.get_user_email user in
          (* Fetch user stats - if it fails, create profile *)
          let%bind stats_result = Firebase_effects.fetch_user_stats_effect ~user_id () in
          let%bind wins, losses = match stats_result with
            | Ok (w, l) -> 
              Ui_effect.return (w, l)
            | Error _ -> 
              (* Profile doesn't exist, create it *)
              let%bind _ = Firebase_effects.create_user_profile_effect ~user_id ~email:user_email () in
              Ui_effect.return (0, 0)
          in
          (* Register FCM token for push notifications *)
          let%bind _ = Firebase_effects.register_fcm_token_effect ~user_id () in
          (* Schedule daily reminder *)
          let%bind _ = Firebase_effects.schedule_daily_reminder_effect ~user_id () in
          Ui_effect.Many [
            set_current_user (Some (user_id, user_email));
            set_app_screen (Home_screen { user_id; user_email; user_display_name = None; user_photo_url = None; wins; losses });
            set_auth_error None
          ]
      | Error err ->
        Ui_effect.Many [
          set_auth_error (Some err)
        ]
  in
  
  (* Sign up effect handler *)
  let%sub sign_up_effect =
    let%arr set_current_user = set_current_user
    and set_app_screen = set_app_screen
    and auth_email = auth_email
    and auth_password = auth_password
    and set_auth_error = set_auth_error in
    let open Ui_effect.Let_syntax in
    fun () ->
      let%bind result = Firebase_effects.sign_up_effect ~email:auth_email ~password:auth_password () in
      match result with
      | Ok user ->
        let user_id = Firebase_auth.Firebase_auth.get_user_id user in
        (* Validate user_id is not empty *)
        if String.is_empty user_id then
          let error_msg = "Sign up succeeded but user ID is empty" in
          let () = js_log error_msg in
          Ui_effect.Many [ set_auth_error (Some error_msg) ]
        else
          let user_email = Firebase_auth.Firebase_auth.get_user_email user in
          (* Create user profile *)
          let%bind _ = Firebase_effects.create_user_profile_effect ~user_id ~email:user_email () in
          (* Register FCM token for push notifications *)
          let%bind _ = Firebase_effects.register_fcm_token_effect ~user_id () in
          (* Schedule daily reminder *)
          let%bind _ = Firebase_effects.schedule_daily_reminder_effect ~user_id () in
          Ui_effect.Many [
            set_current_user (Some (user_id, user_email));
            set_app_screen (Home_screen { user_id; user_email; user_display_name = None; user_photo_url = None; wins = 0; losses = 0 });
            set_auth_error None
          ]
      | Error err ->
        Ui_effect.Many [
          set_auth_error (Some err)
        ]
  in
  
  (* Sign out effect handler *)
  let%sub sign_out_effect =
    let%arr set_current_user = set_current_user
    and set_app_screen = set_app_screen
    and set_game_id_input = set_game_id_input
    and set_current_game_id = set_current_game_id
    and set_player_role = set_player_role in
    let open Ui_effect.Let_syntax in
    fun () ->
      let%bind _ = Firebase_effects.sign_out_effect () in
      Ui_effect.Many [
        set_current_user None;
        set_app_screen (Auth_screen { email = ""; password = ""; is_signup = false; error = None });
        set_game_id_input "";
        set_current_game_id None;
        set_player_role None
      ]
  in
  
  (* Auth state listener - check on mount and when auth changes *)
  let%sub auth_state_listener =
    let%arr set_current_user = set_current_user
    and set_app_screen = set_app_screen in
    let open Ui_effect.Let_syntax in
    (* Check current user on mount *)
    match Firebase_auth.Firebase_auth.get_current_user () with
    | Some user ->
      let user_id = Firebase_auth.Firebase_auth.get_user_id user in
      (* Validate user_id is not empty - if empty, treat as no user *)
      if String.is_empty user_id then
        let () = js_log "Warning: User object found but user_id is empty, treating as logged out" in
        Ui_effect.Ignore
      else
        let user_email = Firebase_auth.Firebase_auth.get_user_email user in
        (* Also extract display name and photo for persisted Google sessions *)
        let user_display_name = Firebase_auth.Firebase_auth.get_user_display_name user in
        let user_photo_url = Firebase_auth.Firebase_auth.get_user_photo_url user in
        let%bind stats_result = Firebase_effects.fetch_user_stats_effect ~user_id () in
        let wins, losses = match stats_result with
          | Ok (w, l) -> (w, l)
          | Error _ -> (0, 0)
        in
        Ui_effect.Many [
          set_current_user (Some (user_id, user_email));
          set_app_screen (Home_screen { user_id; user_email; user_display_name; user_photo_url; wins; losses })
        ]
    | None ->
      Ui_effect.Ignore
  in
  
  (* Run auth state listener on mount *)
  let%sub () = 
    Bonsai.Edge.lifecycle
      ~on_activate:auth_state_listener
      ()
  in
  
  (* Create lobby effect handler *)
  let%sub create_lobby_effect =
    let%arr set_app_screen = set_app_screen
    and set_current_game_id = set_current_game_id
    and set_player_role = set_player_role
    and set_stats_updated_for_game = set_stats_updated_for_game
    and current_user = current_user in
    let open Ui_effect.Let_syntax in
    fun () ->
      match current_user with
      | None -> Ui_effect.Ignore (* Must be authenticated *)
      | Some (user_id, user_email) ->
        (* Validate user_id is not empty *)
        if String.is_empty user_id then
          let () = js_log "Error: Cannot create lobby - user_id is empty" in
          Ui_effect.Ignore
        else
          (* Step 1: Show loading state immediately *)
          let%bind () = set_app_screen (Lobby_screen Creating_lobby) in
          (* Step 2: Wait for async Firebase call *)
          let%bind result = Firebase_effects.create_lobby_effect ~user_id ~user_email () in
          (* Step 3: Update UI based on result *)
          match result with
          | Ok (game_id, players) ->
            (* Step 4: Get FCM token and store it in game document *)
            let () = js_log (sprintf "Creating lobby - getting FCM token for user: %s" user_id) in
            let%bind fcm_result = Firebase_effects.register_fcm_token_effect ~user_id () in
            let fcm_token = match fcm_result with
              | Ok token -> 
                  let () = js_log (sprintf "✅ Got FCM token: %s..." (String.prefix token 20)) in
                  Some token
              | Error err -> 
                  let () = js_log (sprintf "❌ Failed to get FCM token: %s" err) in
                  None
            in
            (* Store FCM token in game document for Player 1 - execute sequentially *)
            let%bind storage_result = match fcm_token with
              | Some token -> 
                  let () = js_log (sprintf "Storing FCM token in game document for Player1, game: %s" game_id) in
                  Firebase_effects.update_game_player_fcm_token_effect ~game_id ~player_role:`Player1 ~fcm_token:token ()
              | None -> 
                  let () = js_log "⚠️ No FCM token to store" in
                  Ui_effect.return (Ok ())
            in
            (match storage_result with
            | Ok () -> js_log "✅ FCM token stored successfully in game document"
            | Error err -> js_log (sprintf "❌ Failed to store FCM token: %s" err)
            );
            Ui_effect.Many [
              set_current_game_id (Some game_id);
              set_player_role (Some `Player1);
              set_stats_updated_for_game None; (* Reset stats tracking for new game *)
              set_app_screen (Lobby_screen (In_lobby { game_id; players; is_host = true }))
            ]
          | Error _err ->
            Ui_effect.Many [
              set_app_screen (Lobby_screen Main_menu);
              (* In a real app, you'd show error message *)
            ]
  in
  
  (* Join lobby effect handler *)
  let%sub join_lobby_effect =
    let%arr set_app_screen = set_app_screen
    and set_current_game_id = set_current_game_id
    and set_player_role = set_player_role
    and set_game_state = set_game_state
    and set_stats_updated_for_game = set_stats_updated_for_game
    and current_user = current_user in
    let open Ui_effect.Let_syntax in
    fun game_id ->
      match current_user with
      | None -> Ui_effect.Ignore (* Must be authenticated *)
      | Some (user_id, user_email) ->
        (* Validate user_id is not empty *)
        if String.is_empty user_id then
          let () = js_log "Error: Cannot join lobby - user_id is empty" in
          Ui_effect.Ignore
        else
          (* Step 1: Show loading state immediately *)
          let%bind () = set_app_screen (Lobby_screen Joining_lobby) in
          (* Step 2: Wait for async Firebase call *)
          let%bind result = Firebase_effects.join_lobby_effect ~game_id ~user_id ~user_email () in
          (* Step 3: Update UI based on result *)
          match result with
          | Ok (game_id, players) ->
            (* Step 4: Get FCM token and store it in game document *)
            let () = js_log (sprintf "Joining lobby - getting FCM token for user: %s" user_id) in
            let%bind fcm_result = Firebase_effects.register_fcm_token_effect ~user_id () in
            let fcm_token = match fcm_result with
              | Ok token -> 
                  let () = js_log (sprintf "✅ Got FCM token: %s..." (String.prefix token 20)) in
                  Some token
              | Error err -> 
                  let () = js_log (sprintf "❌ Failed to get FCM token: %s" err) in
                  None
            in
            (* Store FCM token in game document for Player 2 - execute sequentially *)
            let%bind storage_result = match fcm_token with
              | Some token -> 
                  let () = js_log (sprintf "Storing FCM token in game document for Player2, game: %s" game_id) in
                  Firebase_effects.update_game_player_fcm_token_effect ~game_id ~player_role:`Player2 ~fcm_token:token ()
              | None -> 
                  let () = js_log "⚠️ No FCM token to store" in
                  Ui_effect.return (Ok ())
            in
            (match storage_result with
            | Ok () -> js_log "✅ FCM token stored successfully in game document"
            | Error err -> js_log (sprintf "❌ Failed to store FCM token: %s" err)
            );
            let%bind state_opt = Firebase_effects.fetch_game_state_effect ~game_id () in
            let apply_state_effect =
              match state_opt with
              | Some state -> set_game_state state
              | None -> Ui_effect.Ignore
            in
            Ui_effect.Many [
              set_current_game_id (Some game_id);
              set_player_role (Some `Player2);
              set_stats_updated_for_game None; (* Reset stats tracking for new game *)
              apply_state_effect;
              (* Player 2 should stay in In_lobby until polling callback detects valid game state *)
              set_app_screen (Lobby_screen (In_lobby { game_id; players; is_host = false }))
            ]
          | Error _err ->
            Ui_effect.Many [
              set_app_screen (Lobby_screen Main_menu);
              (* In a real app, you'd show error message *)
            ]
  in
  
  (* Polling callback for lobby status to detect when player2 joins *)
  let%sub lobby_poll_callback =
    let%arr app_screen = app_screen
    and set_app_screen = set_app_screen
    and set_game_state = set_game_state
    and current_game_id = current_game_id
    and player_role = player_role
    and game_state = game_state in
    let open Ui_effect.Let_syntax in
    match current_game_id, app_screen with
    | Some game_id, Lobby_screen (In_lobby { game_id = lobby_id; is_host; _ }) when String.equal game_id lobby_id ->
      if is_host then (
        (* Player 1: Check if player 2 has joined, then save state and transition *)
        let%bind result = Firebase_effects.fetch_lobby_status_effect ~game_id () in
        match result with
        | Ok (player2_joined, _status_in_progress) ->
          if player2_joined then (
            (* Player 2 has joined - save initial game state to Firebase, then transition *)
            let%bind save_result = Firebase_effects.save_game_state_effect ~game_id ~game_state () in
            match save_result with
            | Ok () ->
              Ui_effect.Many [
                set_app_screen (In_game { game_id; player_role = Option.value_exn player_role })
              ]
            | Error _ ->
              Ui_effect.Many [
                set_app_screen (In_game { game_id; player_role = Option.value_exn player_role })
              ]
          ) else
            Ui_effect.Ignore
        | Error _ -> Ui_effect.Ignore
      ) else (
        (* Player 2: FORCE TRANSITION - check for state, but transition no matter what *)
        let%bind firebase_state = Firebase_effects.fetch_game_state_effect ~game_id () in
        match firebase_state with
        | Some state ->
          Ui_effect.Many [
            set_game_state state;
            set_app_screen (In_game { game_id; player_role = Option.value_exn player_role })
          ]
        | None ->
          Ui_effect.Many [
            set_app_screen (In_game { game_id; player_role = Option.value_exn player_role })
          ]
      )
    | _ -> Ui_effect.Ignore
  in
  
  (* Schedule lobby polling every 1 second (more frequent than game state polling) *)
  let%sub () = 
    Bonsai.Clock.every 
      ~when_to_start_next_effect:`Every_multiple_of_period_blocking
      (Time_ns.Span.of_sec 1.0)
      lobby_poll_callback
  in
  
  (* Polling callback for game state updates *)
  let%sub poll_callback =
    let%arr set_game_state = set_game_state
    and current_game_id = current_game_id
    and app_screen = app_screen in
    let open Ui_effect.Let_syntax in
    (* Only poll when in game *)
    match current_game_id with
    | None -> Ui_effect.Ignore
    | Some game_id ->
      (* Check if we're in game mode *)
      let in_game = match app_screen with
        | In_game _ -> true
        | _ -> false
      in
      if in_game then (
        let%bind result = Firebase_effects.fetch_game_state_effect ~game_id () in
        match result with
        | Some new_state ->
          let has_valid_state = 
            (List.length new_state.player1.hand > 0 || List.length new_state.player1.draw_pile > 0) &&
            (List.length new_state.player2.hand > 0 || List.length new_state.player2.draw_pile > 0)
          in
          let has_enemies = List.length new_state.enemies > 0 in
          let state_is_valid = has_valid_state || has_enemies in
          if state_is_valid then (
            set_game_state new_state
          ) else (
            if has_enemies then
              set_game_state new_state
            else
              Ui_effect.Ignore
          )
        | None -> Ui_effect.Ignore
      ) else
        Ui_effect.Ignore
  in
  
  (* Schedule polling every 2 seconds *)
  let%sub () = 
    Bonsai.Clock.every 
      ~when_to_start_next_effect:`Every_multiple_of_period_blocking
      (Time_ns.Span.of_sec 2.0)
      poll_callback
  in
  
  (* Game result tracking - update stats when game ends and show game over screen *)
  let%sub game_end_handler =
    let%arr game_state = game_state
    and current_user = current_user
    and current_game_id = current_game_id
    and player_role = player_role
    and set_app_screen = set_app_screen
    and app_screen = app_screen
    and stats_updated_for_game = stats_updated_for_game
    and set_stats_updated_for_game = set_stats_updated_for_game in
    let open Ui_effect.Let_syntax in
    (* Only process if game ended, we're in game, and stats haven't been updated yet *)
    match game_state.decision, current_user, current_game_id, player_role, app_screen, stats_updated_for_game with
    | (Victory | Defeat), Some (user_id, user_email), Some game_id, Some _, In_game _, None ->
      (* Determine if current user won *)
      let won = match game_state.decision with
        | Victory -> true
        | Defeat -> false
        | _ -> false
      in
      (* Update stats and wait for completion *)
      let%bind _update_result = Firebase_effects.update_user_stats_effect ~user_id ~won () in
      (* Note: Even if update fails, we still show game over screen *)
      (* Mark stats as updated for this game and transition to game over screen *)
      Ui_effect.Many [
        set_stats_updated_for_game (Some game_id);
        set_app_screen (Game_over { won; user_id; user_email })
      ]
    | _ -> Ui_effect.Ignore
  in
  
  (* Return to home from game over screen *)
  let%sub return_to_home_effect =
    let%arr set_app_screen = set_app_screen
    and current_user = current_user
    and set_stats_updated_for_game = set_stats_updated_for_game
    and set_current_game_id = set_current_game_id
    and set_player_role = set_player_role in
    let open Ui_effect.Let_syntax in
    fun () ->
      match current_user with
      | None -> Ui_effect.Ignore
      | Some (user_id, user_email) ->
        (* Fetch updated stats - update should have completed by now *)
        let%bind stats_result = Firebase_effects.fetch_user_stats_effect ~user_id () in
        let wins, losses = match stats_result with
          | Ok (w, l) -> (w, l)
          | Error _ -> (0, 0)
        in
        Ui_effect.Many [
          set_app_screen (Home_screen { user_id; user_email; user_display_name = None; user_photo_url = None; wins; losses });
          set_stats_updated_for_game None;
          set_current_game_id None;
          set_player_role None
        ]
  in
  
  (* Monitor game state for end condition *)
  let%sub () = 
    Bonsai.Clock.every 
      ~when_to_start_next_effect:`Every_multiple_of_period_blocking
      (Time_ns.Span.of_sec 1.0)
      game_end_handler
  in
  
  (* Build the UI *)
  let%arr game_state = game_state
  and set_game_state = set_game_state
  and selected_card_index = selected_card_index
  and set_selected_card_index = set_selected_card_index
  and app_screen = app_screen
  and game_id_input = game_id_input
  and set_game_id_input = set_game_id_input
  and create_lobby_effect = create_lobby_effect
  and join_lobby_effect = join_lobby_effect
  and current_game_id = current_game_id
  and player_role = player_role
  and auth_email = auth_email
  and set_auth_email = set_auth_email
  and auth_password = auth_password
  and set_auth_password = set_auth_password
  and sign_in_effect = sign_in_effect
  and sign_up_effect = sign_up_effect
  and google_sign_in_effect = google_sign_in_effect
  and sign_out_effect = sign_out_effect
  and auth_error = auth_error
  and return_to_home_effect = return_to_home_effect in
  
  (* Check if it's the current player's turn *)
  let is_my_turn =
    match player_role, game_state.decision with
    | Some `Player1, In_progress { whose_turn = `Player1 } -> true
    | Some `Player2, In_progress { whose_turn = `Player2 } -> true
    | _ -> false
  in
  
  (* Card selection handler - only allow if it's your turn *)
  let on_card_select card_index =
    if is_my_turn then
    set_selected_card_index (Some card_index)
    else
      Ui_effect.Ignore
  in
  
  (* Target selection handler - also saves to Firebase, only if it's your turn *)
let on_target_click target =
    if not is_my_turn then Ui_effect.Ignore
    else
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
  
  (* End turn button handler - also saves to Firebase, only if it's your turn *)
  let on_end_turn () =
    if not is_my_turn then (
      let () = js_log "End turn called but not my turn" in
      Ui_effect.Ignore
    )
    else
    let () = js_log "End turn called - processing turn end" in
    let new_state = Game_state.end_turn game_state in
    (* If it's enemy turn, process enemy actions *)
    let final_state = 
      match new_state.decision with
      | In_progress { whose_turn = `Enemy } -> 
        let () = js_log "Enemy turn detected, processing enemy actions" in
        Game_state.process_enemy_turn new_state
      | _ -> new_state
    in
    let save_effect = match current_game_id with
      | Some game_id -> 
        let () = js_log (sprintf "Saving game state for game: %s" game_id) in
        Ui_effect.map (Firebase_effects.save_game_state_effect ~game_id ~game_state:final_state ()) 
          ~f:(fun _ -> ())
      | None -> 
        let () = js_log "No game ID - skipping save" in
        Ui_effect.Ignore
    in
    (* Notify the other player it's their turn (in multiplayer) *)
    let notification_effect = match current_game_id, player_role, final_state.decision with
      | Some game_id, Some role, In_progress { whose_turn } ->
        let () = js_log (sprintf "Checking notification: game_id=%s, my_role=%s, next_turn=%s" 
          game_id
          (match role with `Player1 -> "Player1" | `Player2 -> "Player2")
          (match whose_turn with `Player1 -> "Player1" | `Player2 -> "Player2" | `Enemy -> "Enemy")) in
        (* Determine who should be notified *)
        let should_notify = match role, whose_turn with
          | `Player1, `Player2 -> true  (* I'm player1, now it's player2's turn *)
          | `Player2, `Player1 -> true  (* I'm player2, now it's player1's turn *)
          | _ -> false  (* Enemy turn or same player, no notification *)
        in
        if should_notify then (
          let () = js_log (sprintf "Should notify! Triggering notification for %s" 
            (match whose_turn with `Player1 -> "Player1" | `Player2 -> "Player2" | `Enemy -> "Enemy")) in
          (* Notify the player whose turn it is now (only if it's a player, not enemy) *)
          match whose_turn with
          | `Player1 ->
            Ui_effect.map (Firebase_effects.notify_player_turn_effect ~game_id ~player_role:`Player1 ())
              ~f:(fun result -> 
                match result with
                | Ok () -> js_log "Notification queued successfully for Player1"
                | Error err -> js_log (sprintf "Notification failed: %s" err)
              )
          | `Player2 ->
            Ui_effect.map (Firebase_effects.notify_player_turn_effect ~game_id ~player_role:`Player2 ())
              ~f:(fun result -> 
                match result with
                | Ok () -> js_log "Notification queued successfully for Player2"
                | Error err -> js_log (sprintf "Notification failed: %s" err)
              )
          | `Enemy -> 
            let () = js_log "Enemy turn - no notification needed" in
            Ui_effect.Ignore
        ) else (
          let () = js_log "Should not notify (enemy turn or same player)" in
          Ui_effect.Ignore
        )
      | None, _, _ -> 
        let () = js_log "No game_id - skipping notification" in
        Ui_effect.Ignore
      | _, None, _ -> 
        let () = js_log "No player_role - skipping notification" in
        Ui_effect.Ignore
      | _, _, _ -> 
        let () = js_log "Game not in progress - skipping notification" in
        Ui_effect.Ignore
    in
    Ui_effect.Many [ set_game_state final_state; save_effect; notification_effect ]
  in
  
  (* Render based on app screen state *)
  match app_screen with
  | Auth_screen _ ->
    render_auth_screen
      ~email:auth_email
      ~password:auth_password
      ~set_email:set_auth_email
      ~set_password:set_auth_password
      ~on_sign_in:sign_in_effect
      ~on_sign_up:sign_up_effect
      ~on_google_sign_in:google_sign_in_effect
      ~error:auth_error
  | Home_screen { user_email; user_display_name; user_photo_url; wins; losses; _ } ->
    render_home_screen
      ~user_email
      ~user_display_name
      ~user_photo_url
      ~wins
      ~losses
      ~on_create_lobby:create_lobby_effect
      ~on_join_lobby:join_lobby_effect
      ~game_id_input
      ~set_game_id_input
      ~on_sign_out:sign_out_effect
  | Lobby_screen lobby_screen_state ->
    render_lobby_ui
      ~lobby_screen:lobby_screen_state
      ~game_id_input
      ~set_game_id_input
      ~on_create_lobby:create_lobby_effect
      ~on_join_lobby:join_lobby_effect
  | In_game { game_id = _; player_role = _ } ->
    (* Show game screen - game over screen will be shown via Game_over state *)
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
      ; render_hand game_state ~selected_card_index ~on_card_select ~on_end_turn ~is_my_turn ~player_role
      ]
  | Game_over { won; user_email = _; _ } ->
    render_game_over_screen
      ~won
      ~on_return_home:return_to_home_effect
;;

(* Start the Bonsai app *)
let () = Bonsai_web.Start.start app