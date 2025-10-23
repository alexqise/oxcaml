open! Core
open Tictactoe_logic_library
open Hw2_slaythespire_logic
open Virtual_dom
open! Bonsai.Let_syntax

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
let render_player_card (player : Player_state.t) ~show_image =
  let image_node = 
    if show_image 
    then [ Vdom.Node.create 
             "img" 
             ~attrs:[ Vdom.Attr.class_ "char-img"
                    ; Vdom.Attr.create "src" "assets/ironclad.png"
                    ; Vdom.Attr.create "alt" player.name
                    ] 
             [] 
         ]
    else []
  in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "player-card" ]
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
  (* Choose image based on enemy kind *)
  let image_src = 
    match String.lowercase enemy.kind with
    | "orc" -> "assets/orc.png"
    | "dragon" -> "assets/dragon.png"
    | _ -> "assets/orc.png"  (* Default fallback *)
  in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "enemy-card" ]
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
let render_header (game_state : Game_state.t) =
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "game-header" ]
    [ Vdom.Node.create "h1"
        ~attrs:[ Vdom.Attr.class_ "game-title" ]
        [ Vdom.Node.text "Slay the Spire" ]
    ; Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "floor-info" ]
        [ Vdom.Node.text (sprintf "Floor %d - Turn %d" game_state.floor game_state.turn_count) ]
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
    ~selected_card 
    ~on_target_click =
  (* Player area *)
  let player_area =
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "player-area" ]
      [ (* Player 1 - clickable if Defend/Heal card selected *)
        (match selected_card with
         | Some card when (match card with Card.Defend | Card.Heal -> true | _ -> false) ->
           Vdom.Node.div
             ~attrs:[ Vdom.Attr.on_click (fun _ -> on_target_click `Player1) ]
             [ render_player_card game_state.player1 ~show_image:true ]
         | _ -> render_player_card game_state.player1 ~show_image:true)
      ; (* Player 2 - clickable if Defend/Heal card selected *)
        (match selected_card with
         | Some card when (match card with Card.Defend | Card.Heal -> true | _ -> false) ->
           Vdom.Node.div
             ~attrs:[ Vdom.Attr.on_click (fun _ -> on_target_click `Player2) ]
             [ render_player_card game_state.player2 ~show_image:false ]
         | _ -> render_player_card game_state.player2 ~show_image:false)
      ]
  in
  
  (* Enemy area - with click handlers when card is selected *)
  let enemy_area =
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "enemy-area" ]
      (List.mapi game_state.enemies ~f:(fun index enemy ->
         let enemy_card = render_enemy_card enemy ~index in
         (* Wrap in clickable div if card is selected and can target enemies *)
         match selected_card with
         | None -> enemy_card
         | Some card -> 
           let can_target_enemy = match card with
             | Card.Strike | Card.Special _ -> true
             | Card.Defend | Card.Heal -> false
           in
           if can_target_enemy
           then Vdom.Node.div
             ~attrs:[ Vdom.Attr.on_click (fun _ -> on_target_click (`Enemy index)) ]
             [ enemy_card ]
           else enemy_card
       ))
  in
  
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "battle-area" ]
    [ player_area; enemy_area ]
;;

(* Render the player's hand *)
let render_hand 
    (game_state : Game_state.t) 
    ~selected_card 
    ~on_card_select =
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
          ~attrs:[ Vdom.Attr.class_ "cards-title" ]
          [ Vdom.Node.text hand_title ]
      ; Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "hand" ]
          (List.map player.hand ~f:(fun card ->
             let cost = Card.energy_cost card in
             let can_afford = player.energy >= cost in
             let is_selected = 
               match selected_card with
               | Some sel_card -> Card.equal sel_card card
               | None -> false
             in
             render_card 
               card 
               ~selected:is_selected 
               ~on_select:(fun () -> on_card_select card) 
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
    (* Start Player 1's first turn properly *)
    let player1_with_turn = Player_state.start_turn game_state.player1 in
    { game_state with player1 = player1_with_turn }
  in
  
  (* Bonsai state: game state + selected card *)
  let%sub game_state, set_game_state =
    Bonsai.state ~default_model:initial_state (module Game_state)
  in
  
  let%sub selected_card, set_selected_card =
    Bonsai.state ~default_model:None (module struct
      type t = Card.t option [@@deriving sexp, equal]
    end)
  in
  
  (* Build the UI *)
  let%arr game_state = game_state
  and set_game_state = set_game_state
  and selected_card = selected_card
  and set_selected_card = set_selected_card in
  
  (* Card selection handler *)
  let on_card_select card =
    set_selected_card (Some card)
  in
  
  (* Target selection handler *)
let on_target_click target =
  match selected_card with
  | None -> Ui_effect.Ignore  (* No card selected *)
  | Some card ->
    (* Make the move *)
    let move = Game_state.Move.{ card; target } in
    (match Game_state.make_move game_state move with
     | Ok new_state -> 
       Ui_effect.Many [ set_game_state new_state; set_selected_card None ]
     | Error _ -> 
       Ui_effect.Ignore)
in
  
  (* End turn button handler *)
  let on_end_turn () =
    let new_state = Game_state.end_turn game_state in
    (* If it's enemy turn, process enemy actions *)
    let final_state = 
      match new_state.decision with
      | In_progress { whose_turn = `Enemy } -> 
        (* Process enemy turn - this already sets turn back to Player1 *)
        Game_state.process_enemy_turn new_state
      | _ -> new_state
    in
    set_game_state final_state
  in
  
  (* Build complete UI tree *)
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "game-container" ]
    [ render_header game_state
    ; render_game_status game_state.decision
    ; render_battle_area game_state ~selected_card ~on_target_click
    ; render_hand game_state ~selected_card ~on_card_select
    ; (* End turn button *)
      Vdom.Node.create "button"
        ~attrs:
          [ Vdom.Attr.on_click (fun _ -> on_end_turn ())
          ; Vdom.Attr.style 
              (Css_gen.create 
                 ~field:"margin" 
                 ~value:"20px auto")
          ]
        [ Vdom.Node.text "End Turn" ]
    ]
;;

(* Start the Bonsai app *)
let () = Bonsai_web.Start.start app