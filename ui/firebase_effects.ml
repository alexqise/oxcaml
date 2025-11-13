open! Core
open Tictactoe_logic_library
open Hw2_slaythespire_logic
open Firebase_async

(* Convert async functions to Bonsai effects *)
module Firebase_effects = struct
  (* Create lobby effect *)
  let create_lobby_effect () : (string * string list, string) Result.t Ui_effect.t =
    Bonsai_web.Effect.of_deferred_fun Firebase_async.create_lobby_async ()
  
  (* Join lobby effect *)
  let join_lobby_effect ~game_id () : (string * string list, string) Result.t Ui_effect.t =
    Bonsai_web.Effect.of_deferred_fun (Firebase_async.join_lobby_async ~game_id) ()
  
  (* Fetch game state effect *)
  let fetch_game_state_effect ~game_id () : Game_state.t option Ui_effect.t =
    Bonsai_web.Effect.of_deferred_fun (Firebase_async.fetch_game_state_async ~game_id) ()
  
  (* Save game state effect *)
  let save_game_state_effect ~game_id ~game_state () : (unit, string) Result.t Ui_effect.t =
    Bonsai_web.Effect.of_deferred_fun (Firebase_async.save_game_state_async ~game_id ~game_state) ()
end

