open! Core
open Tictactoe_logic_library
open Hw2_slaythespire_logic
open Firebase_async

(* Convert async functions to Bonsai effects *)
module Firebase_effects = struct
  (* Create lobby effect *)
  let create_lobby_effect ~user_id ~user_email () : (string * string list, string) Result.t Ui_effect.t =
    Bonsai_web.Effect.of_deferred_fun (Firebase_async.create_lobby_async ~user_id ~user_email) ()
  
  (* Join lobby effect *)
  let join_lobby_effect ~game_id ~user_id ~user_email () : (string * string list, string) Result.t Ui_effect.t =
    Bonsai_web.Effect.of_deferred_fun (Firebase_async.join_lobby_async ~game_id ~user_id ~user_email) ()
  
  (* Fetch game state effect *)
  let fetch_game_state_effect ~game_id () : Game_state.t option Ui_effect.t =
    Bonsai_web.Effect.of_deferred_fun (Firebase_async.fetch_game_state_async ~game_id) ()
  
  (* Save game state effect *)
  let save_game_state_effect ~game_id ~game_state () : (unit, string) Result.t Ui_effect.t =
    Bonsai_web.Effect.of_deferred_fun (Firebase_async.save_game_state_async ~game_id ~game_state) ()
  
  (* Fetch lobby status effect *)
  let fetch_lobby_status_effect ~game_id () : (bool * bool, string) Result.t Ui_effect.t =
    Bonsai_web.Effect.of_deferred_fun (Firebase_async.fetch_lobby_status_async ~game_id) ()
  
  (* Sign in effect *)
  let sign_in_effect ~email ~password () : (Js_of_ocaml.Js.Unsafe.any, string) Result.t Ui_effect.t =
    Bonsai_web.Effect.of_deferred_fun (fun () -> Firebase_auth.Firebase_auth.sign_in_with_email_password ~email ~password) ()
  
  (* Sign up effect *)
  let sign_up_effect ~email ~password () : (Js_of_ocaml.Js.Unsafe.any, string) Result.t Ui_effect.t =
    Bonsai_web.Effect.of_deferred_fun (fun () -> Firebase_auth.Firebase_auth.create_user_with_email_password ~email ~password) ()
  
  (* Sign out effect *)
  let sign_out_effect () : (unit, string) Result.t Ui_effect.t =
    Bonsai_web.Effect.of_deferred_fun (fun () -> Firebase_auth.Firebase_auth.sign_out ()) ()
  
  (* Fetch user stats effect *)
  let fetch_user_stats_effect ~user_id () : (int * int, string) Result.t Ui_effect.t =
    Bonsai_web.Effect.of_deferred_fun (Firebase_async.fetch_user_stats_async ~user_id) ()
  
  (* Update user stats effect *)
  let update_user_stats_effect ~user_id ~won () : (unit, string) Result.t Ui_effect.t =
    Bonsai_web.Effect.of_deferred_fun (Firebase_async.update_user_stats_async ~user_id ~won) ()
  
  (* Create user profile effect *)
  let create_user_profile_effect ~user_id ~email () : (unit, string) Result.t Ui_effect.t =
    Bonsai_web.Effect.of_deferred_fun (Firebase_async.create_user_profile_async ~user_id ~email) ()
end

