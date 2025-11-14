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
  
  (* Google Sign-In effect
     This effect triggers a Firebase Auth Google OAuth popup and resolves
     to the Firebase user object on success. *)
  let google_sign_in_effect () : (Js_of_ocaml.Js.Unsafe.any, string) Result.t Ui_effect.t =
    Bonsai_web.Effect.of_deferred_fun
      (fun () -> Firebase_auth.Firebase_auth.sign_in_with_google ())
      ()
  
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
  
  (* Check if user exists effect *)
  let user_exists_effect ~user_id () : bool Ui_effect.t =
    Bonsai_web.Effect.of_deferred_fun (Firebase_async.user_exists_async ~user_id) ()
  
  (* Create user profile effect *)
  let create_user_profile_effect ~user_id ~email () : (unit, string) Result.t Ui_effect.t =
    Bonsai_web.Effect.of_deferred_fun (Firebase_async.create_user_profile_async ~user_id ~email) ()
  
  (* Firebase Cloud Messaging effects *)
  
  (* Register FCM token for push notifications *)
  let register_fcm_token_effect ~user_id () : (string, string) Result.t Ui_effect.t =
    Bonsai_web.Effect.of_deferred_fun (Firebase_messaging.Firebase_messaging.register_fcm_token ~user_id) ()
  
  (* Notify player it's their turn *)
  let notify_player_turn_effect ~game_id ~player_role () : (unit, string) Result.t Ui_effect.t =
    Bonsai_web.Effect.of_deferred_fun (Firebase_messaging.Firebase_messaging.notify_player_turn ~game_id ~player_role) ()
  
  (* Schedule daily reminder notification *)
  let schedule_daily_reminder_effect ~user_id () : (unit, string) Result.t Ui_effect.t =
    Bonsai_web.Effect.of_deferred_fun (Firebase_messaging.Firebase_messaging.schedule_daily_reminder ~user_id) ()
  
  (* Update FCM token for a player in the game document *)
  let update_game_player_fcm_token_effect ~game_id ~player_role ~fcm_token () : (unit, string) Result.t Ui_effect.t =
    Bonsai_web.Effect.of_deferred_fun (Firebase_async.update_game_player_fcm_token_async ~game_id ~player_role ~fcm_token) ()
  
  (* Get FCM token for a player from the game document *)
  let get_game_player_fcm_token_effect ~game_id ~player_role () : string option Ui_effect.t =
    Bonsai_web.Effect.of_deferred_fun (Firebase_async.get_game_player_fcm_token_async ~game_id ~player_role) ()
end

