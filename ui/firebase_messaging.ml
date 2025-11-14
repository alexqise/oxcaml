open! Core
open Async_kernel
open Js_of_ocaml

(* Firebase Cloud Messaging helpers for push notifications *)
module Firebase_messaging = struct
  
  (* Get the Firebase Messaging instance from JavaScript *)
  let get_messaging () =
    try
      let firebase = Js.Unsafe.get Js.Unsafe.global "firebase" in
      let messaging_func = Js.Unsafe.get firebase "messaging" in
      Js.Unsafe.fun_call messaging_func [||]
    with _ ->
      Js.Unsafe.obj [||]
  
  (* Request notification permission and get FCM token *)
  let get_fcm_token () : string Deferred.t =
    let ivar = Ivar.create () in
    try
      let messaging = get_messaging () in
      
      (* VAPID key from Firebase Console -> Cloud Messaging -> Web Push certificates *)
      let vapid_key = Js.string "BP75Vz4zkiRQfQ-EgLflsPRDjKSzpNl5My1-v3JgmrRGHyMK4mkcR1fBeK8cQj7EoDrxeIDFhifrP5oU47jprzw" in
      
      (* Get the service worker registration - Firebase Messaging needs this to use the correct service worker *)
      (* We need to get the registration that was registered in index.html at /firebase-messaging-sw.js *)
      (* This prevents Firebase from trying to register a service worker from a different domain *)
      try
        let navigator = Js.Unsafe.get Js.Unsafe.global "navigator" in
        let service_worker = Js.Unsafe.get navigator "serviceWorker" in
        (* Call getRegistration() - returns a promise that resolves to registration or null *)
        (* Use fun_call since getRegistration is a function property, not a method *)
        let registration_promise = 
          let get_registration_func = Js.Unsafe.get service_worker "getRegistration" in
          Js.Unsafe.fun_call get_registration_func [||]
        in
        
        let on_registration reg =
          (* Got the registration - now create options with it *)
          let options = Js.Unsafe.obj [||] in
          let () = Js.Unsafe.set options "vapidKey" vapid_key in
          (* Explicitly set serviceWorkerRegistration to use our registered service worker *)
          (* This tells Firebase to use OUR service worker, not try to register a new one *)
          let () = Js.Unsafe.set options "serviceWorkerRegistration" reg in
          
          (* Call messaging.getToken() with the service worker registration *)
          let promise = Js.Unsafe.meth_call messaging "getToken" [| Js.Unsafe.inject options |] in
          
          let on_success token = 
            let token_str = Js.to_string token in
            Ivar.fill ivar token_str
          in
          
          let on_error error = 
            let error_msg = 
              try Js.Unsafe.get error "message" |> Js.to_string
              with _ -> "Failed to get FCM token"
            in
            let () = Js.Unsafe.fun_call (Js.Unsafe.js_expr "console.log") 
              [| Js.Unsafe.inject (Js.string ("FCM token error: " ^ error_msg)) |] in
            Ivar.fill ivar ""
          in
          
          let _ = Js.Unsafe.meth_call promise "then" [| Js.Unsafe.inject (Js.wrap_callback on_success) |] in
          let _ = Js.Unsafe.meth_call promise "catch" [| Js.Unsafe.inject (Js.wrap_callback on_error) |] in
          ()
        in
        
        let on_no_registration _ =
          (* No service worker registered yet - try without it (Firebase will register its own) *)
          let options = Js.Unsafe.obj [||] in
          let () = Js.Unsafe.set options "vapidKey" vapid_key in
          
          let promise = Js.Unsafe.meth_call messaging "getToken" [| Js.Unsafe.inject options |] in
          
          let on_success token = 
            let token_str = Js.to_string token in
            Ivar.fill ivar token_str
          in
          
          let on_error error = 
            let error_msg = 
              try Js.Unsafe.get error "message" |> Js.to_string
              with _ -> "Failed to get FCM token"
            in
            let () = Js.Unsafe.fun_call (Js.Unsafe.js_expr "console.log") 
              [| Js.Unsafe.inject (Js.string ("FCM token error: " ^ error_msg)) |] in
            Ivar.fill ivar ""
          in
          
          let _ = Js.Unsafe.meth_call promise "then" [| Js.Unsafe.inject (Js.wrap_callback on_success) |] in
          let _ = Js.Unsafe.meth_call promise "catch" [| Js.Unsafe.inject (Js.wrap_callback on_error) |] in
          ()
        in
        
        (* Handle the registration promise - if we get a registration, use it; otherwise proceed without *)
        let _ = Js.Unsafe.meth_call registration_promise "then" 
          [| Js.Unsafe.inject (Js.wrap_callback (fun reg -> 
              if Js.Opt.test reg then
                on_registration (Js.Opt.get reg (fun () -> assert false))
              else
                on_no_registration ()))
           ; Js.Unsafe.inject (Js.wrap_callback on_no_registration) |] in
        
        Ivar.read ivar
      with _ ->
        (* Fallback: try without service worker registration *)
        let options = Js.Unsafe.obj [||] in
        let () = Js.Unsafe.set options "vapidKey" vapid_key in
        
        let promise = Js.Unsafe.meth_call messaging "getToken" [| Js.Unsafe.inject options |] in
        
        let on_success token = 
          let token_str = Js.to_string token in
          Ivar.fill ivar token_str
        in
        
        let on_error error = 
          let error_msg = 
            try Js.Unsafe.get error "message" |> Js.to_string
            with _ -> "Failed to get FCM token"
          in
          let () = Js.Unsafe.fun_call (Js.Unsafe.js_expr "console.log") 
            [| Js.Unsafe.inject (Js.string ("FCM token error: " ^ error_msg)) |] in
          Ivar.fill ivar ""
        in
        
        let _ = Js.Unsafe.meth_call promise "then" [| Js.Unsafe.inject (Js.wrap_callback on_success) |] in
        let _ = Js.Unsafe.meth_call promise "catch" [| Js.Unsafe.inject (Js.wrap_callback on_error) |] in
        
        Ivar.read ivar
    with _ ->
      let () = Js.Unsafe.fun_call (Js.Unsafe.js_expr "console.log") [| Js.Unsafe.inject (Js.string "Exception getting FCM token") |] in
      Ivar.fill ivar "";
      Ivar.read ivar
  
  (* Save FCM token to Firestore for the current user - using REST API *)
  let save_fcm_token_to_firestore ~user_id ~token () : (unit, string) Result.t Deferred.t =
    let open Deferred.Let_syntax in
    (* Validate user_id is not empty - required for Firestore document path *)
    if String.is_empty user_id then
      let error_msg = "Cannot save FCM token: user_id is empty" in
      let () = Js.Unsafe.fun_call (Js.Unsafe.js_expr "console.error") 
        [| Js.Unsafe.inject (Js.string error_msg) |] in
      return (Error error_msg)
    else
      (* Format timestamp in RFC3339 format for Firestore *)
      let timestamp = 
        let date_constructor = Js.Unsafe.get Js.Unsafe.global "Date" in
        let date_obj = Js.Unsafe.new_obj date_constructor [||] in
        Js.Unsafe.meth_call date_obj "toISOString" [||] |> Js.to_string
      in
      
      (* Use REST API like the rest of the codebase *)
      let body_json = sprintf {|{
        "fields": {
          "fcmToken": {"stringValue": "%s"},
          "lastTokenUpdate": {"timestampValue": "%s"}
        }
      }|} (String.escaped token) timestamp in
      
      let%bind result = Firebase_http.Firebase_http.patch_firestore 
        ~collection:"users" 
        ~document_id:user_id 
        ~body_json
        ~update_mask:["fcmToken"; "lastTokenUpdate"]
        ()
      in
      match result with
      | Ok _ -> Deferred.return (Ok ())
      | Error err -> Deferred.return (Error err)
  
  (* Register FCM token for current user - combines getting token and saving it *)
  let register_fcm_token ~user_id () : (string, string) Result.t Deferred.t =
    let open Deferred.Let_syntax in
    (* Validate user_id is not empty before proceeding *)
    if String.is_empty user_id then
      let error_msg = "Cannot register FCM token: user_id is empty" in
      let () = Js.Unsafe.fun_call (Js.Unsafe.js_expr "console.error") 
        [| Js.Unsafe.inject (Js.string error_msg) |] in
      return (Error error_msg)
    else
      let%bind token = get_fcm_token () in
      if String.is_empty token then
        return (Error "Failed to get FCM token")
      else
        let%bind save_result = save_fcm_token_to_firestore ~user_id ~token () in
        match save_result with
        | Ok () -> 
            let () = Js.Unsafe.fun_call (Js.Unsafe.js_expr "console.log") [| Js.Unsafe.inject (Js.string ("FCM token registered: " ^ token)) |] in
            return (Ok token)
        | Error err -> return (Error err)
  
  (* Get Firestore database instance *)
  let get_firestore () =
    try
      (* Primary: get from window.firebaseFirestore (set by index.html) *)
      Js.Unsafe.get Js.Unsafe.global "firebaseFirestore"
    with _ ->
      try
        (* Fallback: call firebase.firestore() directly *)
        let firebase = Js.Unsafe.get Js.Unsafe.global "firebase" in
        let firestore_func = Js.Unsafe.get firebase "firestore" in
        Js.Unsafe.fun_call firestore_func [||]
      with _ ->
        (* Last resort: return empty object *)
        Js.Unsafe.obj [||]
  
  (* Get player user IDs from a game document *)
  let get_game_player_ids ~game_id () : (string * string) option Deferred.t =
    let ivar = Ivar.create () in
    try
      let db = get_firestore () in
      
      (* Reference to games/{game_id} document *)
      let doc_ref = Js.Unsafe.meth_call db "collection" [| Js.Unsafe.inject (Js.string "games") |] in
      let game_doc = Js.Unsafe.meth_call doc_ref "doc" [| Js.Unsafe.inject (Js.string game_id) |] in
      
      let promise = Js.Unsafe.meth_call game_doc "get" [||] in
      
      let on_success snapshot = 
        try
          let exists = Js.Unsafe.get snapshot "exists" |> Js.to_bool in
          if exists then
            let data_func = Js.Unsafe.get snapshot "data" in
            let data = Js.Unsafe.fun_call data_func [||] in
            let player_ids = Js.Unsafe.get data "player_ids" in
            let player_ids_array = Js.Unsafe.get player_ids "arrayValue" in
            let values = Js.Unsafe.get player_ids_array "values" in
            
            (* Extract player1 and player2 user IDs *)
            let player1_id = try
              let p1_obj = Js.array_get values 0 |> Js.Optdef.to_option in
              match p1_obj with
              | Some obj -> 
                let str_val = Js.Unsafe.get obj "stringValue" in
                Js.to_string str_val
              | None -> ""
            with _ -> ""
            in
            
            let player2_id = try
              let p2_obj = Js.array_get values 1 |> Js.Optdef.to_option in
              match p2_obj with
              | Some obj -> 
                let str_val = Js.Unsafe.get obj "stringValue" in
                Js.to_string str_val
              | None -> ""
            with _ -> ""
            in
            
            if String.is_empty player1_id || String.is_empty player2_id then
              Ivar.fill ivar None
            else
              Ivar.fill ivar (Some (player1_id, player2_id))
          else
            Ivar.fill ivar None
        with _ ->
          Ivar.fill ivar None
      in
      
      let on_error _ = Ivar.fill ivar None in
      
      let _ = Js.Unsafe.meth_call promise "then" [| Js.Unsafe.inject (Js.wrap_callback on_success) |] in
      let _ = Js.Unsafe.meth_call promise "catch" [| Js.Unsafe.inject (Js.wrap_callback on_error) |] in
      
      Ivar.read ivar
    with _ ->
      Ivar.fill ivar None;
      Ivar.read ivar
  
  (* Get FCM token for another user from Firestore *)
  let get_user_fcm_token ~user_id () : string option Deferred.t =
    let ivar = Ivar.create () in
    try
      let db = get_firestore () in
      
      (* Reference to users/{user_id} document *)
      let doc_ref = Js.Unsafe.meth_call db "collection" [| Js.Unsafe.inject (Js.string "users") |] in
      let user_doc = Js.Unsafe.meth_call doc_ref "doc" [| Js.Unsafe.inject (Js.string user_id) |] in
      
      let promise = Js.Unsafe.meth_call user_doc "get" [||] in
      
      let on_success snapshot = 
        try
          let exists = Js.Unsafe.get snapshot "exists" |> Js.to_bool in
          if exists then
            let data_func = Js.Unsafe.get snapshot "data" in
            let data = Js.Unsafe.fun_call data_func [||] in
            let fcm_token_opt = Js.Optdef.to_option (Js.Unsafe.get data "fcmToken") in
            match fcm_token_opt with
            | Some token -> Ivar.fill ivar (Some (Js.to_string token))
            | None -> Ivar.fill ivar None
          else
            Ivar.fill ivar None
        with _ ->
          Ivar.fill ivar None
      in
      
      let on_error _ = Ivar.fill ivar None in
      
      let _ = Js.Unsafe.meth_call promise "then" [| Js.Unsafe.inject (Js.wrap_callback on_success) |] in
      let _ = Js.Unsafe.meth_call promise "catch" [| Js.Unsafe.inject (Js.wrap_callback on_error) |] in
      
      Ivar.read ivar
    with _ ->
      Ivar.fill ivar None;
      Ivar.read ivar
  
  (* Trigger notification for turn change - uses FCM token from game document *)
  (* player_role: which player needs to be notified (`Player1 or `Player2) *)
  let notify_player_turn ~game_id ~player_role () : (unit, string) Result.t Deferred.t =
    let open Deferred.Let_syntax in
    let () = Js.Unsafe.fun_call (Js.Unsafe.js_expr "console.log") 
      [| Js.Unsafe.inject (Js.string (sprintf "[FCM] notify_player_turn called: game_id=%s, player_role=%s" 
        game_id
        (match player_role with `Player1 -> "Player1" | `Player2 -> "Player2"))) |] in
    (* Get FCM token directly from game document (updated on each session) *)
    let%bind token_opt = Firebase_async.Firebase_async.get_game_player_fcm_token_async ~game_id ~player_role () in
    match token_opt with
    | None -> 
      (* No token found - log and return success anyway *)
      let () = Js.Unsafe.fun_call (Js.Unsafe.js_expr "console.log") 
        [| Js.Unsafe.inject (Js.string (sprintf "[FCM] No FCM token found in game document for %s" 
          (match player_role with `Player1 -> "Player1" | `Player2 -> "Player2"))) |] in
      return (Ok ())
    | Some token ->
      let () = Js.Unsafe.fun_call (Js.Unsafe.js_expr "console.log") 
        [| Js.Unsafe.inject (Js.string (sprintf "[FCM] Found token: %s..." (String.prefix token 20))) |] in
      (* Also get user ID for the notification request *)
      let%bind player_ids_opt = get_game_player_ids ~game_id () in
      let target_user_id = match player_ids_opt, player_role with
        | Some (p1_id, _), `Player1 -> p1_id
        | Some (_, p2_id), `Player2 -> p2_id
        | None, _ -> ""
      in
      (* Create a notification request document using Firestore SDK *)
      (* This matches the approach in notification-worker.js - using SDK with automatic auth *)
      let ivar = Ivar.create () in
      try
        (* Verify we have an authenticated user before proceeding *)
        let auth = 
          try Js.Unsafe.get Js.Unsafe.global "firebaseAuth"
          with _ -> Js.Unsafe.obj [||]
        in
        let current_user = Js.Unsafe.get auth "currentUser" in
        let is_authenticated = Js.Opt.test current_user in
        
        let () = Js.Unsafe.fun_call (Js.Unsafe.js_expr "console.log") 
          [| Js.Unsafe.inject (Js.string (sprintf "[FCM] Auth check: authenticated=%b, user_id=%s" 
            is_authenticated target_user_id)) |] in
        
        if not is_authenticated then
          let error_msg = "Cannot queue notification: user is not authenticated" in
          let () = Js.Unsafe.fun_call (Js.Unsafe.js_expr "console.error") 
            [| Js.Unsafe.inject (Js.string error_msg) |] in
          Ivar.fill ivar (Error error_msg);
          Ivar.read ivar
        else
          let db = get_firestore () in
          
          (* Create a notification request document - using SDK like notification-worker.js *)
          let notifications_ref = Js.Unsafe.meth_call db "collection" 
            [| Js.Unsafe.inject (Js.string "notificationRequests") |] in
          
          (* Data for notification - matches the structure expected by notification-worker.js *)
          let data = Js.Unsafe.obj [||] in
          let () = Js.Unsafe.set data "userId" (Js.string target_user_id) in
          let () = Js.Unsafe.set data "fcmToken" (Js.string token) in
          let () = Js.Unsafe.set data "gameId" (Js.string game_id) in
          let () = Js.Unsafe.set data "type" (Js.string "turn_notification") in
          let () = Js.Unsafe.set data "title" (Js.string "Your Turn! 🎮") in
          let () = Js.Unsafe.set data "body" (Js.string "It's your turn in Slay the Spire!") in
          let () = Js.Unsafe.set data "createdAt" (Js.Unsafe.js_expr "new Date()") in
          let () = Js.Unsafe.set data "processed" Js._false in
          
          (* Add document using SDK - this automatically includes auth context *)
          let promise = Js.Unsafe.meth_call notifications_ref "add" [| Js.Unsafe.inject data |] in
          
          let on_success _doc_ref = 
            let () = Js.Unsafe.fun_call (Js.Unsafe.js_expr "console.log") 
              [| Js.Unsafe.inject (Js.string (sprintf "[FCM] ✅ Turn notification queued successfully! user_id=%s, game_id=%s, token=%s..." 
                target_user_id game_id (String.prefix token 20))) |] in
            Ivar.fill ivar (Ok ())
          in
          
          let on_error error = 
            let error_msg = 
              try 
                let code = Js.Unsafe.get error "code" |> Js.to_string in
                let message = Js.Unsafe.get error "message" |> Js.to_string in
                sprintf "Firestore error [%s]: %s" code message
              with _ -> 
                try Js.Unsafe.get error "message" |> Js.to_string
                with _ -> "Failed to queue notification"
            in
            let () = Js.Unsafe.fun_call (Js.Unsafe.js_expr "console.error") 
              [| Js.Unsafe.inject (Js.string (sprintf "[FCM] ❌ Error queueing notification: %s" error_msg)) |] in
            Ivar.fill ivar (Error error_msg)
          in
          
          let _ = Js.Unsafe.meth_call promise "then" [| Js.Unsafe.inject (Js.wrap_callback on_success) |] in
          let _ = Js.Unsafe.meth_call promise "catch" [| Js.Unsafe.inject (Js.wrap_callback on_error) |] in
          
          Ivar.read ivar
      with exn ->
        let error_msg = Exn.to_string exn in
        let () = Js.Unsafe.fun_call (Js.Unsafe.js_expr "console.error") 
          [| Js.Unsafe.inject (Js.string (sprintf "[FCM] ❌ Exception queueing notification: %s" error_msg)) |] in
        Ivar.fill ivar (Error error_msg);
        Ivar.read ivar
  
  (* Schedule daily reminder notification *)
  let schedule_daily_reminder ~user_id () : (unit, string) Result.t Deferred.t =
    let ivar = Ivar.create () in
    try
      let db = get_firestore () in
      
      (* Create a scheduled notification document *)
      let scheduled_ref = Js.Unsafe.meth_call db "collection" 
        [| Js.Unsafe.inject (Js.string "scheduledNotifications") |] in
      
      (* Calculate next reminder time (24 hours from now) *)
      let now_ms = Js.Unsafe.js_expr "Date.now()" |> Js.float_of_number in
      let tomorrow_ms = now_ms +. (24.0 *. 60.0 *. 60.0 *. 1000.0) in
      let tomorrow_date = Js.Unsafe.new_obj (Js.Unsafe.pure_js_expr "Date") 
        [| Js.Unsafe.inject (Js.number_of_float tomorrow_ms) |] in
      
      (* Data for scheduled notification *)
      let data = Js.Unsafe.obj [||] in
      let () = Js.Unsafe.set data "userId" (Js.string user_id) in
      let () = Js.Unsafe.set data "type" (Js.string "daily_reminder") in
      let () = Js.Unsafe.set data "title" (Js.string "Come back and play! 🎮") in
      let () = Js.Unsafe.set data "body" (Js.string "Your adventure awaits in Slay the Spire!") in
      let () = Js.Unsafe.set data "scheduledFor" tomorrow_date in
      let () = Js.Unsafe.set data "createdAt" (Js.Unsafe.js_expr "new Date()") in
      let () = Js.Unsafe.set data "processed" Js._false in
      
      (* Use user ID as document ID to ensure only one daily reminder per user *)
      let user_reminder_doc = Js.Unsafe.meth_call scheduled_ref "doc" 
        [| Js.Unsafe.inject (Js.string user_id) |] in
      
      (* Set document (overwrites if exists) *)
      let promise = Js.Unsafe.meth_call user_reminder_doc "set" [| Js.Unsafe.inject data |] in
      
      let on_success _ = 
        let () = Js.Unsafe.fun_call (Js.Unsafe.js_expr "console.log") [| Js.Unsafe.inject (Js.string "Daily reminder scheduled") |] in
        Ivar.fill ivar (Ok ())
      in
      
      let on_error error = 
        let error_msg = 
          try Js.Unsafe.get error "message" |> Js.to_string
          with _ -> "Failed to schedule reminder"
        in
        Ivar.fill ivar (Error error_msg)
      in
      
      let _ = Js.Unsafe.meth_call promise "then" [| Js.Unsafe.inject (Js.wrap_callback on_success) |] in
      let _ = Js.Unsafe.meth_call promise "catch" [| Js.Unsafe.inject (Js.wrap_callback on_error) |] in
      
      Ivar.read ivar
    with exn ->
      Ivar.fill ivar (Error (Exn.to_string exn));
      Ivar.read ivar
end

