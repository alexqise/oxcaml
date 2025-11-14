open! Core
open Async_kernel
open Js_of_ocaml

(* Firebase Authentication helpers *)
module Firebase_auth = struct
  (* Simple JS console logger for debugging auth flows.
     This writes messages into the browser DevTools console. *)
  let log (msg : string) =
    try
      let console = Js.Unsafe.get Js.Unsafe.global "console" in
      ignore
        (Js.Unsafe.meth_call console "log"
           [| Js.Unsafe.inject (Js.string ("[Firebase_auth] " ^ msg)) |])
    with _ -> ()
  
  (* Get the Firebase Auth instance from JavaScript.
     We expect window.firebaseAuth to be set by index.html after initialization. *)
  let get_auth () =
    try
      (* Primary: get from window.firebaseAuth (set by index.html) *)
      Js.Unsafe.get Js.Unsafe.global "firebaseAuth"
    with _ ->
      try
        (* Fallback: call firebase.auth() directly *)
        let firebase = Js.Unsafe.get Js.Unsafe.global "firebase" in
        let auth_func = Js.Unsafe.get firebase "auth" in
        Js.Unsafe.fun_call auth_func [||]
      with _ ->
        (* Last resort: return empty object *)
        Js.Unsafe.obj [||]
  
  (* Get current user *)
  let get_current_user () =
    try
      let auth = get_auth () in
      let current_user = Js.Unsafe.get auth "currentUser" in
      if Js.Opt.test current_user then
        Some (Js.Opt.get current_user (fun () -> assert false))
      else
        None
    with _ -> None
  
  (* Sign in with Google using Firebase Auth and a popup *)
  let sign_in_with_google () =
    (* This function mirrors [sign_in_with_email_password] but uses
       Firebase's GoogleAuthProvider and [signInWithPopup]. It wraps the
       JavaScript Promise into an Async [Deferred] using an [Ivar]. *)
    let ivar = Ivar.create () in
    log "sign_in_with_google called";
    try
      (* Get the shared Auth instance from our JS helper *)
      let auth = get_auth () in
      (* Access the Firebase namespace on window to construct Google provider.
         In the compat SDK, GoogleAuthProvider is a constructor at
         firebase.auth.GoogleAuthProvider (not on the auth instance). *)
      let firebase = Js.Unsafe.get Js.Unsafe.global "firebase" in
      let auth_namespace = Js.Unsafe.get firebase "auth" in
      let provider_ctor = Js.Unsafe.get auth_namespace "GoogleAuthProvider" in
      log (sprintf "GoogleAuthProvider type: %s" 
        (Js.to_string (Js.typeof provider_ctor)));
      (* Create a new GoogleAuthProvider instance *)
      let provider = Js.Unsafe.new_obj provider_ctor [||] in
      (* Call signInWithPopup(auth, provider) and handle the resulting Promise *)
      let promise =
        Js.Unsafe.meth_call auth "signInWithPopup"
          [| Js.Unsafe.inject provider |]
      in
      (* On success, Firebase returns a UserCredential with a .user property.
         We need to extract the actual user object from the credential. *)
      let on_success credential =
        log "Google sign-in success - extracting user from credential";
        try
          (* Extract the user from credential.user *)
          let user = Js.Unsafe.get credential "user" in
          log "Extracted user from credential";
          Ivar.fill ivar (Ok user)
        with exn ->
          let msg = sprintf "Failed to extract user from credential: %s" (Exn.to_string exn) in
          log msg;
          Ivar.fill ivar (Error msg)
      in
      let on_error error =
        (* Try to extract a helpful error message from the JS error object. *)
        let error_msg =
          try Js.Unsafe.get error "message" |> Js.to_string
          with _ -> "Google sign in failed"
        in
        log (sprintf "Google sign-in error: %s" error_msg);
        Ivar.fill ivar (Error error_msg)
      in
      ignore
        (Js.Unsafe.meth_call promise "then"
           [| Js.Unsafe.inject (Js.wrap_callback on_success) |]);
      ignore
        (Js.Unsafe.meth_call promise "catch"
           [| Js.Unsafe.inject (Js.wrap_callback on_error) |]);
      Ivar.read ivar
    with exn ->
      (* In case of unexpected JS errors, surface them as a string. *)
      let msg = sprintf "Google auth exception: %s" (Exn.to_string exn) in
      log msg;
      Ivar.fill ivar (Error msg);
      Ivar.read ivar
  
  (* Sign in with email and password *)
  let sign_in_with_email_password ~email ~password =
    let ivar = Ivar.create () in
    try
      let auth = get_auth () in
      let email_js = Js.string email in
      let password_js = Js.string password in
      let promise = Js.Unsafe.meth_call auth "signInWithEmailAndPassword"
        [| Js.Unsafe.inject email_js
         ; Js.Unsafe.inject password_js
        |] in
      (* Firebase returns a UserCredential, we need to extract the user from credential.user *)
      let on_success credential =
        log "Email/password sign-in success - extracting user from credential";
        try
          (* Extract the user from credential.user *)
          let user = Js.Unsafe.get credential "user" in
          log "Extracted user from credential";
          Ivar.fill ivar (Ok user)
        with exn ->
          let msg = sprintf "Failed to extract user from credential: %s" (Exn.to_string exn) in
          log msg;
          Ivar.fill ivar (Error msg)
      in
      let on_error error = 
        let error_msg = 
          try
            Js.Unsafe.get error "message" |> Js.to_string
          with _ -> "Sign in failed"
        in
        log (sprintf "Email/password sign-in error: %s" error_msg);
        Ivar.fill ivar (Error error_msg)
      in
      ignore (Js.Unsafe.meth_call promise "then" 
        [| Js.Unsafe.inject (Js.wrap_callback on_success) |]);
      ignore (Js.Unsafe.meth_call promise "catch"
        [| Js.Unsafe.inject (Js.wrap_callback on_error) |]);
      Ivar.read ivar
    with exn ->
      Ivar.fill ivar (Error (sprintf "Auth error: %s" (Exn.to_string exn)));
      Ivar.read ivar
  
  (* Sign up with email and password *)
  let create_user_with_email_password ~email ~password =
    let ivar = Ivar.create () in
    try
      let auth = get_auth () in
      let email_js = Js.string email in
      let password_js = Js.string password in
      let promise = Js.Unsafe.meth_call auth "createUserWithEmailAndPassword"
        [| Js.Unsafe.inject email_js
         ; Js.Unsafe.inject password_js
        |] in
      (* Firebase returns a UserCredential, we need to extract the user from credential.user *)
      let on_success credential =
        log "Email/password sign-up success - extracting user from credential";
        try
          (* Extract the user from credential.user *)
          let user = Js.Unsafe.get credential "user" in
          log "Extracted user from credential";
          Ivar.fill ivar (Ok user)
        with exn ->
          let msg = sprintf "Failed to extract user from credential: %s" (Exn.to_string exn) in
          log msg;
          Ivar.fill ivar (Error msg)
      in
      let on_error error =
        let error_msg = 
          try
            Js.Unsafe.get error "message" |> Js.to_string
          with _ -> "Sign up failed"
        in
        log (sprintf "Email/password sign-up error: %s" error_msg);
        Ivar.fill ivar (Error error_msg)
      in
      ignore (Js.Unsafe.meth_call promise "then"
        [| Js.Unsafe.inject (Js.wrap_callback on_success) |]);
      ignore (Js.Unsafe.meth_call promise "catch"
        [| Js.Unsafe.inject (Js.wrap_callback on_error) |]);
      Ivar.read ivar
    with exn ->
      Ivar.fill ivar (Error (sprintf "Auth error: %s" (Exn.to_string exn)));
      Ivar.read ivar
  
  (* Sign out *)
  let sign_out () =
    let ivar = Ivar.create () in
    try
      let auth = get_auth () in
      let promise = Js.Unsafe.meth_call auth "signOut" [||] in
      let on_success _ = Ivar.fill ivar (Ok ()) in
      let on_error error =
        let error_msg = 
          try
            Js.Unsafe.get error "message" |> Js.to_string
          with _ -> "Sign out failed"
        in
        Ivar.fill ivar (Error error_msg)
      in
      ignore (Js.Unsafe.meth_call promise "then"
        [| Js.Unsafe.inject (Js.wrap_callback on_success) |]);
      ignore (Js.Unsafe.meth_call promise "catch"
        [| Js.Unsafe.inject (Js.wrap_callback on_error) |]);
      Ivar.read ivar
    with exn ->
      Ivar.fill ivar (Error (sprintf "Auth error: %s" (Exn.to_string exn)));
      Ivar.read ivar
  
  (* Get user ID from user object *)
  let get_user_id user =
    try
      let uid = Js.Unsafe.get user "uid" |> Js.to_string in
      (* Validate uid is not empty *)
      if String.is_empty uid then
        let () = log "Warning: user object has empty uid property" in
        ""
      else
        uid
    with exn ->
      let () = log (sprintf "Error getting user ID: %s" (Exn.to_string exn)) in
      ""
  
  (* Get user email from user object *)
  let get_user_email user =
    try
      Js.Unsafe.get user "email" |> Js.to_string
    with _ -> ""
  
  (* Get user display name from user object (Google Auth provides this) *)
  let get_user_display_name user =
    try
      let display_name = Js.Unsafe.get user "displayName" in
      if Js.Opt.test display_name then
        Some (Js.Opt.get display_name (fun () -> Js.string "") |> Js.to_string)
      else
        None
    with _ -> None
  
  (* Get user photo URL from user object (Google Auth provides this) *)
  let get_user_photo_url user =
    try
      let photo_url = Js.Unsafe.get user "photoURL" in
      if Js.Opt.test photo_url then
        Some (Js.Opt.get photo_url (fun () -> Js.string "") |> Js.to_string)
      else
        None
    with _ -> None
  
  (* Listen to auth state changes *)
  let on_auth_state_changed callback =
    try
      let auth = get_auth () in
      let wrapped_callback user =
        if Js.Opt.test user then
          callback (Some (Js.Opt.get user (fun () -> assert false)))
        else
          callback None
      in
      ignore (Js.Unsafe.meth_call auth "onAuthStateChanged"
        [| Js.Unsafe.inject (Js.wrap_callback wrapped_callback)
        |])
    with _ -> ()
end

