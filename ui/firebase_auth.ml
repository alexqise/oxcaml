open! Core
open Async_kernel
open Js_of_ocaml

(* Firebase Authentication helpers *)
module Firebase_auth = struct
  (* Get the Firebase Auth instance from JavaScript *)
  let get_auth () =
    try
      (* Try to get from window.firebase.auth() first (our custom wrapper) *)
      let firebase_obj = Js.Unsafe.get Js.Unsafe.global "firebase" in
      let auth_func = Js.Unsafe.get firebase_obj "auth" in
      Js.Unsafe.fun_call auth_func [||]
    with _ ->
      try
        (* Fallback: try firebase namespace directly (compat library) *)
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
      let on_success user = Ivar.fill ivar (Ok user) in
      let on_error error = 
        let error_msg = 
          try
            Js.Unsafe.get error "message" |> Js.to_string
          with _ -> "Sign in failed"
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
      let on_success user = Ivar.fill ivar (Ok user) in
      let on_error error =
        let error_msg = 
          try
            Js.Unsafe.get error "message" |> Js.to_string
          with _ -> "Sign up failed"
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
      Js.Unsafe.get user "uid" |> Js.to_string
    with _ -> ""
  
  (* Get user email from user object *)
  let get_user_email user =
    try
      Js.Unsafe.get user "email" |> Js.to_string
    with _ -> ""
  
  (* Get ID token from current user *)
  let get_id_token () : (string, string) Result.t Deferred.t =
    let ivar = Ivar.create () in
    try
      match get_current_user () with
      | None -> 
        Ivar.fill ivar (Error "No user signed in");
        Ivar.read ivar
      | Some user ->
        let promise = Js.Unsafe.meth_call user "getIdToken" [||] in
        let on_success token = 
          let token_str = Js.to_string token in
          Ivar.fill ivar (Ok token_str)
        in
        let on_error error =
          let error_msg = 
            try
              Js.Unsafe.get error "message" |> Js.to_string
            with _ -> "Failed to get ID token"
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

