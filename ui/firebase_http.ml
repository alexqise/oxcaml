open! Core
open Async_kernel
open Js_of_ocaml

(* Firebase configuration *)
let project_id = "coop-slaythespire"
let api_key = ""

(* Firebase Firestore REST API helpers *)
module Firebase_http = struct
  (* Get Firebase Auth ID token for current user *)
  let get_auth_token () : string option Deferred.t =
    let ivar = Ivar.create () in
    try
      (* Get auth instance - we need to access Firebase_auth module *)
      (* Import the get_auth and get_current_user functions *)
      let auth = 
        try
          Js.Unsafe.get Js.Unsafe.global "firebaseAuth"
        with _ ->
          try
            let firebase = Js.Unsafe.get Js.Unsafe.global "firebase" in
            let auth_func = Js.Unsafe.get firebase "auth" in
            Js.Unsafe.fun_call auth_func [||]
          with _ ->
            Js.Unsafe.obj [||]
      in
      let current_user = Js.Unsafe.get auth "currentUser" in
      if Js.Opt.test current_user then
        let user = Js.Opt.get current_user (fun () -> assert false) in
        (* Call getIdToken() on the user object *)
        let promise = Js.Unsafe.meth_call user "getIdToken" [||] in
        let on_success token = 
          let token_str = Js.to_string token in
          Ivar.fill ivar (Some token_str)
        in
        let on_error _ = Ivar.fill ivar None in
        ignore (Js.Unsafe.meth_call promise "then" [| Js.Unsafe.inject (Js.wrap_callback on_success) |]);
        ignore (Js.Unsafe.meth_call promise "catch" [| Js.Unsafe.inject (Js.wrap_callback on_error) |]);
        Ivar.read ivar
      else
        let () = Ivar.fill ivar None in
        Ivar.read ivar
    with _ ->
      let () = Ivar.fill ivar None in
      Ivar.read ivar
  (* Make an HTTP POST request to Firebase Firestore *)
  let post_to_firestore ~collection ~document_id ~body_json =
    let%bind auth_token_opt = get_auth_token () in
    let ivar = Ivar.create () in
    (* Build URL - use access_token if available, otherwise fall back to API key *)
    let url = match auth_token_opt with
      | Some token -> 
        sprintf 
          "https://firestore.googleapis.com/v1/projects/%s/databases/(default)/documents/%s?documentId=%s&access_token=%s"
          project_id collection document_id token
      | None ->
        sprintf 
          "https://firestore.googleapis.com/v1/projects/%s/databases/(default)/documents/%s?documentId=%s&key=%s"
          project_id collection document_id api_key
    in
    let xhr = XmlHttpRequest.create () in
    ignore (Js.Unsafe.meth_call xhr "open" [| Js.Unsafe.inject (Js.string "POST")
                                             ; Js.Unsafe.inject (Js.string url)
                                             ; Js.Unsafe.inject Js._true |]);
    ignore (Js.Unsafe.meth_call xhr "setRequestHeader" [| Js.Unsafe.inject (Js.string "Content-Type")
                                                         ; Js.Unsafe.inject (Js.string "application/json") |]);
    (* Add Authorization header if we have a token (more secure than query param) *)
    (match auth_token_opt with
    | Some token -> 
      ignore (Js.Unsafe.meth_call xhr "setRequestHeader" 
        [| Js.Unsafe.inject (Js.string "Authorization")
         ; Js.Unsafe.inject (Js.string ("Bearer " ^ token)) |])
    | None -> ());
    
    Js.Unsafe.set xhr "onreadystatechange" (Js.wrap_callback (fun _ ->
      let ready_state = Js.Unsafe.get xhr "readyState" |> Js.to_int32 |> Int32.to_int_exn in
      if ready_state = 4 then ( (* XmlHttpRequest.DONE = 4 *)
        let status = Js.Unsafe.get xhr "status" |> Js.to_int32 |> Int32.to_int_exn in
        if status >= 200 && status < 300 then (
          let body = Js.Unsafe.get xhr "responseText" |> Js.to_string in
          Ivar.fill ivar (Ok body)
        ) else (
          let response_text = Js.Unsafe.get xhr "responseText" |> Js.to_string in
          let error_msg = sprintf "HTTP %d: %s" status response_text in
          Ivar.fill ivar (Error error_msg)
        )
      )
    ));
    
    ignore (Js.Unsafe.meth_call xhr "send" [| Js.Unsafe.inject (Js.Opt.return (Js.string body_json)) |]);
    Ivar.read ivar
  
  (* Make an HTTP GET request to Firebase Firestore *)
  let get_from_firestore ~collection ~document_id =
    let%bind auth_token_opt = get_auth_token () in
    let ivar = Ivar.create () in
    (* Build URL - use access_token if available, otherwise fall back to API key *)
    let url = match auth_token_opt with
      | Some token -> 
        sprintf 
          "https://firestore.googleapis.com/v1/projects/%s/databases/(default)/documents/%s/%s?access_token=%s"
          project_id collection document_id token
      | None ->
        sprintf 
          "https://firestore.googleapis.com/v1/projects/%s/databases/(default)/documents/%s/%s?key=%s"
          project_id collection document_id api_key
    in
    let xhr = XmlHttpRequest.create () in
    ignore (Js.Unsafe.meth_call xhr "open" [| Js.Unsafe.inject (Js.string "GET")
                                             ; Js.Unsafe.inject (Js.string url)
                                             ; Js.Unsafe.inject Js._true |]);
    (* Add Authorization header if we have a token *)
    (match auth_token_opt with
    | Some token -> 
      ignore (Js.Unsafe.meth_call xhr "setRequestHeader" 
        [| Js.Unsafe.inject (Js.string "Authorization")
         ; Js.Unsafe.inject (Js.string ("Bearer " ^ token)) |])
    | None -> ());
    
    Js.Unsafe.set xhr "onreadystatechange" (Js.wrap_callback (fun _ ->
      let ready_state = Js.Unsafe.get xhr "readyState" |> Js.to_int32 |> Int32.to_int_exn in
      if ready_state = 4 then ( (* XmlHttpRequest.DONE = 4 *)
        let status = Js.Unsafe.get xhr "status" |> Js.to_int32 |> Int32.to_int_exn in
        if status >= 200 && status < 300 then (
          let body = Js.Unsafe.get xhr "responseText" |> Js.to_string in
          Ivar.fill ivar (Ok body)
        ) else (
          let response_text = Js.Unsafe.get xhr "responseText" |> Js.to_string in
          let error_msg = sprintf "HTTP %d: %s" status response_text in
          Ivar.fill ivar (Error error_msg)
        )
      )
    ));
    
    ignore (Js.Unsafe.meth_call xhr "send" [| Js.Unsafe.inject Js.Opt.empty |]);
    Ivar.read ivar
  
  (* Make an HTTP PATCH request to update a document *)
  let patch_firestore ~collection ~document_id ~body_json ?(update_mask = []) () =
    let%bind auth_token_opt = get_auth_token () in
    let ivar = Ivar.create () in
    let base_url =
      sprintf
        "https://firestore.googleapis.com/v1/projects/%s/databases/(default)/documents/%s/%s"
        project_id collection document_id
    in
    let query_params =
      let mask_params =
        List.map update_mask ~f:(fun field ->
            sprintf "updateMask.fieldPaths=%s" field)
      in
      (* Use access_token if available, otherwise use API key *)
      let auth_param = match auth_token_opt with
        | Some token -> ("access_token", token)
        | None -> ("key", api_key)
      in
      let all_params = auth_param :: List.map mask_params ~f:(fun param -> (param, "")) in
      let encoded =
        all_params
        |> List.map ~f:(fun (k, v) ->
               if String.is_empty v then k else sprintf "%s=%s" k v)
        |> String.concat ~sep:"&"
      in
      if String.is_empty encoded then ""
      else sprintf "?%s" encoded
    in
    let url = base_url ^ query_params in
    let xhr = XmlHttpRequest.create () in
    ignore (Js.Unsafe.meth_call xhr "open" [| Js.Unsafe.inject (Js.string "PATCH")
                                             ; Js.Unsafe.inject (Js.string url)
                                             ; Js.Unsafe.inject Js._true |]);
    ignore (Js.Unsafe.meth_call xhr "setRequestHeader" [| Js.Unsafe.inject (Js.string "Content-Type")
                                                          ; Js.Unsafe.inject (Js.string "application/json") |]);
    (* Add Authorization header if we have a token *)
    (match auth_token_opt with
    | Some token -> 
      ignore (Js.Unsafe.meth_call xhr "setRequestHeader" 
        [| Js.Unsafe.inject (Js.string "Authorization")
         ; Js.Unsafe.inject (Js.string ("Bearer " ^ token)) |])
    | None -> ());
    
    Js.Unsafe.set xhr "onreadystatechange" (Js.wrap_callback (fun _ ->
      let ready_state = Js.Unsafe.get xhr "readyState" |> Js.to_int32 |> Int32.to_int_exn in
      if ready_state = 4 then ( (* XmlHttpRequest.DONE = 4 *)
        let status = Js.Unsafe.get xhr "status" |> Js.to_int32 |> Int32.to_int_exn in
        if status >= 200 && status < 300 then (
          let body = Js.Unsafe.get xhr "responseText" |> Js.to_string in
          Ivar.fill ivar (Ok body)
        ) else (
          let response_text = Js.Unsafe.get xhr "responseText" |> Js.to_string in
          let error_msg = sprintf "HTTP %d: %s" status response_text in
          Ivar.fill ivar (Error error_msg)
        )
      )
    ));
    
    ignore (Js.Unsafe.meth_call xhr "send" [| Js.Unsafe.inject (Js.Opt.return (Js.string body_json)) |]);
    Ivar.read ivar
end

