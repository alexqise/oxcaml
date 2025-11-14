open! Core
open Js_of_ocaml

(* Firebase configuration *)
let project_id = "coop-slaythespire"
let api_key = ""

(* Firebase Firestore REST API helpers *)
module Firebase_http = struct
  (* Make an HTTP POST request to Firebase Firestore *)
  let post_to_firestore ~collection ~document_id ~body_json =
    let open Async_kernel in
    let ivar = Ivar.create () in
    let url = sprintf 
      "https://firestore.googleapis.com/v1/projects/%s/databases/(default)/documents/%s?documentId=%s&key=%s"
      project_id collection document_id api_key
    in
    let xhr = XmlHttpRequest.create () in
    ignore (Js.Unsafe.meth_call xhr "open" [| Js.Unsafe.inject (Js.string "POST")
                                             ; Js.Unsafe.inject (Js.string url)
                                             ; Js.Unsafe.inject Js._true |]);
    ignore (Js.Unsafe.meth_call xhr "setRequestHeader" [| Js.Unsafe.inject (Js.string "Content-Type")
                                                         ; Js.Unsafe.inject (Js.string "application/json") |]);
    
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
    let open Async_kernel in
    let ivar = Ivar.create () in
    let url = sprintf 
      "https://firestore.googleapis.com/v1/projects/%s/databases/(default)/documents/%s/%s?key=%s"
      project_id collection document_id api_key
    in
    let xhr = XmlHttpRequest.create () in
    ignore (Js.Unsafe.meth_call xhr "open" [| Js.Unsafe.inject (Js.string "GET")
                                             ; Js.Unsafe.inject (Js.string url)
                                             ; Js.Unsafe.inject Js._true |]);
    
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
    let open Async_kernel in
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
      let all_params = ("key", api_key) :: List.map mask_params ~f:(fun param -> (param, "")) in
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

