open! Core
open Js_of_ocaml

(* Firebase configuration *)
let project_id = "coop-slaythespire"
let api_key = "AIzaSyAMNg5zUEAiwLU4TDTlLhGm3Pqp032se5Q"

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
  let patch_firestore ~collection ~document_id ~body_json =
    let open Async_kernel in
    let ivar = Ivar.create () in
    let url = sprintf 
      "https://firestore.googleapis.com/v1/projects/%s/databases/(default)/documents/%s/%s?key=%s"
      project_id collection document_id api_key
    in
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

