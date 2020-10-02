# Websocket client

## Example

    let main uri =
      let module WS = Websocket_client in

      let process_message = function
        | WS.Response.End -> prerr_endline "<< END"
        | Error error -> Printf.eprintf "<< ERROR: %s\n%!" error
        | Text data -> Printf.eprintf "<< TEXT: %s\n%!" data
        | Binary data -> Printf.eprintf "<< BINARY: %s\n%!" data in

      let websocket_proc event =
        process_message event;
        Lwt.return_unit in

      match%lwt WS.create uri websocket_proc with
      | Ok channel ->
          WS.send_text channel "Ku!";
          let%lwt () = Lwt_unix.sleep 2. in

          (* fragmented message *)
          let t = WS.Partial.Text.start channel "One" in
          WS.Partial.Text.next t " Two";
          WS.Partial.Text.next t " Three";
          WS.Partial.Text.finish t " Four";
          let%lwt () = Lwt_unix.sleep 2. in
          WS.close channel;
          let%lwt () = Lwt_unix.sleep 2. in
          Lwt.return_unit
      | Error error ->
          Printf.eprintf "Connection error: %s\n%!" error;
          Lwt.return_unit

    let () = main "ws://...." |> Lwt_main.run
