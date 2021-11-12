open Core

(* $MDX part-begin=applicative_demo *)
module Host_and_port = struct
  type t = Host_and_port.t =
    { host : string
             [@command.custom
               let open Command.Param in
               flag "host" (required string) ~doc:"host"]
    ; port : int
             [@command.custom
               let open Command.Param in
               flag "port" (required int) ~doc:"port"]
    }
  [@@deriving command]
end

type t =
  { host_and_port : Host_and_port.t
  ; name : string option
           [@command.custom
             let open Command.Param in
             flag "name" (optional string) ~doc:"name"]
  }
[@@deriving command]

(* $MDX part-end *)

let%expect_test "variants_aren't_supported" =
  let open Expect_test_helpers_core in
  show_raise (fun () ->
    let module _ = struct
      type t =
        | A
        | B
      [@@deriving command]
    end
    in
    ());
  [%expect {| (raised (Failure "Cannot support variants without either")) |}]
;;
