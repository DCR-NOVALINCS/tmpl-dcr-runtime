open State
open Errors
open Common
open Monads.ResultMonad

(* open Printing *)
open Cmdliner

let num =
  Arg.(
    value & opt int 1
    & info ["n"; "num"] ~docv:"NUM" ~doc:"Number of time to rollback" )

let rollback_cmd num state =
  let rec rollback_n n state =
    let {previous_state; _} = state in
    match previous_state with
    | None -> state
    | Some prev when n <= 0 -> prev
    | Some prev -> rollback_n (n - 1) prev
  in
  if num <= 0 then invalid_number_rollback num
  else return {(rollback_n num state) with output= "Successfully rolled back."}

let term = Term.(const rollback_cmd $ num)
