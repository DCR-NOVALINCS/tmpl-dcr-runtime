open State
open Errors
open Common
open Monads.ResultMonad
open Cmdliner

(** [num] yields the number of times to rollback in the program.
    @return a argument of the number of times to rollback *)
let num =
  Arg.(
    value & opt int 1
    & info ["n"; "num"] ~docv:"NUM" ~doc:"Number of time to rollback" )

(** [rollback_cmd] command to rollback a number of times in the program.
    @param num the number of times to rollback
    @param state the current state
    @return the new state with the output *)
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
