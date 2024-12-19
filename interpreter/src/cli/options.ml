open Common
open Monads.ResultMonad
open Printing
open Errors

type options = {logger_level: string option}

let set_logger logger_level =
  if Option.is_none logger_level then return (Logger.disable ())
  else (
    Logger.enable () ;
    let logger_level = Option.get logger_level in
    match String.trim logger_level with
    | "debug" -> return (Logger.set_logger_level Debug)
    | "info" -> return (Logger.set_logger_level Info)
    | "warn" -> return (Logger.set_logger_level Warn)
    | "error" -> return (Logger.set_logger_level Error)
    | "success" -> return (Logger.set_logger_level Success)
    | _ -> invalid_logger_level logger_level )
