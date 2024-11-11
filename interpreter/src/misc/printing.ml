module type ColorType = sig
  type t =
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
    | Default
    | Black
    | BrightRed
    | BrightGreen
    | BrightYellow
    | BrightBlue
    | BrightMagenta
    | BrightCyan
    | BrightWhite

  val color_code : t -> string
end

module NoColor : ColorType = struct
  type t =
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
    | Default
    | Black
    | BrightRed
    | BrightGreen
    | BrightYellow
    | BrightBlue
    | BrightMagenta
    | BrightCyan
    | BrightWhite

  let color_code _ = ""
end

module ASNIColor : ColorType = struct
  type t =
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
    | Default
    | Black
    | BrightRed
    | BrightGreen
    | BrightYellow
    | BrightBlue
    | BrightMagenta
    | BrightCyan
    | BrightWhite

  let color_code = function
    | Red -> "\027[31m"
    | Green -> "\027[32m"
    | Yellow -> "\027[33m"
    | Blue -> "\027[34m"
    | Magenta -> "\027[35m"
    | Cyan -> "\027[36m"
    | White -> "\027[37m"
    | Default -> "\027[0m"
    | Black -> "\027[30m"
    | BrightRed -> "\027[91m"
    | BrightGreen -> "\027[92m"
    | BrightYellow -> "\027[93m"
    | BrightBlue -> "\027[94m"
    | BrightMagenta -> "\027[95m"
    | BrightCyan -> "\027[96m"
    | BrightWhite -> "\027[97m"
end

module ASNIString (Color : ColorType) = struct
  include String

  let colorize ?(color = Color.Default) text =
    let color_start = Color.color_code color in
    let color_end = Color.color_code Color.Default in
    Printf.sprintf "%s%s%s" color_start text color_end
end

module type Printer = sig
  module Color : ColorType

  val cprint : ?color:Color.t -> string -> unit

  val cprintln : ?color:Color.t -> string -> unit

  val cprintf : ?color:Color.t -> ('a, unit, string, unit) format4 -> 'a

  val eprint : string -> unit

  val eprintln : string -> unit

  val eprintf : ('a, unit, string, unit) format4 -> 'a
end

module MakePrinter (C : ColorType) : Printer = struct
  include Printf
  module Color = C

  let cprint ?(color = Color.Default) text =
    let color_start = Color.color_code color in
    let color_end = Color.color_code Color.Default in
    printf "%s%s%s" color_start text color_end

  let cprintln ?(color = Color.Default) text = cprint ~color (text ^ "\n")

  let cprintf ?(color = Color.Default) fmt = ksprintf (cprint ~color) fmt

  let eprint text =
    let red = Color.Red in
    cprint ~color:red text

  let eprintln text = eprint (text ^ "\n")

  let eprintf fmt = ksprintf eprint fmt
end

module type Logger = sig
  type log_t = Log | Error | Warn | Info | Debug

  val enabled : bool ref

  val is_enabled : unit -> bool

  val enable : unit -> unit

  val disable : unit -> unit

  val set_logger_level : log_t -> unit

  val log : ?log_type:log_t -> ?indent:string -> string -> unit

  val error : string -> unit

  val warn : string -> unit

  val info : string -> unit

  val debug : string -> unit

  val group : string -> unit

  val end_group : unit -> unit
end

module MakeLogger (Color : ColorType) : Logger = struct
  module P = MakePrinter (Color)
  module GroupStack = Stack

  type log_t = Log | Error | Warn | Info | Debug

  let group_stack = GroupStack.create ()

  let enabled = ref false

  let is_enabled () = !enabled

  let enable () = enabled := true

  let disable () = enabled := false

  let convert_log_type =
    let open P.Color in
    function
    | Debug -> ("DEBUG", 1, Blue)
    | Info -> ("INFO", 2, Cyan)
    | Warn -> ("WARN", 3, Yellow)
    | Error -> ("ERROR", 4, Red)
    | Log -> ("LOG", 99, Default)

  let logger_level =
    let _, log_level, _ = convert_log_type Debug in
    ref log_level

  let set_logger_level level =
    let _, log_level, _ = convert_log_type level in
    logger_level := log_level

  (*"│ "*)
  let log ?(log_type = Log) ?(indent = "") text =
    if not @@ is_enabled () then ()
    else
      let log_type_str, log_level, log_color = convert_log_type log_type in
      if log_level < !logger_level then ()
      else
        let group_size = GroupStack.length group_stack in
        let indent = if group_size > 0 then "│ " ^ indent else indent in
        P.cprint ~color:Cyan indent ;
        P.cprintf "[%s]: " ~color:log_color log_type_str ;
        P.cprintln text

  let error text = log ~log_type:Error text

  let warn text = log ~log_type:Warn text

  let info text = log ~log_type:Info text

  let debug text = log ~log_type:Debug text

  let group text =
    if is_enabled () then (
      GroupStack.push text group_stack ;
      P.cprint ~color:Cyan "┌ " ;
      P.cprint ~color:Green text ;
      P.cprintln ~color:Cyan " ─" )

  let end_group () =
    if is_enabled () then
      let open P.Color in
      match GroupStack.pop_opt group_stack with
      | None -> ()
      | Some text ->
          let indent =
            String.concat "" (List.init (String.length text + 3) (fun _ -> "─"))
          in
          P.cprintf ~color:Cyan "└" ;
          P.cprintln ~color:Cyan indent
end

module CPrinter = MakePrinter (ASNIColor)
module Printer = MakePrinter (NoColor)
module CString = ASNIString (ASNIColor)
module String = ASNIString (NoColor)
module Logger = MakeLogger (ASNIColor)
