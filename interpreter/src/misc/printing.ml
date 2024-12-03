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

module type FormatType = sig
  type t =
    | Bold
    | Underline
    | Italic
    | StrikeThrough
    | Inverse
    | Hidden
    | Default
    | None

  val format_code : t -> string
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

module NoFormat : FormatType = struct
  type t =
    | Bold
    | Underline
    | Italic
    | StrikeThrough
    | Inverse
    | Hidden
    | Default
    | None

  let format_code _ = ""
end

module ASNIFormat : FormatType = struct
  type t =
    | Bold
    | Underline
    | Italic
    | StrikeThrough
    | Inverse
    | Hidden
    | Default
    | None

  let format_code = function
    | Bold -> "\027[1m"
    | Underline -> "\027[4m"
    | Italic -> "\027[3m"
    | StrikeThrough -> "\027[9m"
    | Inverse -> "\027[7m"
    | Hidden -> "\027[8m"
    | Default -> "\027[0m"
    | None -> ""
end

module ASNIString (Color : ColorType) (Format : FormatType) = struct
  include String

  let colorize ?(color = Color.Default) ?(format = Format.None) text =
    let color_start, color_end =
      (Color.color_code color, Color.color_code Color.Default)
    in
    let format_start, format_end =
      (Format.format_code format, Format.format_code Format.Default)
    in
    Printf.sprintf "%s%s%s%s%s" color_start format_start text color_end
      format_end
end

module type Printer = sig
  module Color : ColorType

  module Format : FormatType

  val cprint : ?color:Color.t -> ?format:Format.t -> string -> unit

  val cprintln : ?color:Color.t -> ?format:Format.t -> string -> unit

  val cprintf :
    ?color:Color.t -> ?format:Format.t -> ('a, unit, string, unit) format4 -> 'a

  val eprint : string -> unit

  val eprintln : string -> unit

  val eprintf : ('a, unit, string, unit) format4 -> 'a
end

module MakePrinter (C : ColorType) (F : FormatType) : Printer = struct
  include Printf
  module Color = C
  module Format = F

  let cprint ?(color = Color.Default) ?(format = Format.None) text =
    let color_start, color_end =
      (Color.color_code color, Color.color_code Color.Default)
    in
    let format_start, format_end =
      (Format.format_code format, Format.format_code Format.Default)
    in
    Printf.printf "%s%s%s%s%s" color_start format_start text color_end
      format_end

  let cprintln ?(color = Color.Default) ?(format = Format.None) text =
    cprint ~color ~format (text ^ "\n")

  let cprintf ?(color = Color.Default) ?(format = Format.None) fmt =
    ksprintf (cprint ~color ~format) fmt

  let eprint text =
    let red = Color.Red in
    let bold = Format.Bold in
    cprint ~color:red ~format:bold text

  let eprintln text = eprint (text ^ "\n")

  let eprintf fmt = ksprintf eprint fmt
end

module type Logger = sig
  type log_t = Log | Error | Warn | Info | Debug | Success

  val enabled : bool ref

  val is_enabled : unit -> bool

  val enable : unit -> unit

  val disable : unit -> unit

  val set_logger_level : log_t -> unit

  val log : ?log_type:log_t -> ?indent:string -> string -> unit

  val error : string -> unit

  val warn : string -> unit

  val success : string -> unit

  val info : string -> unit

  val debug : string -> unit

  val group : string -> unit

  val end_group : unit -> unit
end

module MakeLogger (Color : ColorType) (Format : FormatType) : Logger = struct
  module P = MakePrinter (Color) (Format)
  module GroupStack = Stack

  type log_t = Log | Error | Warn | Info | Debug | Success

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
    | Success -> ("SUCCESS", 4, BrightGreen)
    | Error -> ("ERROR", 5, Red)
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
        P.cprint indent ;
        P.cprintf "[%s] " ~color:log_color ~format:Bold log_type_str ;
        P.cprintln text

  let error text = log ~log_type:Error text

  let warn text = log ~log_type:Warn text

  let success text = log ~log_type:Success text

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

module CPrinter = MakePrinter (ASNIColor) (ASNIFormat)
module Printer = MakePrinter (NoColor) (NoFormat)
module CString = ASNIString (ASNIColor) (ASNIFormat)
module String = ASNIString (NoColor) (NoFormat)
module Logger = MakeLogger (ASNIColor) (ASNIFormat)
