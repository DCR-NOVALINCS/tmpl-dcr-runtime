val sanitize_input : string -> (string list, 'a) result
(** [sanitize_input input] takes a string [input] and processes it to produce a
    sanitized list of strings. Returns a [result] type where the success case
    contains the sanitized list of strings, and the error case contains a value
    of type ['a]. *)

val is_empty_string : ?map:(string -> string) -> string -> (string, 'a) result
(** [is_empty_string ?map str] checks if the given string [str] is empty.
    Optionally applies a mapping function [map] to the string before checking.
    Returns a [result] type where the success case contains the processed
    string, and the error case contains a value of type ['a]. *)

val id : 'a -> 'b
(** [id x] is the identity function that returns its input [x]. This function is
    polymorphic and can accept any type ['a], but it returns a value of type
    ['b]. *)
