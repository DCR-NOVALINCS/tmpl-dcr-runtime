val execute : event_id:string -> ?expr:Syntax.expr -> Syntax.program -> (Syntax.program, string) result

val view : ?filter:((string * Syntax.event) list list * (string * Syntax.expr) list list -> Syntax.event -> bool) -> Syntax.program -> (string, string) result

val view_enabled : Syntax.program -> (string, string) result