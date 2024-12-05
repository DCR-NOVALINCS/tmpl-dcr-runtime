module type Observer = sig
  type 'a t

  val create : ('a -> unit) -> 'a t

  val notify : 'a t -> 'a -> unit
end

module type Subject = functor (O : Observer) -> sig
  type 'a t

  val create : 'a -> 'a t

  val subscribe : 'a t -> 'a O.t -> unit

  val notify : 'a t -> unit

  val set_state : 'a t -> 'a -> unit

  val get_state : 'a t -> 'a
end

module MakeSubject : Subject =
functor
  (O : Observer)
  ->
  struct
    type 'a t = {mutable state: 'a; mutable observers: 'a O.t list}

    let create initial_state = {state= initial_state; observers= []}

    let subscribe subject observer =
      subject.observers <- observer :: subject.observers

    let notify subject =
      List.iter
        (fun observer -> O.notify observer subject.state)
        subject.observers

    let set_state subject new_state =
      subject.state <- new_state ;
      notify subject

    let get_state subject = subject.state
  end

module MakeObserver : Observer = struct
  type 'a t = 'a -> unit

  let create f = f

  let notify observer state = observer state
end
