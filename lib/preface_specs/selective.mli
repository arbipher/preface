(** A [Selective] (applicative functor) allows to declare effects statically and
    select which execute dynamically. It is an algebraic structure between
    [Applicative] and [Monad]. *)

(** {1 Structure anatomy} *)

module type CORE_WITH_SELECT = sig
  type 'a t

  type ('a, 'b) either

  val pure : 'a -> 'a t

  val select : ('a, 'b) either t -> ('a -> 'b) t -> 'b t
end

(** Standard requirement. *)
module type CORE = sig
  include CORE_WITH_SELECT

  include Applicative.CORE with type 'a t := 'a t
end

(** Operations. *)
module type OPERATION = sig
  type 'a t

  type ('a, 'b) either

  include Applicative.OPERATION with type 'a t := 'a t

  val branch : ('a, 'b) either t -> ('a -> 'c) t -> ('b -> 'c) t -> 'c t

  val if_ : bool t -> 'a t -> 'a t -> 'a t

  val when_ : bool t -> unit t -> unit t

  val while_ : bool t -> unit t
end

(** Syntax extensions. *)
module type SYNTAX = sig
  include Applicative.SYNTAX
end

(** Infix notations. *)
module type INFIX = sig
  type 'a t

  type ('a, 'b) either

  include Applicative.INFIX with type 'a t := 'a t

  val ( <?* ) : ('a, 'b) either t -> ('a -> 'b) t -> 'b t

  val ( *?> ) : ('a -> 'b) t -> ('a, 'b) either t -> 'b t

  val ( <||> ) : bool t -> bool t -> bool t

  val ( <&&> ) : bool t -> bool t -> bool t
end

(** {1 API} *)

(** The complete interface of a [Monad]. *)
module type API = sig
  include CORE

  include
    OPERATION with type 'a t := 'a t and type ('a, 'b) either := ('a, 'b) either

  module Syntax : SYNTAX with type 'a t := 'a t

  include module type of Syntax

  module Infix : INFIX with type 'a t := 'a t

  include module type of Infix with type ('a, 'b) either := ('a, 'b) either
end

(** {1 Bibliography}

    - {{:http://hackage.haskell.org/package/selective} Haskell's documentation
      of a Selective Application Functor}
    - {{:https://www.staff.ncl.ac.uk/andrey.mokhov/selective-functors.pdf}
      Selective Applicative Functors} *)
