(** {1 Signature} *)

module type LAWS = sig
  module Alt : Preface_specs.ALT

  include Functor.LAWS with module Functor := Alt
  (** @closed *)

  val associative_combine :
    unit -> ('a Alt.t, 'a Alt.t -> 'a Alt.t -> 'a Alt.t) Law.t
  (** Generates the law: [(a <|> b) <|> c = a <|> (b <|> c)]. *)

  val left_distributive_map_over_combine :
    unit -> ('a -> 'b, 'a Alt.t -> 'a Alt.t -> 'b Alt.t) Law.t
  (** Generates the law: [f <$> (a <|> b) = (f <$> a) <|> (f <$> b)]. *)
end

(** {1 Building}

    Construct the set of functions verifying the laws for a given alt. *)

module For (A : Preface_specs.ALT) : LAWS with module Alt := A
