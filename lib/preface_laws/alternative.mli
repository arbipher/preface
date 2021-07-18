(** {1 Signature} *)

module type LAWS = sig
  module Alternative : Preface_specs.ALTERNATIVE

  include Applicative.LAWS with module Applicative := Alternative
  (** @closed *)

  val alternative_apply_is_right_distributive :
       unit
    -> ( ('a -> 'b) Alternative.t
       , ('a -> 'b) Alternative.t -> 'a Alternative.t -> 'b Alternative.t )
       Law.t
  (** Generates the law: [(f <|> g) <*> x = (f <*> x) <|> (g <*> x)]. *)

  val alternative_right_absorbtion_for_apply :
    unit -> ('a Alternative.t, 'a Alternative.t) Law.t
  (** Generates the law: [neutral <*> a = neutral]. *)

  val alternative_left_identity :
    unit -> ('a Alternative.t, 'a Alternative.t) Law.t
  (** Generates the law: [neutral <|> x = x]. *)

  val alternative_right_identity :
    unit -> ('a Alternative.t, 'a Alternative.t) Law.t
  (** Generates the law: [x <|> neutral = x]. *)

  val alternative_combine_is_associative :
       unit
    -> ( 'a Alternative.t
       , 'a Alternative.t -> 'a Alternative.t -> 'a Alternative.t )
       Law.t
  (** Generates the law: [x <|> (y <|> z) = (x <|> y) <|> z]. *)
end

(** {1 Building}

    Construct the set of functions verifying the laws for a given alternative. *)

module For (A : Preface_specs.ALTERNATIVE) : LAWS with module Alternative := A
