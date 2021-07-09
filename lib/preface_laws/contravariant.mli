(** {1 Signature} *)

module type LAWS = sig
  module Contravariant : Preface_specs.CONTRAVARIANT

  val preserve_identity_morphisms :
    unit -> ('a Contravariant.t, 'a Contravariant.t) Law.t
  (** Generates the law: [contramap id = id]. *)

  val preserve_composition_of_morphisms :
       unit
    -> ('a -> 'b, ('b -> 'c) -> 'c Contravariant.t -> 'a Contravariant.t) Law.t
  (** Generates the law: [contramap (g % f) = (contramap f) % (contramap g)]. *)
end

(** {1 Building}

    Construct the set of functions verifying the laws for a given contravariant
    functor. *)

module For (C : Preface_specs.CONTRAVARIANT) :
  LAWS with module Contravariant := C
