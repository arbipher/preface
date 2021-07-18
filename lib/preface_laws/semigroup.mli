(** {1 Signature} *)

module type LAWS = sig
  module Semigroup : Preface_specs.SEMIGROUP

  val semigroup_associative_combine :
    unit -> (Semigroup.t, Semigroup.t -> Semigroup.t -> Semigroup.t) Law.t
  (** Generates the law: [(a <|> b) <|> c = a <|> (b <|> c)]. *)
end

(** {1 Building}

    Construct the set of functions verifying the laws for a given semigroup. *)

module For (S : Preface_specs.SEMIGROUP) : LAWS with module Semigroup := S
