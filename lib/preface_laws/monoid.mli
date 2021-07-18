(** {1 Signature} *)

module type LAWS = sig
  module Monoid : Preface_specs.MONOID

  include Semigroup.LAWS with module Semigroup := Monoid
  (** @closed *)

  val monoid_left_identity : unit -> (Monoid.t, Monoid.t) Law.t
  (** Generates the law: [neutral <|> x = x]. *)

  val monoid_right_identity : unit -> (Monoid.t, Monoid.t) Law.t
  (** Generates the law: [x <|> neutral = x]. *)
end

(** {1 Building}

    Construct the set of functions verifying the laws for a given monoid. *)

module For (M : Preface_specs.MONOID) : LAWS with module Monoid := M
