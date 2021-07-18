(** {1 Signature} *)

module type LAWS = sig
  module Applicative : Preface_specs.APPLICATIVE

  include Functor.LAWS with module Functor := Applicative
  (** @closed *)

  val applicative_identity : unit -> ('a Applicative.t, 'a Applicative.t) Law.t
  (** Generates the law: [pure id <*> x = x]. *)

  val applicative_homomorphism :
    unit -> ('a -> 'b, 'a -> 'b Applicative.t) Law.t
  (** Generates the law: [pure f <*> pure x = pure f x]. *)

  val applicative_interchange :
    unit -> (('a -> 'b) Applicative.t, 'a -> 'b Applicative.t) Law.t
  (** Generates the law: [f <*> pure x = pure ((|>) x) <*> f]. *)

  val applicative_composition :
       unit
    -> ( ('a -> 'b) Applicative.t
       , ('c -> 'a) Applicative.t -> 'c Applicative.t -> 'b Applicative.t )
       Law.t
  (** Generates the law: [pure ( % ) <*> u <*> v <*> w = u <*> (v <*> w)]. *)

  val applicative_map_is_pure_and_apply :
    unit -> ('a -> 'b, 'a Applicative.t -> 'b Applicative.t) Law.t
  (** Generates the law: [map f x = pure f <*> x]. *)

  val applicative_ignore_left :
    unit -> (unit Applicative.t, 'a Applicative.t -> 'a Applicative.t) Law.t
  (** Generates the law: [u *> v = (id <$ u) <*> v]. *)

  val applicative_ignore_right :
    unit -> ('a Applicative.t, unit Applicative.t -> 'a Applicative.t) Law.t
  (** Generates the law: [u <* v = lift2 const u v]. *)
end

(** {1 Building}

    Construct the set of functions verifying the laws for a given applicative. *)

module For (A : Preface_specs.APPLICATIVE) : LAWS with module Applicative := A
