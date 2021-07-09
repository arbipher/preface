(** The abstractions described in [Preface] generally impose laws (or behaviour)
    that must be respected to ensure that the derived combinators have the
    expected behaviour. This library allows the generation of structured laws
    ({!module:Law}) for a concrete representation of an abstraction. They are
    notably used in tests.*)

(** {1 Monoid hierarchy} *)

module Semigroup = Semigroup
module Monoid = Monoid

(** {1 Functor hierarchy} *)

module Functor = Functor
module Contravariant = Contravariant
module Alt = Alt
module Applicative = Applicative
module Alternative = Alternative

(** {1 Bifunctor hierarchy} *)

(** {1 Profunctor hierarchy} *)

(** {1 Arrow hierarchy} *)

(** {1 Descriptions of laws}

    Describes a law as structured data. *)

module Law = Law
