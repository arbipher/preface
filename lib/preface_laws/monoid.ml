module type LAWS = sig
  module Monoid : Preface_specs.MONOID

  include Semigroup.LAWS with module Semigroup := Monoid

  val monoid_left_identity : unit -> (Monoid.t, Monoid.t) Law.t

  val monoid_right_identity : unit -> (Monoid.t, Monoid.t) Law.t
end

module For (M : Preface_specs.MONOID) = struct
  open Law
  include Semigroup.For (M)

  let monoid_left_identity () =
    make "Left identity"
      (Side.make "neutral <|> x" (fun x -> M.(combine neutral x)))
      (Side.make "x" Fun.id)
  ;;

  let monoid_right_identity () =
    make "Right identity"
      (Side.make "x <|> neutral" (fun x -> M.(combine x neutral)))
      (Side.make "x" Fun.id)
  ;;
end
