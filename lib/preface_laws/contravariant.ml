module type LAWS = sig
  module Contravariant : Preface_specs.CONTRAVARIANT

  val contravariant_preserve_identity_morphisms :
    unit -> ('a Contravariant.t, 'a Contravariant.t) Law.t

  val contravariant_preserve_composition_of_morphisms :
       unit
    -> ('a -> 'b, ('b -> 'c) -> 'c Contravariant.t -> 'a Contravariant.t) Law.t
end

module For (C : Preface_specs.CONTRAVARIANT) = struct
  open Law

  let contravariant_preserve_identity_morphisms () =
    make "Preserve identity morphisms"
      (Side.make "contramap id" (C.contramap Fun.id))
      (Side.make "id" Fun.id)
  ;;

  let contravariant_preserve_composition_of_morphisms () =
    let open Preface_core.Fun.Infix in
    make "Preserve composition of morphisms"
      (Side.make "contramap (g % f)" (fun f g -> C.contramap (g % f)))
      (Side.make "(contramap f) % (contramap g)" (fun f g ->
           C.(contramap f % contramap g) ) )
  ;;
end
