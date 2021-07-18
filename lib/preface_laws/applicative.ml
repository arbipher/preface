module type LAWS = sig
  module Applicative : Preface_specs.APPLICATIVE

  include Functor.LAWS with module Functor := Applicative

  val applicative_identity : unit -> ('a Applicative.t, 'a Applicative.t) Law.t

  val applicative_homomorphism :
    unit -> ('a -> 'b, 'a -> 'b Applicative.t) Law.t

  val applicative_interchange :
    unit -> (('a -> 'b) Applicative.t, 'a -> 'b Applicative.t) Law.t

  val applicative_composition :
       unit
    -> ( ('a -> 'b) Applicative.t
       , ('c -> 'a) Applicative.t -> 'c Applicative.t -> 'b Applicative.t )
       Law.t

  val applicative_map_is_pure_and_apply :
    unit -> ('a -> 'b, 'a Applicative.t -> 'b Applicative.t) Law.t

  val applicative_ignore_left :
    unit -> (unit Applicative.t, 'a Applicative.t -> 'a Applicative.t) Law.t

  val applicative_ignore_right :
    unit -> ('a Applicative.t, unit Applicative.t -> 'a Applicative.t) Law.t
end

module For (A : Preface_specs.APPLICATIVE) = struct
  open Law
  include Functor.For (A)

  let applicative_identity () =
    make "Identity"
      (Side.make "pure id <*> x" (fun x ->
           let open A in
           pure Fun.id <*> x ) )
      (Side.make "x" Fun.id)
  ;;

  let applicative_homomorphism () =
    make "Homomorphism"
      (Side.make "pure f <*> pure x" (fun f x ->
           let open A in
           pure f <*> pure x ) )
      (Side.make "pure f x" (fun f x ->
           let open A in
           pure (f x) ) )
  ;;

  let applicative_interchange () =
    make "Interchange"
      (Side.make "f <*> pure x" (fun f x ->
           let open A in
           f <*> pure x ) )
      (Side.make "pure ((|>) x) <*> f" (fun f x ->
           let open A in
           pure (( |> ) x) <*> f ) )
  ;;

  let applicative_composition () =
    make "Composition"
      (Side.make "pure ( % ) <*> u <*> v <*> w" (fun u v w ->
           let open Preface_core.Fun.Infix in
           let open A in
           pure ( % ) <*> u <*> v <*> w ) )
      (Side.make "u <*> (v <*> w)" (fun u v w ->
           let open A in
           let right = v <*> w in
           u <*> right ) )
  ;;

  let applicative_map_is_pure_and_apply () =
    make "Map is encodable via pure and apply"
      (Side.make "map f x" A.map)
      (Side.make "pure f <*> x" (fun f x -> A.(pure f <*> x)))
  ;;

  let applicative_ignore_left () =
    make "Ignore left"
      (Side.make "u *> v" (fun u v -> A.Infix.(u *> v)))
      (Side.make "(id <$ u) <*> v" (fun u v -> A.Infix.(Fun.id <$ u <*> v)))
  ;;

  let applicative_ignore_right () =
    make "Ignore right"
      (Side.make "u <* v" A.Infix.( <* ))
      (Side.make "lift2 const u v" (A.lift2 Fun.const))
  ;;
end
