module type LAWS = sig
  module Alternative : Preface_specs.ALTERNATIVE

  include Applicative.LAWS with module Applicative := Alternative

  val alternative_apply_is_right_distributive :
       unit
    -> ( ('a -> 'b) Alternative.t
       , ('a -> 'b) Alternative.t -> 'a Alternative.t -> 'b Alternative.t )
       Law.t

  val alternative_right_absorbtion_for_apply :
    unit -> ('a Alternative.t, 'a Alternative.t) Law.t

  val alternative_left_identity :
    unit -> ('a Alternative.t, 'a Alternative.t) Law.t

  val alternative_right_identity :
    unit -> ('a Alternative.t, 'a Alternative.t) Law.t

  val alternative_combine_is_associative :
       unit
    -> ( 'a Alternative.t
       , 'a Alternative.t -> 'a Alternative.t -> 'a Alternative.t )
       Law.t
end

module For (A : Preface_specs.ALTERNATIVE) = struct
  open Law
  include Applicative.For (A)

  let alternative_apply_is_right_distributive () =
    make "Apply is right distributive"
      (Side.make "(f <|> g) <*> a " (fun f g x ->
           let open A in
           let left = f <|> g in
           left <*> x ) )
      (Side.make "(f <*> a) <|> (g <*> a)" (fun f g x ->
           let open A in
           let left = f <*> x
           and right = g <*> x in
           left <|> right ) )
  ;;

  let alternative_right_absorbtion_for_apply () =
    make "Right absorbtion for Apply"
      (Side.make "neutral <*> x" A.(apply neutral))
      (Side.make "x" Fun.id)
  ;;

  let alternative_left_identity () =
    make "Left identity"
      (Side.make "neutral <|> x" A.(combine neutral))
      (Side.make "x" Fun.id)
  ;;

  let alternative_right_identity () =
    make "Right identity"
      (Side.make "x <|> neutral" (fun x -> A.(combine x neutral)))
      (Side.make "x" Fun.id)
  ;;

  let alternative_combine_is_associative () =
    make "Combine is associative"
      (Side.make "x <|> (y <|> z)" (fun x y z ->
           let open A in
           let right = y <|> z in
           x <|> right ) )
      (Side.make "(x <|> y) <|> z" (fun x y z ->
           let open A in
           let left = x <|> y in
           left <|> z ) )
  ;;
end
