module type LAWS = sig
  module Alt : Preface_specs.ALT

  include Functor.LAWS with module Functor := Alt

  val alt_associative_combine :
    unit -> ('a Alt.t, 'a Alt.t -> 'a Alt.t -> 'a Alt.t) Law.t

  val alt_left_distributive_map_over_combine :
    unit -> ('a -> 'b, 'a Alt.t -> 'a Alt.t -> 'b Alt.t) Law.t
end

module For (A : Preface_specs.ALT) = struct
  open Law
  include Functor.For (A)

  let alt_associative_combine () =
    make "Combine must be associative"
      (Side.make "(a <|> b) <|> c" (fun a b c ->
           let open A.Infix in
           let left = a <|> b in
           left <|> c ) )
      (Side.make "a <|> (b <|> c)" (fun a b c ->
           let open A.Infix in
           let right = b <|> c in
           a <|> right ) )
  ;;

  let alt_left_distributive_map_over_combine () =
    make "Map is left-distributive over combine"
      (Side.make "f <$> (a <|> b)" (fun f a b ->
           let open A.Infix in
           let right = a <|> b in
           f <$> right ) )
      (Side.make "(f <$> a) <|> (f <$> b)" (fun f a b ->
           let open A.Infix in
           let left = f <$> a
           and right = f <$> b in
           left <|> right ) )
  ;;
end
