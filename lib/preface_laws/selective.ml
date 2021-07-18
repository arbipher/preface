module Either = Preface_core.Shims.Either

module type LAWS = sig
  module Selective : Preface_specs.SELECTIVE

  include Applicative.LAWS with module Applicative := Selective

  val selective_identity :
    unit -> (('a, 'a) Either.t Selective.t, 'a Selective.t) Law.t

  val selective_distributivity :
       unit
    -> ( ('a, 'b) Either.t
       , ('a -> 'b) Selective.t -> ('a -> 'b) Selective.t -> 'b Selective.t )
       Law.t

  val selective_associativity :
       unit
    -> ( ('a, 'b) Either.t Selective.t
       ,    ('c, 'a -> 'b) Either.t Selective.t
         -> ('c -> 'a -> 'b) Selective.t
         -> 'b Selective.t )
       Law.t

  val selective_apply_pure_function_to_the_result :
       unit
    -> ( 'a -> 'b
       ,    ('c, 'a) Either.t Selective.t
         -> ('c -> 'a) Selective.t
         -> 'b Selective.t )
       Law.t

  val selective_apply_pure_function_to_left_case_of_the_first_argument :
       unit
    -> ( 'a -> 'b
       ,    ('a, 'c) Either.t Selective.t
         -> ('b -> 'c) Selective.t
         -> 'c Selective.t )
       Law.t

  val selective_apply_pure_function_to_the_second_argument :
       unit
    -> ( 'a -> 'b -> 'c
       , ('b, 'c) Either.t Selective.t -> 'a Selective.t -> 'c Selective.t )
       Law.t

  val selective_generalized_identity :
    unit -> (('a, 'b) Either.t Selective.t, ('a -> 'b) -> 'b Selective.t) Law.t

  val selective_rigid_apply :
    unit -> (('a -> 'b) Selective.t, 'a Selective.t -> 'b Selective.t) Law.t

  val selective_rigid_interchange :
       unit
    -> ( 'a Selective.t
       ,    ('b, 'c) Either.t Selective.t
         -> ('b -> 'c) Selective.t
         -> 'c Selective.t )
       Law.t
end

module For (S : Preface_specs.SELECTIVE) = struct
  open Law
  include Applicative.For (S)

  let selective_identity () =
    make "Identity"
      (Side.make "x <*? pure Fun.id" (fun x ->
           let open S in
           x <*? pure Fun.id ) )
      (Side.make "Either.case Fun.id Fun.id <$> x" (fun x ->
           let open S in
           Either.case Fun.id Fun.id <$> x ) )
  ;;

  let selective_distributivity () =
    make "Distributivity"
      (Side.make "pure x <*? (y *> z)" (fun x y z ->
           let open S in
           let right = replace () y *> z in
           pure x <*? right ) )
      (Side.make "(pure x <*? y) *> (pure x <*? z)" (fun x y z ->
           let open S in
           let left = replace () (pure x <*? y)
           and right = pure x <*? z in
           left *> right ) )
  ;;

  let selective_associativity () =
    make "Associativity"
      (Side.make "x <*? (y <*? z)" (fun x y z ->
           let open S in
           x <*? (y <*? z) ) )
      (Side.make
         "(Either.(map_right right) <$> x) <*? ((fun x a -> Either.map \
          ~left:(fun x -> (x, a)) ~right:(fun f -> f a) x) <$> y) <*? (uncurry \
          <$> z)" (fun x y z ->
           let f a = Either.map_right Either.right a in
           let g x a =
             Either.map ~left:(fun x -> (x, a)) ~right:(fun f -> f a) x
           in
           let h f (a, b) = f a b in
           let open S in
           let fst = f <$> x
           and snd = g <$> y
           and trd = h <$> z in
           fst <*? snd <*? trd ) )
  ;;

  let selective_apply_pure_function_to_the_result () =
    make "Apply a pure function to the result (Theorem 1)"
      (Side.make "f <$> select x y" (fun f x y ->
           let open S in
           f <$> select x y ) )
      (Side.make "select (Either.map_right f <$> x) (map f <$> y)" (fun f x y ->
           let open S in
           let open Preface_core.Fun in
           select (Either.map_right f <$> x) (( % ) f <$> y) ) )
  ;;

  let selective_apply_pure_function_to_left_case_of_the_first_argument () =
    make
      "Apply a pure function to the Left case of the first argument (Theorem 2)"
      (Side.make "select (Either.map_left f <$> x) y" (fun f x y ->
           let open S in
           select (Either.map_left f <$> x) y ) )
      (Side.make "select x ((%>) f) <$> y)" (fun f x y ->
           let open S in
           let open Preface_core.Fun in
           select x (( %> ) f <$> y) ) )
  ;;

  let selective_apply_pure_function_to_the_second_argument () =
    make "Apply a pure function to the second argument (Theorem 3)"
      (Side.make "select x (f <$> y)" (fun f x y ->
           let open S in
           select x (f <$> y) ) )
      (Side.make "select (Either.map_left (flip f) <$> x) ((|>) <$> y)"
         (fun f x y ->
           let open S in
           let open Preface_core.Fun in
           select (Either.map_left (flip f) <$> x) (( |> ) <$> y) ) )
  ;;

  let selective_generalized_identity () =
    make "Generalised identity (Theorem 4)"
      (Side.make "x <*? pure y" (fun x y ->
           let open S in
           x <*? pure y ) )
      (Side.make "Either.case y Fun.id <$> x" (fun x y ->
           let open S in
           Either.case y Fun.id <$> x ) )
  ;;

  let selective_rigid_apply () =
    make "Selective Apply for Rigid Selective (Theorem 5)"
      (Side.make "f <*> x" S.apply)
      (Side.make "select (map Either.left f) (map ( |> ) x" (fun f x ->
           let open S in
           select (map Either.left f) (map ( |> ) x) ) )
  ;;

  let selective_rigid_interchange () =
    make "Selective Interchange for Rigid Selective (Theorem 6)"
      (Side.make "x *> (y <*? z)" (fun x y z ->
           let open S in
           let b = y <*? z in
           let a = ignore <$> x in
           a *> b ) )
      (Side.make "(x *> y) <*? z" (fun x y z ->
           let open S in
           let a = ignore <$> x in
           let b = a *> y in
           b <*? z ) )
  ;;
end
