(** {1 Signature} *)

module type LAWS = sig
  module Selective : Preface_specs.SELECTIVE

  include Applicative.LAWS with module Applicative := Selective
  (** @closed *)

  val selective_identity :
       unit
    -> (('a, 'a) Preface_core.Shims.Either.t Selective.t, 'a Selective.t) Law.t
  (** Generates the law: [x <*? pure Fun.id = Either.case Fun.id Fun.id <$> x]. *)

  val selective_distributivity :
       unit
    -> ( ('a, 'b) Preface_core.Shims.Either.t
       , ('a -> 'b) Selective.t -> ('a -> 'b) Selective.t -> 'b Selective.t )
       Law.t
  (** Generates the law:
      [pure x <*? (y *> z) = (pure x <*? y) *> (pure x <*? z)]. *)

  val selective_associativity :
       unit
    -> ( ('a, 'b) Preface_core.Shims.Either.t Selective.t
       ,    ('c, 'a -> 'b) Preface_core.Shims.Either.t Selective.t
         -> ('c -> 'a -> 'b) Selective.t
         -> 'b Selective.t )
       Law.t
  (** Generates the law: [x <*? (y <*? z] [=] [(Either.(map_right right) <$> x)]
      [<*?]
      [((fun x a -> Either.map ~left:(fun x -> (x, a)) ~right:(fun f -> f a) x) <$> y)]
      [<*?] [(uncurry <$> z]. *)

  val selective_apply_pure_function_to_the_result :
       unit
    -> ( 'a -> 'b
       ,    ('c, 'a) Preface_core.Shims.Either.t Selective.t
         -> ('c -> 'a) Selective.t
         -> 'b Selective.t )
       Law.t
  (** Generates the law: [f <$> select x y] [=]
      [select (Either.map_right f <$> x) (map f <$> y)]. *)

  val selective_apply_pure_function_to_left_case_of_the_first_argument :
       unit
    -> ( 'a -> 'b
       ,    ('a, 'c) Preface_core.Shims.Either.t Selective.t
         -> ('b -> 'c) Selective.t
         -> 'c Selective.t )
       Law.t
  (** Generates the law:
      [select (Either.map_left f <$> x) y = select x ((%>) f) <$> y)]. *)

  val selective_apply_pure_function_to_the_second_argument :
       unit
    -> ( 'a -> 'b -> 'c
       ,    ('b, 'c) Preface_core.Shims.Either.t Selective.t
         -> 'a Selective.t
         -> 'c Selective.t )
       Law.t
  (** Generates the law: [select x (f <$> y)] [=]
      [select (Either.map_left (flip f) <$> x) ((|>) <$> y)]. *)

  val selective_generalized_identity :
       unit
    -> ( ('a, 'b) Preface_core.Shims.Either.t Selective.t
       , ('a -> 'b) -> 'b Selective.t )
       Law.t
  (** Generates the law: [x <*? pure y = Either.case y Fun.id <$> x]. *)

  val selective_rigid_apply :
    unit -> (('a -> 'b) Selective.t, 'a Selective.t -> 'b Selective.t) Law.t
  (** {b Only for Rigid Selective}: Generates the law:
      [f <*> x = select (map Either.left f) (map ( |> ) x)]. *)

  val selective_rigid_interchange :
       unit
    -> ( 'a Selective.t
       ,    ('b, 'c) Preface_core.Shims.Either.t Selective.t
         -> ('b -> 'c) Selective.t
         -> 'c Selective.t )
       Law.t
  (** {b Only for Rigid Selective}: Generates the law:
      [x *> (y <*? z) = (x *> y) <*? z]. *)
end

(** {1 Building}

    Construct the set of functions verifying the laws for a given selective. *)

module For (S : Preface_specs.SELECTIVE) : LAWS with module Selective := S
