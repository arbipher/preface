(** Modules for building {!Preface_specs.TRAVERSABLE} modules. *)

(** {1 Tutorial}

    A [Traversable] is a little different from the modules that we have seen
    previously ([Functor], [Applicative] and [Monad]). It is built using two
    main components:

    - an iterable structure (for example, a [List.t]);
    - a subject, which can be an [Applicative] or a [Monad].

    So, rather than directly producing an interface, the use of [Traversable]
    produces a functor which is to be configured by the subject that we would
    like to traverse.

    {2 Basics}

    {3 Using an [Applicative] and [List]}

    It is very common to define list as traversable. Here, for example, is a
    proposal for a traversable list inhabited by applicatives.

    {[
      module List = struct
        module Traversable (A: Preface_specs.APPLICATIVE) :
          Preface_specs.TRAVERSABLE
          with type 'a t = 'a A.t
           and type 'a iter = 'a list
        struct
          type 'a t = 'a A.t
          type 'a iter = 'a list
          let traverse =
            let open A.Infix in
            let rec aux f = function
              | [] -> A.pure []
              | x :: xs -> Stdlib.List.cons <$> f x <*> aux f xs
            in
            aux
        end
      end
    ]}

    The code can be intimidating, but we just define the [traverse] function.
    Now it is possible to build modules that traverse a list for any
    applicative.

    {3 Using our [List.Traversable]}

    Let's see how to use the module [List.Traversable]. For example, for
    [Option]!

    {[
      module Option = Preface_stdlib.Option
      module Option_traversable = List.Traversable (Option.Applicative)

      let result = [ Some 10; Some 20; Some 30 ] |> Option_traversable.sequence

      (* val result : int list option = Some [10; 20; 30] *)

      let result' = [ Some 10; None; Some 20 ] |> Option_traversable.sequence

      (* val result : int list option = None *)
    ]}

    {2 Advanced}

    You can define [Traversable] using [Monad] instead of [Applicative] and like
    [Functor], [Monad] or [Applicative], a [Traversable] can be define component
    by component. Offering your own implementation for performance issues. *)

(** {1 Documentation} *)

(** {1 Construction}

    Standard way to build a [Traversable]. *)

(** Incarnation of [Traversable] using an [Applicative]. *)
module Over_applicative
    (A : Preface_specs.APPLICATIVE)
    (Req : Preface_specs.Traversable.WITH_TRAVERSE with type 'a t = 'a A.t) :
  Preface_specs.TRAVERSABLE
    with type 'a t = 'a Req.t
     and type 'a iter = 'a Req.iter

(** Incarnation of [Traversable] using a [Monad]. *)
module Over_monad
    (M : Preface_specs.MONAD)
    (Req : Preface_specs.Traversable.WITH_TRAVERSE with type 'a t = 'a M.t) :
  Preface_specs.TRAVERSABLE
    with type 'a t = 'a Req.t
     and type 'a iter = 'a Req.iter

(** {2 Manual construction}

    Advanced way to build a [Traversable], constructing and assembling a
    component-by-component a traversable. (In order to provide your own
    implementation for some features.) *)

(** Incarnation of [Traversable] using each components of a [Traversable].*)
module Via
    (C : Preface_specs.Traversable.CORE)
    (O : Preface_specs.Traversable.OPERATION
           with type 'a t = 'a C.t
            and type 'a iter = 'a C.iter) :
  Preface_specs.TRAVERSABLE with type 'a t = 'a C.t and type 'a iter = 'a C.iter

(** Incarnation of [Traversable.Core] using [traverse]. *)
module Core (Req : Preface_specs.Traversable.WITH_TRAVERSE) :
  Preface_specs.Traversable.CORE
    with type 'a t = 'a Req.t
     and type 'a iter = 'a Req.iter

(** Incarnation of [Traversable.Core] using an [Applicative]. *)
module Core_over_applicative
    (A : Preface_specs.APPLICATIVE)
    (Req : Preface_specs.Traversable.WITH_TRAVERSE with type 'a t = 'a A.t) :
  Preface_specs.Traversable.CORE
    with type 'a t = 'a Req.t
     and type 'a iter = 'a Req.iter

(** Incarnation of [Traversable.Core] using a [Monad]. *)
module Core_over_monad
    (M : Preface_specs.MONAD)
    (Req : Preface_specs.Traversable.WITH_TRAVERSE with type 'a t = 'a M.t) :
  Preface_specs.Traversable.CORE
    with type 'a t = 'a Req.t
     and type 'a iter = 'a Req.iter

(** Incarnation of [Traversable.Operation] with [Traversable.Core].*)
module Operation (C : Preface_specs.Traversable.CORE) :
  Preface_specs.Traversable.OPERATION
    with type 'a t = 'a C.t
     and type 'a iter = 'a C.iter

(** {1 Join traversable}

    Since [Traversable] can be define for traversing iterable structure being a
    [Monad] or an [Applicative], there is some module (like [List] or
    [Nonempty_list] which define in [Applicative] or [Monad] Modules a
    [Traversable] module. *)

(** Join [Monad] and [Traversable] *)
module Join_with_monad
    (I : Preface_specs.MONAD) (T : functor (M : Preface_specs.MONAD) ->
      Preface_specs.TRAVERSABLE
        with type 'a t = 'a M.t
         and type 'a iter = 'a I.t) :
  Preface_specs.Traversable.API_OVER_MONAD with type 'a t = 'a I.t

(** Join [Applicative] and [Traversable] *)
module Join_with_applicative
    (I : Preface_specs.APPLICATIVE)
    (T : functor
      (A : Preface_specs.APPLICATIVE)
      ->
      Preface_specs.TRAVERSABLE
        with type 'a t = 'a A.t
         and type 'a iter = 'a I.t) :
  Preface_specs.Traversable.API_OVER_APPLICATIVE with type 'a t = 'a I.t
