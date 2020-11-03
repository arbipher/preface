open Preface_stdlib.Option

module Requirement = struct
  type nonrec 'a t = 'a t

  let name = "Option"

  let size = 100

  let arbitrary x = Preface_qcheck.Arbitrary.option x
end

module Functor_test =
  Preface_qcheck.Functor.Make (Functor) (Requirement)
    (Preface_qcheck.Sample.Pack)
module Applicative_test =
  Preface_qcheck.Applicative.Make (Applicative) (Requirement)
    (Preface_qcheck.Sample.Pack)

module Alternative_test =
  Preface_qcheck.Alternative.Make_for_monoidal_behaviour
    (Alternative)
    (Requirement)
    (Preface_qcheck.Sample.Pack)

module Alternative_right_absorption_test =
  Preface_qcheck.Alternative.Make_right_absorption (Alternative) (Requirement)
    (Preface_qcheck.Sample.Pack)

module Alternative_right_distributivity_of_apply_test =
  Preface_qcheck.Alternative.Make_right_distributivity_of_apply
    (Alternative)
    (Requirement)
    (Preface_qcheck.Sample.Pack)

module Monad_test =
  Preface_qcheck.Monad.Make (Monad) (Requirement) (Preface_qcheck.Sample.Pack)
module Monad_plus_test =
  Preface_qcheck.Monad_plus.Make_behaviour (Monad_plus) (Requirement)
    (Preface_qcheck.Sample.Pack)
module Monad_plus_monoid_test =
  Preface_qcheck.Monad_plus.Make_monoidal_laws (Monad_plus) (Requirement)
    (Preface_qcheck.Sample.Pack)
module Monad_plus_left_zero_test =
  Preface_qcheck.Monad_plus.Make_left_zero_law (Monad_plus) (Requirement)
    (Preface_qcheck.Sample.Pack)
module Monad_plus_left_catch_test =
  Preface_qcheck.Monad_plus.Make_left_catch_law (Monad_plus) (Requirement)
    (Preface_qcheck.Sample.Pack)

module Monoid_test =
  Preface_qcheck.Monoid.Make
    (Monoid
       (Misc.Sum_semigroup))
       (struct
         type nonrec t = int t

         let name = "Option over Sum int"

         let size = 100

         let arbitrary = Preface_qcheck.Arbitrary.(option int)
       end)

let cases =
  Functor_test.cases
  @ Applicative_test.cases
  @ Alternative_test.cases
  @ Alternative_right_absorption_test.cases
  @ Alternative_right_distributivity_of_apply_test.cases
  @ Monad_test.cases
  @ Monoid_test.cases
  @ Monad_plus_test.cases
  @ Monad_plus_monoid_test.cases
  @ Monad_plus_left_zero_test.cases
  @ Monad_plus_left_catch_test.cases
;;