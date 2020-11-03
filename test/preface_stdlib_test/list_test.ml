module Functor_test = Support.Functor (struct
  include Preface_stdlib.List
  include Functor
end)

module Applicative_test = Support.Applicative (struct
  include Preface_stdlib.List
  include Applicative
end)

module Monad_test = Support.Monad (struct
  include Preface_stdlib.List
  include Monad
end)

let fold_map_over_values () =
  let module Prod = Preface_make.Monoid.Via_combine_and_neutral (struct
    type t = int

    let neutral = 1

    let combine = ( * )
  end) in
  let expected = 120
  and computed =
    Preface_stdlib.List.Foldable.fold_map
      (module Prod)
      int_of_string
      [ "1"; "2"; "3"; "4"; "5" ]
  in
  Alcotest.(check int) "fold_map with success" expected computed
;;

let fold_map_over_empty () =
  let module Prod = Preface_make.Monoid.Via_combine_and_neutral (struct
    type t = int

    let neutral = 1

    let combine = ( * )
  end) in
  let expected = 1
  and computed =
    Preface_stdlib.List.Foldable.fold_map (module Prod) int_of_string []
  in
  Alcotest.(check int) "fold_map with success" expected computed
;;

let test_cases =
  [
    ("List Functor", Functor_test.cases)
  ; ("List Applicative", Applicative_test.cases)
  ; ("List Monad", Monad_test.cases)
  ; ( "List Foldable"
    , let open Alcotest in
      [
        test_case "Fold_map over values" `Quick fold_map_over_values
      ; test_case "Fold_map over empty" `Quick fold_map_over_empty
      ] )
  ]
;;