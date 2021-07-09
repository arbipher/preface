module type LAWS = sig
  module Semigroup : Preface_specs.SEMIGROUP

  val associative_combine :
    unit -> (Semigroup.t, Semigroup.t -> Semigroup.t -> Semigroup.t) Law.t
end

module For (S : Preface_specs.SEMIGROUP) = struct
  open Law

  let associative_combine () =
    make "Combine must be associative"
      (Side.make "(a <|> b) <|> c" (fun a b c ->
           let open S.Infix in
           let left = a <|> b in
           left <|> c ) )
      (Side.make "a <|> (b <|> c)" (fun a b c ->
           let open S.Infix in
           let right = b <|> c in
           a <|> right ) )
  ;;
end
