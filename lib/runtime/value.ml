(* Runtime values *)

(** Values stored in variables *)
type value =
  | VBoolean of bool
  | VInteger of Mpzf.t
  | VReal of Reals.Real.t
  | VUnit

(** Results of computations *)
type result =
  | CBoolean of bool
  | CInteger of Mpzf.t
  | CReal of Reals.Real.t
  | CUnit
  | CNone

(** Embed a value into results *)
let return = function
  | VBoolean b -> CBoolean b
  | VInteger k -> CInteger k
  | VReal r -> CReal r
  | VUnit -> CUnit

(** Extract an integers from a value *)
let value_as_integer = function
  | VInteger k -> Some k
  | VBoolean _ | VReal _ | VUnit -> None

(** Extract a boolean from a value *)
let value_as_boolean = function
  | VBoolean b -> Some b
  | VInteger _ | VReal _ | VUnit -> None

(** Extract a real from a value *)
let value_as_real = function
  | VReal r -> Some r
  | VInteger _ | VBoolean _ | VUnit -> None

(** Extract a boolean from a computation results *)
let computation_as_boolean = function
  | CBoolean b -> Some b
  | CInteger _ | CReal _ | CUnit | CNone -> None

(** Extract an integer from a computation results *)
let computation_as_integer = function
  | CInteger k -> Some k
  | CBoolean _ | CReal _ | CUnit | CNone -> None

(** Extract an integer from a computation results *)
let computation_as_real = function
  | CReal r -> Some r
  | CBoolean _ | CInteger _ | CUnit | CNone -> None

(** Extract a value from a computation results *)
let computation_as_value = function
  | CInteger k -> Some (VInteger k)
  | CBoolean b -> Some (VBoolean b)
  | CReal r -> Some (VReal r)
  | CUnit -> Some VUnit
  | CNone -> None

(** Make sure the computation returned no result *)
let computation_as_unit = function
  | CUnit -> Some ()
  | CBoolean _ | CInteger _ | CReal _ | CNone -> None

let print_value v ppf =
  match v with
  | VBoolean b -> Format.fprintf ppf "%b" b
  | VInteger k -> Format.fprintf ppf "%t" (fun ppf -> Mpz.print ppf k)
  | VReal r -> Format.fprintf ppf "%s" (Reals.Real.to_string r)
  | VUnit -> Format.fprintf ppf "()"

let print_result v ppf =
  match v with
  | CNone -> Format.fprintf ppf ""
  | CBoolean b -> Format.fprintf ppf "%b" b
  | CInteger k -> Format.fprintf ppf "%t" (fun ppf -> Mpz.print ppf k)
  | CReal r -> Format.fprintf ppf "%s" (Reals.Real.to_string r)
  | CUnit -> Format.fprintf ppf "()"
