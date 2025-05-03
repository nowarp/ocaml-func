open! IStd

module Id = struct
  type t = int
  [@@deriving show]
  let next_id =
    let n = ref (-1) in
    fun () -> incr n; !n
end
type 'a identified = { id: Id.t; node: 'a }
[@@deriving show]

module Span = struct
  type t = { lo: int; hi: int }
  let spans: (Id.t, t) Hashtbl.t =
    Hashtbl.create (module Int)
  let save id lo hi =
    Hashtbl.add_exn spans ~key:id ~data:{lo; hi}
  let get id =
    Hashtbl.find spans id
  let get_exn id = get id |> Option.value_exn
  let to_string filename {lo; hi} =
    In_channel.with_file filename ~f:(fun ic ->
      let buf = Buffer.create 100000 in
      let len = hi - lo in
      In_channel.seek ic (Int64.of_int lo);
      ignore @@ In_channel.input_buffer ic buf ~len;
      Buffer.contents buf)
  let to_string_id filename id =
    get id |> Option.value_map ~default:"" ~f:(to_string filename)
end

type string_type =
  | StrRaw        (* s *)
  | StrConst      (* a *)
  | StrInt        (* u *)
  | Str32Sha256   (* h *)
  | StrSha256     (* H *)
  | StrCrc32      (* c *)
[@@deriving show]

type identifier = string
[@@deriving show]

type number_literal = string
[@@deriving show]

type string_literal = { str_val: string
                      ; str_ty: string_type option }
[@@deriving show]

type assign_op =
  | OpAssign          (* = *)
  | OpAssignAdd       (* += *)
  | OpAssignSub       (* -= *)
  | OpAssignMul       (* *= *)
  | OpAssignDivFloor  (* /= *)
  | OpAssignDivRound  (* ~/= *)
  | OpAssignDivCeil   (* ^/= *)
  | OpAssignMod       (* %= *)
  | OpAssignModRound  (* ~%= *)
  | OpAssignModCeil   (* ^%= *)
  | OpAssignShl       (* <<= *)
  | OpAssignShr       (* >>= *)
  | OpAssignShrRound  (* ~>>= *)
  | OpAssignShrCeil   (* ^>>= *)
  | OpAssignBitAnd    (* &= *)
  | OpAssignBitOr     (* |= *)
  | OpAssignBitXor    (* ^= *)
[@@deriving show]

type bin_op =
  | OpEq          (* == *)
  | OpLt          (* < *)
  | OpGt          (* > *)
  | OpLeq         (* <= *)
  | OpGeq         (* >= *)
  | OpNeq         (* != *)
  | OpCmp         (* <<> *)
  | OpShl         (* << *)
  | OpShr         (* >> *)
  | OpShrRound    (* ~>> *)
  | OpShrCeil     (* ^>> *)
  | OpSub         (* - *)
  | OpAdd         (* + *)
  | OpBitOr       (* | *)
  | OpBitXor      (* ^ *)
  | OpMul         (* * *)
  | OpDivFloor    (* / *)
  | OpMod         (* % *)
  | OpDivRound    (* ~/ *)
  | OpDivCeil     (* ^/ *)
  | OpModRound    (* ~% *)
  | OpModCeil     (* ^% *)
  | OpDivMod      (* /% *)
  | OpBitAnd      (* & *)
[@@deriving show]

type unary_operator =
  | UMinus | UTilde
[@@deriving show]

let string_of_binop = function
  | OpEq       -> "=="
  | OpLt       -> "<"
  | OpGt       -> ">"
  | OpLeq      -> "<="
  | OpGeq      -> ">="
  | OpNeq      -> "!="
  | OpCmp      -> "<<>"
  | OpShl      -> "<<"
  | OpShr      -> ">>"
  | OpShrRound -> "~>>"
  | OpShrCeil  -> "^>>"
  | OpSub      -> "-"
  | OpAdd      -> "+"
  | OpBitOr    -> "|"
  | OpBitXor   -> "^"
  | OpMul      -> "*"
  | OpDivFloor -> "/"
  | OpMod      -> "%"
  | OpDivRound -> "~/"
  | OpDivCeil  -> "^/"
  | OpModRound -> "~%"
  | OpModCeil  -> "^%"
  | OpDivMod   -> "/%"
  | OpBitAnd   -> "&"

let string_type_of_char = function
  | 's' -> StrRaw
  | 'a' -> StrConst
  | 'u' -> StrInt
  | 'h' -> Str32Sha256
  | 'H' -> StrSha256
  | 'c' -> StrCrc32
  | _ -> invalid_arg "Unknown string type"
