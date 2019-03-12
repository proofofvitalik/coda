open Core_kernel
open Fold_lib
open Tuple_lib
open Signature_lib
open Snark_params.Tick
open Currency
module Tag = Transaction_union_tag

module Body = struct
  type ('tag, 'pk, 'amount) t_ = {tag: 'tag; public_key: 'pk; amount: 'amount}
  [@@deriving sexp]

  type var = (Tag.var, Public_key.Compressed.var, Currency.Amount.var) t_

  type t = (Tag.t, Public_key.Compressed.t, Currency.Amount.t) t_
  [@@deriving sexp]

  let fold ({tag; public_key; amount} : t) =
    Fold.(
      Tag.fold tag
      +> Public_key.Compressed.fold public_key
      +> Currency.Amount.fold amount)

  let gen ~fee =
    let open Quickcheck.Generator.Let_syntax in
    let%bind tag = Tag.gen in
    let%map amount =
      let min, max =
        let max_amount_without_overflow =
          Amount.(sub max_int (of_fee fee))
          |> Option.value_exn ?here:None ?message:None ?error:None
        in
        match tag with
        | Payment -> (Amount.zero, max_amount_without_overflow)
        | Stake_delegation -> (Amount.zero, max_amount_without_overflow)
        | Fee_transfer -> (Amount.zero, max_amount_without_overflow)
        | Coinbase ->
            (* In this case, 
             fee - amount should be defined. In other words,
             fee >= amount. *)
            (Amount.zero, Amount.of_fee fee)
      in
      Amount.gen_incl min max
    and public_key = Public_key.Compressed.gen in
    {tag; public_key; amount}

  let length_in_triples =
    Tag.length_in_triples + Public_key.Compressed.length_in_triples
    + Currency.Amount.length_in_triples

  let to_hlist {tag; public_key; amount} = H_list.[tag; public_key; amount]

  let spec =
    Data_spec.[Tag.typ; Public_key.Compressed.typ; Currency.Amount.typ]

  let typ =
    Typ.of_hlistable spec ~var_to_hlist:to_hlist ~value_to_hlist:to_hlist
      ~var_of_hlist:(fun H_list.([tag; public_key; amount]) ->
        {tag; public_key; amount} )
      ~value_of_hlist:(fun H_list.([tag; public_key; amount]) ->
        {tag; public_key; amount} )

  let of_user_command_payload_body = function
    | User_command_payload.Body.Payment {receiver; amount} ->
        {tag= Tag.Payment; public_key= receiver; amount}
    | Stake_delegation (Set_delegate {new_delegate}) ->
        { tag= Tag.Stake_delegation
        ; public_key= new_delegate
        ; amount= Currency.Amount.zero }

  module Checked = struct
    let constant ({tag; public_key; amount} : t) : var =
      { tag= Tag.Checked.constant tag
      ; public_key= Public_key.Compressed.var_of_t public_key
      ; amount= Currency.Amount.var_of_t amount }

    let to_triples ({tag; public_key; amount} : var) =
      let%map public_key = Public_key.Compressed.var_to_triples public_key in
      Tag.Checked.to_triples tag @ public_key
      @ Currency.Amount.var_to_triples amount
  end
end

type t = (User_command_payload.Common.t, Body.t) User_command_payload.t_
[@@deriving sexp]

type payload = t [@@deriving sexp]

type var = (User_command_payload.Common.var, Body.var) User_command_payload.t_

type payload_var = var

let gen =
  let open Quickcheck.Generator.Let_syntax in
  let%bind common = User_command_payload.Common.gen in
  let%map body = Body.gen ~fee:common.fee in
  {User_command_payload.common; body}

let to_hlist ({common; body} : (_, _) User_command_payload.t_) =
  H_list.[common; body]

let of_hlist : type c v.
    (unit, c -> v -> unit) H_list.t -> (c, v) User_command_payload.t_ =
 fun H_list.([common; body]) -> {common; body}

let typ : (var, t) Typ.t =
  Typ.of_hlistable
    [User_command_payload.Common.typ; Body.typ]
    ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
    ~value_of_hlist:of_hlist

let of_user_command_payload ({common; body} : User_command_payload.t) : t =
  {common; body= Body.of_user_command_payload_body body}

let fold ({common; body} : t) =
  Fold.(User_command_payload.Common.fold common +> Body.fold body)

let payload_typ = typ

module Changes = struct
  type ('amount, 'signed_amount) t_ =
    { sender_delta: 'signed_amount
    ; receiver_increase: 'amount
    ; excess: 'signed_amount
    ; supply_increase: 'amount }
  [@@deriving eq]

  type t = (Amount.t, Amount.Signed.t) t_ [@@deriving eq]

  type var = (Amount.var, Amount.Signed.var) t_

  let to_hlist {sender_delta; receiver_increase; excess; supply_increase} =
    H_list.[sender_delta; receiver_increase; excess; supply_increase]

  let invariant
      ({sender_delta; receiver_increase; excess; supply_increase} : t) =
    let open Amount.Signed in
    let ( + ) x y = Option.value_exn (x + y) in
    let ( - ) x y = x + negate y in
    [%test_eq: Amount.Signed.t] zero
      ( sender_delta
      + of_unsigned receiver_increase
      + excess
      - of_unsigned supply_increase )

  let typ =
    Typ.of_hlistable
      [Amount.Signed.typ; Amount.typ; Amount.Signed.typ; Amount.typ]
      ~var_to_hlist:to_hlist ~value_to_hlist:to_hlist
      ~var_of_hlist:
        (fun H_list.([sender_delta; receiver_increase; excess; supply_increase])
             -> {sender_delta; receiver_increase; excess; supply_increase} )
      ~value_of_hlist:
        (fun H_list.([sender_delta; receiver_increase; excess; supply_increase])
             -> {sender_delta; receiver_increase; excess; supply_increase} )

  let of_payload (payload : payload) : t =
    let tag = payload.body.tag in
    let fee = payload.common.fee in
    let amount = payload.body.amount in
    match tag with
    | Payment ->
        { sender_delta=
            Option.value_exn (Amount.add_fee amount fee)
            |> Amount.Signed.of_unsigned |> Amount.Signed.negate
        ; receiver_increase= amount
        ; excess= Amount.Signed.of_unsigned (Amount.of_fee fee)
        ; supply_increase= Amount.zero }
    | Stake_delegation ->
        { sender_delta=
            Amount.of_fee fee |> Amount.Signed.of_unsigned
            |> Amount.Signed.negate
        ; receiver_increase= Amount.zero
        ; excess= Amount.Signed.of_unsigned (Amount.of_fee fee)
        ; supply_increase= Amount.zero }
    | Fee_transfer ->
        { sender_delta= Amount.Signed.of_unsigned (Amount.of_fee fee)
        ; receiver_increase= amount
        ; excess=
            Option.value_exn (Amount.add_fee amount fee)
            |> Amount.Signed.of_unsigned |> Amount.Signed.negate
        ; supply_increase= Amount.zero }
    | Coinbase ->
        let coinbase_amount = Amount.of_fee fee in
        { sender_delta=
            Amount.sub coinbase_amount amount
            |> Option.value_exn ?here:None ?message:None ?error:None
            |> Amount.Signed.of_unsigned
        ; receiver_increase= amount
        ; excess= Amount.Signed.zero
        ; supply_increase= coinbase_amount }

  let%test_unit "invariant" =
    Quickcheck.test gen ~f:(fun t -> invariant (of_payload t))

  module Checked = struct
    open Let_syntax

    let if_' cond ~then_ ~else_ =
      let%bind e = else_ in
      Amount.Signed.Checked.if_ cond ~then_ ~else_:e

    let of_payload (payload : payload_var) =
      let tag = payload.body.tag in
      let fee = payload.common.fee in
      let amount = payload.body.amount in
      let%bind is_coinbase = Tag.Checked.is_coinbase tag in
      let%bind is_stake_delegation = Tag.Checked.is_stake_delegation tag in
      let%bind is_payment = Tag.Checked.is_payment tag in
      let%bind is_fee_transfer = Tag.Checked.is_fee_transfer tag in
      let%bind is_user_command = Tag.Checked.is_user_command tag in
      let coinbase_amount = Amount.Checked.of_fee fee in
      let%bind supply_increase =
        Amount.Checked.if_ is_coinbase ~then_:coinbase_amount
          ~else_:Amount.(var_of_t zero)
      in
      let%bind receiver_increase =
        Amount.Checked.if_ is_stake_delegation
          ~then_:(Amount.var_of_t Amount.zero)
          ~else_:amount
      in
      let%bind neg_amount_plus_fee =
        let%bind res, `Overflow overflowed =
          Amount.Checked.add_flagged amount (Amount.Checked.of_fee fee)
        in
        let%bind is_fee_transfer_or_payment =
          Boolean.(is_payment || is_fee_transfer)
        in
        let%map () =
          Boolean.Assert.any
            [Boolean.not overflowed; Boolean.not is_fee_transfer_or_payment]
        in
        Amount.Signed.create ~magnitude:res ~sgn:Sgn.Checked.neg
      in
      let pos_fee =
        Amount.Signed.Checked.of_unsigned (Amount.Checked.of_fee fee)
      in
      let neg_fee =
        Amount.Signed.create
          ~magnitude:(Amount.Checked.of_fee fee)
          ~sgn:Sgn.Checked.neg
      in
      let%bind excess =
        let user_command_excess = pos_fee in
        let coinbase_excess = Amount.Signed.(Checked.constant zero) in
        let fee_transfer_excess = neg_amount_plus_fee in
        if_' is_user_command ~then_:user_command_excess
          ~else_:
            (if_' is_coinbase ~then_:coinbase_excess
               ~else_:(return fee_transfer_excess))
      in
      let%bind sender_delta =
        let%bind coinbase_sender_delta =
          with_label __LOC__
            (let%bind proposer_reward, `Underflow underflowed =
               Amount.Checked.sub_flagged coinbase_amount amount
             in
             let%map () =
               Boolean.Assert.any
                 [Boolean.not underflowed; Boolean.not is_coinbase]
             in
             Amount.Signed.Checked.of_unsigned proposer_reward)
        in
        if_' is_stake_delegation ~then_:neg_fee
          ~else_:
            (if_' is_coinbase ~then_:coinbase_sender_delta
               ~else_:
                 (if_' is_fee_transfer ~then_:pos_fee
                    ~else_:(return neg_amount_plus_fee)))
      in
      return {sender_delta; receiver_increase; excess; supply_increase}
  end

  let%test_unit "checked-unchecked changes" =
    Quickcheck.test gen ~trials:100 ~f:(fun (t : payload) ->
        Test_util.test_equal ~equal payload_typ typ Checked.of_payload
          of_payload t )
end

module Checked = struct
  let to_triples ({common; body} : var) =
    let%map body = Body.Checked.to_triples body in
    User_command_payload.Common.Checked.to_triples common @ body

  let constant ({common; body} : t) : var =
    { common= User_command_payload.Common.Checked.constant common
    ; body= Body.Checked.constant body }
end

let excess (payload : t) : Amount.Signed.t =
  let tag = payload.body.tag in
  let fee = payload.common.fee in
  let amount = payload.body.amount in
  match tag with
  | Payment -> Amount.Signed.of_unsigned (Amount.of_fee fee)
  | Stake_delegation -> Amount.Signed.of_unsigned (Amount.of_fee fee)
  | Fee_transfer ->
      Option.value_exn (Amount.add_fee amount fee)
      |> Amount.Signed.of_unsigned |> Amount.Signed.negate
  | Coinbase -> Amount.Signed.zero

let supply_increase (payload : payload) =
  let tag = payload.body.tag in
  let fee = payload.common.fee in
  match tag with
  | Coinbase -> Amount.of_fee fee
  | Payment | Stake_delegation | Fee_transfer -> Amount.zero

let%test_unit "fold_compatibility" =
  Quickcheck.test User_command_payload.gen ~f:(fun t ->
      [%test_eq: bool Triple.t list]
        (Fold.to_list (User_command_payload.fold t))
        (Fold.to_list (fold (of_user_command_payload t))) )
