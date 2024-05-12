Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumCircuitOperator"]

PackageScope["QuantumCircuitOperatorQ"]
PackageScope["BarrierQ"]
PackageScope["FromCircuitOperatorShorthand"]



BarrierQ[barrier_] := MatchQ[barrier, "Barrier" | "Barrier"[_ ? orderQ, ___] | "Barrier"[Span[_Integer, _Integer | All], ___]]

quantumCircuitOperatorQ[QuantumCircuitOperator[data_Association]] /; ! AtomQ[Unevaluated[data]] :=
    QuantumCircuitOperatorQ[QuantumCircuitOperator[data]]

quantumCircuitOperatorQ[QuantumCircuitOperator[KeyValuePattern[{"Elements" -> elements_}]]] :=
    AllTrue[elements,
        BarrierQ[#] ||
        QuantumFrameworkOperatorQ[#] &
    ]

QuantumCircuitOperatorQ[___] := False

QuantumCircuitOperatorQ[qco_QuantumCircuitOperator] := System`Private`HoldValidQ[qco]

QuantumCircuitOperatorQ[___] := False


qco_QuantumCircuitOperator /; quantumCircuitOperatorQ[Unevaluated[qco]] && ! System`Private`HoldValidQ[qco] := System`Private`HoldSetValid[qco]


Options[QuantumCircuitOperator] := Join[{"Parameters" -> {}}, Options[CircuitDraw]]

(* constructors *)

circuitNamePattern = name_String | {name_String, ___} | name_String[___] /; MemberQ[$QuantumCircuitOperatorNames, name]

FromCircuitOperatorShorthand[barrier_ ? BarrierQ] := barrier
FromCircuitOperatorShorthand[qc_ ? QuantumCircuitOperatorQ] := qc
FromCircuitOperatorShorthand[qc_ ? QuantumCircuitOperatorQ -> order_ ? orderQ] := QuantumCircuitOperator[qc, order]
FromCircuitOperatorShorthand[arg : circuitNamePattern] := QuantumCircuitOperator[arg]
FromCircuitOperatorShorthand[(arg : circuitNamePattern) -> args_] := QuantumCircuitOperator[FromCircuitOperatorShorthand[arg], args]
FromCircuitOperatorShorthand[(arg : circuitNamePattern) -> order_ ? orderQ -> args_] := QuantumCircuitOperator[FromCircuitOperatorShorthand[arg], order, args]
FromCircuitOperatorShorthand["M" | {"M", args___} | "M"[args___]] := FromCircuitOperatorShorthand[{"M", args} -> {1}]
FromCircuitOperatorShorthand["M" | {"M", args___} | "M"[args___] -> target_Integer] := FromCircuitOperatorShorthand[{"M", args} -> {target}]
FromCircuitOperatorShorthand["M" | {"M", args___} | "M"[args___] -> target_ ? orderQ] := QuantumMeasurementOperator[args, target]
FromCircuitOperatorShorthand[op : Except[_ ? QuantumCircuitOperatorQ, _ ? QuantumFrameworkOperatorQ]] := op
FromCircuitOperatorShorthand[arg_] := Enclose @ Replace[Confirm @ FromOperatorShorthand[arg], ops_List :> QuantumCircuitOperator[ops]]


QuantumCircuitOperator[operators_ ? ListQ] := Enclose @ With[{ops = Confirm @* FromCircuitOperatorShorthand /@ operators},
    QuantumCircuitOperator[<|
        "Elements" -> ops, 
        "Label" -> If[# === {} || MemberQ[#, None], None, Composition @@ Reverse @ #] & @ Through[DeleteCases[ops, _ ? BarrierQ]["Label"]]
    |>]
]

QuantumCircuitOperator[arg_, order_ ? orderQ, args___] := QuantumCircuitOperator[QuantumCircuitOperator[arg, args], order]

QuantumCircuitOperator[arg_ -> order_ ? orderQ, args___] := QuantumCircuitOperator[QuantumCircuitOperator[arg, args], order]

QuantumCircuitOperator[operators_ ? ListQ, opts : OptionsPattern[]] := Enclose @ With[{parameters = OptionValue["Parameters"]},
    QuantumCircuitOperator[With[{
        ops = (Confirm[If[parameters === {} || BarrierQ[#], #, Head[#][#, "Parameters" -> Join[#["ParameterSpec"], {parameters}]]] & @ FromCircuitOperatorShorthand[#]] & /@ operators)
    },
        <|
            "Elements" -> ops,
            "Label" -> Replace[Composition @@ Reverse @ Through[DeleteCases[ops, _ ? BarrierQ]["Label"]], Identity -> None],
            DeleteDuplicatesBy[First] @ FilterRules[{opts}, Options[CircuitDraw]]
        |>
    ]]
]


QuantumCircuitOperator[op : Except[_ ? QuantumCircuitOperatorQ | _ ? ListQ, _ ? QuantumFrameworkOperatorQ], args___] := QuantumCircuitOperator[{op}, args]

QuantumCircuitOperator[op_ ? QuantumCircuitOperatorQ, opts : OptionsPattern[]] := QuantumCircuitOperator[op["Elements"], opts, "Label" -> op["Label"]]

QuantumCircuitOperator[qco_ ? QuantumCircuitOperatorQ | {qco_ ? QuantumCircuitOperatorQ}] := qco

QuantumCircuitOperator[params: Except[{Alternatives @@ $QuantumCircuitOperatorNames, ___}, _List]] :=
    Enclose @ QuantumCircuitOperator[ConfirmBy[QuantumOperator[#], QuantumOperatorQ] & @@ Replace[#, param : Except[_List] :> {param}] & /@ params]

QuantumCircuitOperator[tn_ ? GraphQ, opts___] := FromTensorNetwork[tn, opts]

QuantumCircuitOperator[arg : Except[_List | _Association]] := QuantumCircuitOperator[{arg}]

QuantumCircuitOperator[operators_, label_, opts : OptionsPattern[]] := QuantumCircuitOperator[operators, "Label" -> label, opts]

QuantumCircuitOperator[] := QuantumCircuitOperator[{}]


(* composition *)

(qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ)[op_ ? QuantumFrameworkOperatorQ] :=
    QuantumCircuitOperator[Prepend[qco["Elements"], op], Replace[{qco["Label"], op["Label"]}, {{None, _} | {_, None} -> None, {lbl1_, lbl2_} :> lbl1[lbl2]}]]

Options[quantumCircuitApply] := Join[{Method -> Automatic}, Options[TensorNetworkApply]]

quantumCircuitApply[qco_QuantumCircuitOperator, qs_QuantumState, opts : OptionsPattern[]] /; qco["InputDimensions"][[Ordering[qco["InputOrder"]]]] == qs["OutputDimensions"] := Replace[
    OptionValue[Method],
    {
        "Schrodinger" :> Fold[ReverseApplied[Construct], qs, qco["Operators"]],
        Automatic | "TensorNetwork" :> TensorNetworkApply[qco["Flatten"], qs, FilterRules[{opts}, Options[TensorNetworkApply]]],
        "QuEST" :> QuESTApply[qco, qs],
        "Qiskit" | {"Qiskit", subOpts___} :> qco["Qiskit"][qs, subOpts],
        "Stabilizer" :> PauliStabilizerApply[qco, qs],
        _ -> $Failed
    }
]

QuantumCircuitOperator::dim = "Circuit expecting dimensions `1`, but the state has dimensions `2`."

quantumCircuitApply[qco_QuantumCircuitOperator, qs_QuantumState, OptionsPattern[]] :=
    (Message[QuantumCircuitOperator::dim, qco["InputDimensions"], qs["OutputDimensions"]]; $Failed)


(qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ)[qs_QuantumState ? QuantumStateQ, opts : OptionsPattern[quantumCircuitApply]] :=
    quantumCircuitApply[
        qco /* QuantumCircuitOperator[
            Join[
                MapIndexed[
                    If[#2[[1]] <= qs["OutputQudits"], "I"[qs["Output"]["Extract", #2]], "I"] -> #1 &,
                    qco["FreeOrder"]
                ],
                MapIndexed[
                    If[#2[[1]] <= qs["OutputQudits"], "I"[qs["Output"]["Extract", #2 + Length[qco["FullInputOrder"]]]], "I"] -> #1 &,
                    Max[qco["Order"]] + Range[Max[0, qs["OutputQudits"] - Length[qco["FullInputOrder"]]]]
                ]
            ]
        ],
        If[# === {}, qs, QuantumTensorProduct[qs, QuantumState[{"Register", #}]]] & @
            ConstantArray[2, Max[0, Length[qco["FullInputOrder"]] - qs["OutputQudits"]]],
        opts
    ]

(qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ)[opts : OptionsPattern[]] := Switch[
    OptionValue[quantumCircuitApply, {opts}, Method],
    "Stabilizer",
    PauliStabilizerApply[qco, Automatic],
    "QuEST",
    QuESTApply[qco, QuantumState[{"Register", qco["InputDimensions"]}]],
    _,
    (QuantumCircuitOperator[
        Join[
            MapThread[QuantumState["Register", #2, "Label" -> Ket[{"0"}]] -> {#1} &, {qco["InputOrder"], qco["InputDimensions"]}],
            Map[QuantumState["Register", 2, "Label" -> Ket[{"0"}]] -> {#1} &, qco["FreeOrder"]]
        ]
    ] /* qco)[QuantumState[1, 1], opts]
]

(qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ)[qm_QuantumMeasurement, opts : OptionsPattern[]] :=
    QuantumMeasurement[qco[qm["QuantumOperator"]]["QuantumOperator", opts]]

op_QuantumMeasurementOperator[qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ] :=
    QuantumCircuitOperator[Append[qco["Elements"], op], Replace[{op["Label"], qco["Label"]}, {{None, _} | {_, None} -> None, {lbl1_, lbl2_} :> lbl1[lbl2]}]]


QuantumCircuitOperator /: comp : Composition[___, _QuantumCircuitOperator ? QuantumCircuitOperatorQ, ___] := Enclose @
With[{ops = ConfirmBy[QuantumCircuitOperator[#], QuantumCircuitOperatorQ] & /@ List @@ Unevaluated[comp]},
    QuantumCircuitOperator[Flatten[Through[Reverse[ops]["Elements"]], 1], If[# === {} || MemberQ[#, None], None, Composition @@ #] & @ Through[ops["Label"]]]
]

QuantumCircuitOperator /: comp : RightComposition[___, _QuantumCircuitOperator ? QuantumCircuitOperatorQ, ___] := Enclose @
With[{ops = ConfirmBy[QuantumCircuitOperator[#], QuantumCircuitOperatorQ] & /@ List @@ Unevaluated[comp]},
    QuantumCircuitOperator[Flatten[Through[ops["Elements"]], 1], If[# === {} || MemberQ[#, None], None, Composition @@ Reverse @ #] & @ Through[ops["Label"]]]
]


(* dagger *)

SuperDagger[qco_QuantumCircuitOperator] ^:= qco["Dagger"]

SuperStar[qco_QuantumCircuitOperator] ^:= qco["Conjugate"]

Transpose[qco_QuantumCircuitOperator] ^:= qco["Transpose"]


(* equality *)

QuantumCircuitOperator /: Equal[left___, qco_QuantumCircuitOperator, right___] :=
    Equal @@ (If[QuantumCircuitOperatorQ[#], #["CircuitOperator"], #] & /@ {left, qco, right})


(* part *)

Part[qco_QuantumCircuitOperator, part___] ^:= QuantumCircuitOperator[qco["Elements"][[part]], qco["Options"]]


(* reorder *)

QuantumCircuitOperator[qc_ ? QuantumCircuitOperatorQ, order_ ? orderQ] := QuantumCircuitOperator[qc, {order, order}]

QuantumCircuitOperator[qc_ ? QuantumCircuitOperatorQ, order : {_ ? orderQ, _ ? orderQ}] := Block[{
    outOrder = Select[qc["OutputOrder"], Positive],
    inOrder = Select[qc["InputOrder"], Positive],
    outRepl, inRepl
},
    outRepl = Thread[outOrder -> Take[Join[order[[1]], Drop[outOrder, UpTo[Length[order[[1]]]]]], UpTo[Length[outOrder]]]];
    inRepl = Thread[inOrder -> Take[Join[order[[2]], Drop[inOrder, UpTo[Length[order[[2]]]]]], UpTo[Length[inOrder]]]];
    {outRepl, inRepl} = {Join[outRepl, inRepl], Join[inRepl, outRepl]};
    QuantumCircuitOperator[
        Which[
            BarrierQ[#], # /. inRepl,
            True, With[{newOrder = {#["OutputOrder"] /. outRepl, #["InputOrder"] /. inRepl}},
                If[QuantumOperatorQ[#], #["Reorder", newOrder], Head[#][#, newOrder]]
            ]
        ] & /@ qc["Elements"],
        qc["Options"]
    ]
]

(* simplify *)

Scan[
    (Symbol[#][qco_QuantumCircuitOperator, args___] ^:= qco[#, args]) &,
    {"Simplify", "FullSimplify", "Chop", "ComplexExpand"}
]


(* parameterization *)

(qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ)[rules_ ? AssociationQ] :=
    QuantumCircuitOperator[
        If[BarrierQ[#] || #["ParameterArity"] == 0, #, #[KeyTake[rules, #["Parameters"]]]] & /@ qco["Elements"],
        qco["Label"] /. rules
    ] /; ContainsOnly[Keys[rules], qco["Parameters"]]

(qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ)[parameters : PatternSequence[p : Except[_Association], ___]] /; ! MemberQ[QuantumCircuitOperator["Properties"], p] /; Length[{parameters}] <= Length[qco["Parameters"]] :=
    qco[AssociationThread[Take[qco["Parameters"], UpTo[Length[{parameters}]]], {parameters}]]

