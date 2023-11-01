Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumCircuitOperator"]

PackageScope["QuantumCircuitOperatorQ"]
PackageScope["BarrierQ"]
PackageScope["FromCircuitOperatorShorthand"]



BarrierQ[barrier_] := MatchQ[barrier, "Barrier" | "Barrier"[_ ? orderQ] | "Barrier"[Span[_Integer, _Integer | All]]]

QuantumCircuitOperatorQ[QuantumCircuitOperator[data_Association]] /; ! AtomQ[Unevaluated[data]] :=
    QuantumCircuitOperatorQ[QuantumCircuitOperator[data]]

QuantumCircuitOperatorQ[QuantumCircuitOperator[KeyValuePattern[{"Elements" -> elements_, "Label" -> _}]]] :=
    AllTrue[elements,
        BarrierQ[#] ||
        QuantumFrameworkOperatorQ[#] &
    ]

QuantumCircuitOperatorQ[___] := False


(* constructors *)

circuitNamePattern = name_String | {name_String, ___} /; MemberQ[$QuantumCircuitOperatorNames, name]

FromCircuitOperatorShorthand[barrier_ ? BarrierQ] := barrier
FromCircuitOperatorShorthand[qc_ ? QuantumCircuitOperatorQ] := qc
FromCircuitOperatorShorthand[qc_ ? QuantumCircuitOperatorQ -> order_ ? orderQ] := QuantumCircuitOperator[qc, order]
FromCircuitOperatorShorthand[arg : circuitNamePattern] := QuantumCircuitOperator[arg]
FromCircuitOperatorShorthand[(arg : circuitNamePattern) -> order_ ? orderQ] :=
    QuantumCircuitOperator[FromCircuitOperatorShorthand[arg], order]
FromCircuitOperatorShorthand["M" | {"M", args___}] := FromCircuitOperatorShorthand[{"M", args} -> {1}]
FromCircuitOperatorShorthand["M" | {"M", args___} -> target_Integer] := FromCircuitOperatorShorthand[{"M", args} -> {target}]
FromCircuitOperatorShorthand["M" | {"M", args___} -> target_ ? orderQ] := QuantumMeasurementOperator[args, target]
FromCircuitOperatorShorthand[op : Except[_ ? QuantumCircuitOperatorQ, _ ? QuantumFrameworkOperatorQ]] := op
FromCircuitOperatorShorthand[arg_] := Replace[FromOperatorShorthand[arg], ops_List :> QuantumCircuitOperator[ops]]


QuantumCircuitOperator[operators_ ? ListQ] := Enclose @ With[{ops = Confirm @* FromCircuitOperatorShorthand /@ operators},
    QuantumCircuitOperator[<|"Elements" -> ops, "Label" -> Replace[RightComposition @@ (#["Label"] & /@ DeleteCases[ops, _ ? BarrierQ]), Identity -> "I"]|>]
]

QuantumCircuitOperator[arg_, order_ ? orderQ, args___] := QuantumCircuitOperator[QuantumCircuitOperator[arg, args], order]

QuantumCircuitOperator[operators_ ? ListQ, label_, ___] :=
    Enclose @ QuantumCircuitOperator[<|"Elements" -> Confirm @* FromCircuitOperatorShorthand /@ operators, "Label" -> label|>]

QuantumCircuitOperator[op : Except[_ ? QuantumCircuitOperatorQ, _ ? QuantumFrameworkOperatorQ], args___] := QuantumCircuitOperator[{op}, args]

QuantumCircuitOperator[op_ ? QuantumCircuitOperatorQ, args__] := QuantumCircuitOperator[op["Elements"], args]

QuantumCircuitOperator[qco_ ? QuantumCircuitOperatorQ | {qco_ ? QuantumCircuitOperatorQ}] := qco

QuantumCircuitOperator[params: Except[{Alternatives @@ $QuantumCircuitOperatorNames, ___}, _List]] :=
    Enclose @ QuantumCircuitOperator[ConfirmBy[QuantumOperator[#], QuantumOperatorQ] & @@ Replace[#, param : Except[_List] :> {param}] & /@ params]

QuantumCircuitOperator[tn_ ? TensorNetworkQ, opts___] := FromTensorNetwork[tn, opts]

QuantumCircuitOperator[arg : Except[_List | _Association]] := QuantumCircuitOperator[{arg}]

QuantumCircuitOperator[] := QuantumCircuitOperator[{}]


(* composition *)

(qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ)[op_ ? QuantumFrameworkOperatorQ] :=
    QuantumCircuitOperator[Prepend[qco["Elements"], op], qco["Label"][op["Label"]]]

Options[quantumCircuitApply] = {Method -> Automatic}

quantumCircuitApply[qco_QuantumCircuitOperator, qs_QuantumState, OptionsPattern[]] /; qco["InputDimensions"] == qs["OutputDimensions"] :=
    Switch[
        OptionValue[Method],
        "Schrodinger" | "Schroedinger" | "Schrödinger",
        Fold[ReverseApplied[Construct], qs, qco["Operators"]],
        Automatic | "TensorNetwork",
        TensorNetworkApply[qco["Flatten"], qs],
        "QuEST",
        QuESTApply[qco, qs],
        "Qiskit",
        qco["Qiskit"][qs],
        "Stabilizer",
        PauliStabilizerApply[qco, qs],
        _,
        $Failed
    ]

QuantumCircuitOperator::dim = "Circuit expecting dimensions `1`, but the state has dimensions `2`."
QuantumCircuitOperator::fail = "Circuit evaluation resulted in a failure: ``."

quantumCircuitApply[qco_QuantumCircuitOperator, qs_QuantumState, OptionsPattern[]] :=
    (Message[QuantumCircuitOperator::dim, qco["InputDimensions"], qs["OutputDimensions"]]; $Failed)


(qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ)[qs_ ? QuantumStateQ, opts : OptionsPattern[quantumCircuitApply]] :=
    With[{result = quantumCircuitApply[
        qco /* QuantumCircuitOperator["I" -> # & /@ qco["FreeOrder"]],
        If[# === {}, qs, QuantumTensorProduct[qs, QuantumState[{"Register", #}]]] & @
            ConstantArray[2, Max[0, Length[qco["FullInputOrder"]] - qs["OutputQudits"]]],
        opts
    ]},
        result /; ! FailureQ[result] || Message[QuantumCircuitOperator::fail, result]
    ]

(qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ)[opts : OptionsPattern[quantumCircuitApply]] := If[
    OptionValue[{opts}, Method] === "Stabilizer",
    PauliStabilizerApply[qco, Automatic],
    (QuantumCircuitOperator[MapThread[QuantumState["Register", #2, "Label" -> Ket[{"0"}]] -> {#1} &, {qco["InputOrder"], qco["InputDimensions"]}]] /* qco)[QuantumState[1, 1]]
]

(qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ)[qm_QuantumMeasurement, opts : OptionsPattern[]] :=
    QuantumMeasurement[qco[qm["QuantumOperator"]]["QuantumOperator", opts]]

op_QuantumMeasurementOperator[qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ] :=
    QuantumCircuitOperator[Append[qco["Elements"], op], op["Label"][qco["Label"]]]


QuantumCircuitOperator /: comp : Composition[___, _QuantumCircuitOperator ? QuantumCircuitOperatorQ, ___] := Enclose @
With[{ops = List @@ Unevaluated[comp]},
    QuantumCircuitOperator[Flatten[ConfirmBy[QuantumCircuitOperator[#], QuantumCircuitOperatorQ]["Elements"] & /@ Reverse @ ops, 1], Composition @@ (#["Label"] & /@ ops)]
]

QuantumCircuitOperator /: comp : RightComposition[___, _QuantumCircuitOperator ? QuantumCircuitOperatorQ, ___] := Enclose @
With[{ops = List @@ Unevaluated[comp]},
    QuantumCircuitOperator[Flatten[ConfirmBy[QuantumCircuitOperator[#], QuantumCircuitOperatorQ]["Elements"] & /@ ops, 1], RightComposition @@ Reverse @ (#["Label"] & /@ ops)]
]


(* dagger *)

SuperDagger[qco_QuantumCircuitOperator] ^:= qco["Dagger"]


(* equality *)

QuantumCircuitOperator /: Equal[left___, qco_QuantumCircuitOperator, right___] :=
    Equal @@ (If[QuantumCircuitOperatorQ[#], #["CircuitOperator"], #] & /@ {left, qco, right})


(* part *)

Part[qco_QuantumCircuitOperator, part_] ^:= QuantumCircuitOperator[qco["Elements"][[part]], qco["Label"]]


(* reorder *)

QuantumCircuitOperator[qc_ ? QuantumCircuitOperatorQ, order_ ? orderQ] := With[{
    repl = Thread[qc["InputOrder"] -> Take[Join[order, Drop[qc["InputOrder"], UpTo[Length[order]]]], UpTo[Length[qc["InputOrder"]]]]]
},
    QuantumCircuitOperator[qc, {qc["OutputOrder"] /. repl, order}]
]

QuantumCircuitOperator[qc_ ? QuantumCircuitOperatorQ, {outOrder_ ? orderQ, inOrder_ ? orderQ}] := With[{
    outRepl = Thread[qc["OutputOrder"] -> Take[Join[outOrder, Drop[qc["OutputOrder"], UpTo[Length[outOrder]]]], UpTo[Length[qc["OutputOrder"]]]]],
    inRepl = Thread[qc["InputOrder"] -> Take[Join[inOrder, Drop[qc["InputOrder"], UpTo[Length[inOrder]]]], UpTo[Length[qc["InputOrder"]]]]]
},
    QuantumCircuitOperator[
        Which[
            BarrierQ[#], # /. inRepl,
            True, Head[#][#, {#["OutputOrder"] /. outRepl, #["InputOrder"] /. inRepl}]
        ] & /@ qc["Elements"],
        qc["Label"]
    ]
]

