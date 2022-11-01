Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumCircuitOperator"]

PackageScope["QuantumCircuitOperatorQ"]
PackageScope["BarrierQ"]
PackageScope["circuitElementOrder"]



BarrierQ[barrier_] := MatchQ[barrier, "Barrier" | "Barrier"[_ ? orderQ] | "Barrier"[_Span]]

QuantumCircuitOperatorQ[QuantumCircuitOperator[data_Association]] /; ! AtomQ[Unevaluated[data]] :=
    QuantumCircuitOperatorQ[QuantumCircuitOperator[data]]

QuantumCircuitOperatorQ[QuantumCircuitOperator[KeyValuePattern[{"Elements" -> elements_, "Label" -> _}]]] :=
    AllTrue[elements,
        BarrierQ[#] ||
        QuantumOperatorQ[#] && AllTrue[Join @@ #["Order"], GreaterEqualThan[1]] ||
        QuantumMeasurementOperatorQ[#] && AllTrue[#["InputOrder"], GreaterEqualThan[1]] ||
        QuantumChannelQ[#] && AllTrue[#["InputOrder"], GreaterEqualThan[1]] ||
        QuantumCircuitOperatorQ[#] &
    ]

QuantumCircuitOperatorQ[___] := False

circuitElementOrder["Barrier", width_] := Range[width]
circuitElementOrder["Barrier"[order_ ? orderQ], width_] := Clip[order, {1, width}]
circuitElementOrder["Barrier"[span_Span], width_] := Enclose[
    Clip[Range @@ Confirm @ ResourceFunction["SpanRange"][span, width], {1, width}],
    Range[width] &
]
circuitElementOrder[op_, _] := op["InputOrder"]


(* constructors *)

FromCircuitOperatorShorthand[barrier_ ? BarrierQ] := barrier
FromCircuitOperatorShorthand[arg : name_String | {name_String, ___}] /; MemberQ[$QuantumCircuitOperatorNames, name] := QuantumCircuitOperator[arg]
FromCircuitOperatorShorthand[arg : (name_String | {name_String, ___} /; MemberQ[$QuantumCircuitOperatorNames, name]) -> order_ ? orderQ] :=
    QuantumCircuitOperator[FromCircuitOperatorShorthand[arg], order]
FromCircuitOperatorShorthand[arg_] := Replace[FromOperatorShorthand[arg], ops_List :> QuantumCircuitOperator[ops]]


QuantumCircuitOperator[operators_ ? ListQ] := Enclose @ With[{ops = Confirm @* FromCircuitOperatorShorthand /@ operators},
    QuantumCircuitOperator[<|"Elements" -> ops, "Label" -> RightComposition @@ (#["Label"] & /@ DeleteCases[ops, _ ? BarrierQ])|>]
]

QuantumCircuitOperator[arg_, order_ ? orderQ, args___] := QuantumCircuitOperator[QuantumCircuitOperator[arg, args], order]

QuantumCircuitOperator[operators_ ? ListQ, label_, ___] :=
    Enclose @ QuantumCircuitOperator[<|"Elements" -> Confirm @* FromCircuitOperatorShorthand /@ operators, "Label" -> label|>]

QuantumCircuitOperator[op : Except[_ ? QuantumCircuitOperatorQ, _ ? QuantumFrameworkOperatorQ], args___] := QuantumCircuitOperator[{op}, args]

QuantumCircuitOperator[op_ ? QuantumCircuitOperatorQ, args__] := QuantumCircuitOperator[op["Elements"], args]

QuantumCircuitOperator[qco_ ? QuantumCircuitOperatorQ | {qco_ ? QuantumCircuitOperatorQ}] := qco

QuantumCircuitOperator[params: Except[{Alternatives @@ $QuantumCircuitOperatorNames, ___}, _List]] :=
    Enclose @ QuantumCircuitOperator[ConfirmBy[QuantumOperator[#], QuantumOperatorQ] & @@ Replace[#, param : Except[_List] :> {param}] & /@ params]


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
        TensorNetworkApply[qco, qs],
        "QuEST",
        QuESTApply[qco, qs],
        "Qiskit",
        qco["Qiskit"][qs],
        _,
        $Failed
    ]

QuantumCircuitOperator::dim = "Circuit expecting dimensions `1`, but the state has dimensions `2`."

quantumCircuitApply[qco_QuantumCircuitOperator, qs_QuantumState, OptionsPattern[]] :=
    (Message[QuantumCircuitOperator::dim, qco["InputDimensions"], qs["OutputDimensions"]]; $Failed)

(qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ)[qs_ ? QuantumStateQ, opts : OptionsPattern[quantumCircuitApply]] :=
    With[{result = quantumCircuitApply[qco, qs, opts]},
        result /; ! FailureQ[result]
    ]

(qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ)[opts : OptionsPattern[quantumCircuitApply]] := qco[QuantumState[{"Register", qco["InputDimensions"]}], opts]


op_QuantumMeasurementOperator[qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ] :=
    QuantumCircuitOperator[Append[qco["Elements"], op], op["Label"][qco["Label"]]]


QuantumCircuitOperator /: comp : Composition[___ ? QuantumFrameworkOperatorQ, _QuantumCircuitOperator ? QuantumCircuitOperatorQ, ___ ? QuantumFrameworkOperatorQ] :=
With[{ops = List @@ Unevaluated[comp]},
    QuantumCircuitOperator[Flatten[Replace[qco_QuantumCircuitOperator :> qco["Elements"]] /@ Reverse @ ops, 1], Composition @@ (#["Label"] & /@ ops)]
]

QuantumCircuitOperator /: comp : RightComposition[___ ? QuantumFrameworkOperatorQ, _QuantumCircuitOperator ? QuantumCircuitOperatorQ, ___ ? QuantumFrameworkOperatorQ] :=
With[{ops = List @@ Unevaluated[comp]},
    QuantumCircuitOperator[Flatten[Replace[qco_QuantumCircuitOperator :> qco["Elements"]] /@ ops, 1], RightComposition @@ Reverse @ (#["Label"] & /@ ops)]
]


(* equality *)

QuantumCircuitOperator /: Equal[left___, qco_QuantumCircuitOperator, right___] :=
    Equal @@ (If[QuantumCircuitOperatorQ[#], #["CircuitOperator"], #] & /@ {left, qco, right})


(* part *)

Part[qco_QuantumCircuitOperator, part_] ^:= QuantumCircuitOperator[qco["Elements"][[part]], qco["Label"]]


(* reorder *)

QuantumCircuitOperator[qc_ ? QuantumCircuitOperatorQ, order_ ? orderQ] := With[{
    repl = Thread[qc["InputOrder"] -> Take[Join[order, Drop[qc["InputOrder"], UpTo[Length[order]]]], UpTo[Length[qc["InputOrder"]]]]]
},
    QuantumCircuitOperator[
        Which[
            BarrierQ[#], # /. repl,
            QuantumCircuitOperatorQ[#], QuantumCircuitOperator[#, order],
            True, Head[#][#, #["Order"] /. repl]
        ] & /@ qc["Elements"],
        qc["Label"]
    ]
]

