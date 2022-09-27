Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumCircuitOperator"]

PackageScope["QuantumCircuitOperatorQ"]



QuantumCircuitOperatorQ[QuantumCircuitOperator[KeyValuePattern[{"Operators" -> operators_, "Label" -> _}]]] :=
    VectorQ[Unevaluated @ operators, QuantumFrameworkOperatorQ]

QuantumCircuitOperatorQ[___] := False


(* constructors *)

toOperators[op_ ? QuantumFrameworkOperatorQ] := op
toOperators[order_ ? orderQ] := QuantumMeasurementOperator[order]
toOperators[{name_, args___}] /; MemberQ[$QuantumOperatorNames, name] := QuantumOperator[{name, args}]
toOperators[{name_, args___} -> order_ ? orderQ] /; MemberQ[$QuantumOperatorNames, name] := QuantumOperator[{name, args}, order]
toOperators[{name_, args___} -> args_List] /; MemberQ[$QuantumOperatorNames, name] := QuantumOperator[{name, args}, Sequence @@ args]
toOperators[lhs_ -> order_ ? orderQ] := QuantumOperator[lhs, order]
toOperators[lhs_ -> args_List] := QuantumOperator[lhs, Sequence @@ args]
toOperators[l_List] := QuantumCircuitOperator[toOperators /@ l]
toOperators[arg_] := QuantumOperator[arg]


QuantumCircuitOperator[operators_ ? ListQ] := With[{ops = toOperators /@ operators},
    QuantumCircuitOperator[<|"Operators" -> ops, "Label" -> RightComposition @@ (#["Label"] & /@ ops)|>]
]

QuantumCircuitOperator[operators_ ? ListQ, label_, ___] :=
    QuantumCircuitOperator[<|"Operators" -> toOperators /@ operators, "Label" -> label|>]

QuantumCircuitOperator[op : Except[_ ? QuantumCircuitOperatorQ, _ ? QuantumFrameworkOperatorQ], args___] := QuantumCircuitOperator[{op}, args]

QuantumCircuitOperator[op_ ? QuantumCircuitOperatorQ, args__] := QuantumCircuitOperator[op["Operators"], args]

QuantumCircuitOperator[qco_ ? QuantumCircuitOperatorQ | {qco_ ? QuantumCircuitOperatorQ}] := qco

QuantumCircuitOperator[params: Except[{Alternatives @@ $QuantumCircuitOperatorNames, ___}, _List]] :=
    Enclose @ QuantumCircuitOperator[ConfirmBy[QuantumOperator[#], QuantumOperatorQ] & @@ Replace[#, param : Except[_List] :> {param}] & /@ params]


(* composition *)

(qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ)[op_ ? QuantumFrameworkOperatorQ] :=
    QuantumCircuitOperator[Prepend[qco["Operators"], op], qco["Label"][op["Label"]]]

Options[quantumCircuitApply] = {Method -> Automatic}

quantumCircuitApply[qco_QuantumCircuitOperator, qs_QuantumState, OptionsPattern[]] /; qco["Arity"] == qs["OutputQudits"] :=
    Switch[
        OptionValue[Method],
        "Schrodinger" | "Schroedinger" | "Schrödinger",
        Fold[ReverseApplied[Construct], qs, qco["Operators"]],
        Automatic | "TensorNetwork",
        Block[{
            state = If[
                qs["PureStateQ"],
                QuantumState[SparseArrayFlatten[#], TensorDimensions[#], "Label" -> qs["Label"] /* qco["Label"]] & @
                    ContractTensorNetwork @ InitializeTensorNetwork[
                        qco["TensorNetwork"],
                        qs["Computational"]["Tensor"],
                        Join[Superscript[0, #] & /@ qco["InputOrder"], Subscript[0, #] & /@ (Range[qs["InputQudits"]])]
                    ],
                QuantumState[
                    quantumCircuitApply[QuantumTensorProduct[qco, qco["Conjugate"]], qs["Bend"]]["State"]["PermuteOutput",
                        With[{a = qco["Eigenqudits"] + qco["TraceQudits"], b = qs["Qudits"]},
                           FindPermutation[
                                Join[Array[0, a], Array[1, a], Array[2, b], Array[3, b]],
                                Join[Array[0, a], Array[2, b], Array[1, a], Array[3, b]]
                            ]
                        ]]["Unbend"],
                    "Label" -> qs["Label"] /* qco["Label"]
                ]
            ]
        },
            If[ qco["Channels"] > 0,
                state = QuantumPartialTrace[state,
                    First @ Fold[
                        {
                            Join[#1[[1]], If[QuantumChannelQ[#2], #1[[2]] + Range[#2["TraceQudits"]], {}]],
                             #1[[2]] + Which[QuantumChannelQ[#2], #2["TraceQudits"], QuantumMeasurementOperatorQ[#2], #2["Eigenqudits"], True, 0]
                        } &,
                        {{}, 0},
                        qco["Operators"]
                    ]
                ]
            ];
            If[ qco["Measurements"] > 0,
                QuantumMeasurement[
                    QuantumMeasurementOperator[
                        QuantumOperator[
                            state,
                            {
                                Join[Range[- qco["Eigenqudits"] + 1, 0], DeleteCases[qco["OutputOrder"], _ ? NonPositive]],
                                qco["InputOrder"]
                            }
                        ],
                        qco["Target"]
                    ],
                    Fold[
                        ReverseApplied[Construct],
                        Prepend[
                            Select[qco["Operators"] , QuantumMeasurementOperatorQ],
                            QuantumOperator["I", Complement[Range[qco["Arity"]], qco["Target"]]]
                        ]
                    ]["POVM"]["Sort"]["OutputBasis"]
                ],
                state
            ]
        ],
        _,
        $Failed
    ]

(qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ)[qs_ ? QuantumStateQ, opts : OptionsPattern[quantumCircuitApply]] := quantumCircuitApply[qco, qs, opts]

(qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ)[opts : OptionsPattern[quantumCircuitApply]] := qco[QuantumState[{"Register", qco["InputDimensions"]}], opts]


op_QuantumMeasurementOperator[qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ] :=
    QuantumCircuitOperator[Append[qco["Operators"], op], op["Label"][qco["Label"]]]


QuantumCircuitOperator /: comp : Composition[___ ? QuantumFrameworkOperatorQ, _QuantumCircuitOperator ? QuantumCircuitOperatorQ, ___ ? QuantumFrameworkOperatorQ] :=
With[{ops = List @@ Unevaluated[comp]},
    QuantumCircuitOperator[Flatten[Replace[qco_QuantumCircuitOperator :> qco["Operators"]] /@ Reverse @ ops, 1], Composition @@ (#["Label"] & /@ ops)]
]

QuantumCircuitOperator /: comp : RightComposition[___ ? QuantumFrameworkOperatorQ, _QuantumCircuitOperator ? QuantumCircuitOperatorQ, ___ ? QuantumFrameworkOperatorQ] :=
With[{ops = List @@ Unevaluated[comp]},
    QuantumCircuitOperator[Flatten[Replace[qco_QuantumCircuitOperator :> qco["Operators"]] /@ ops, 1], RightComposition @@ Reverse @ (#["Label"] & /@ ops)]
]


(* equality *)

QuantumCircuitOperator /: Equal[left___, qco_QuantumCircuitOperator, right___] :=
    Equal @@ (If[QuantumCircuitOperatorQ[#], #["CircuitOperator"], #] & /@ {left, qco, right})


(* part *)

Part[qco_QuantumCircuitOperator, part_] ^:= QuantumCircuitOperator[qco["Operators"][[part]], qco["Label"]]

