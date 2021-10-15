Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumMeasurementOperator"]

PackageScope["QuantumMeasurementOperatorQ"]



QuantumMeasurementOperatorQ[QuantumMeasurementOperator[qo_, _ ? orderQ]] := QuantumOperatorQ[qo]

QuantumMeasurementOperatorQ[___] := False


(* constructors *)

QuantumMeasurementOperator[qb_ ? QuantumBasisQ -> eigenvalues_ ? VectorQ, args___, target : (_ ? orderQ) : {1}] := Enclose @
    QuantumMeasurementOperator[
        ConfirmBy[
            QuantumOperator[
                QuantumOperator[
                    PadRight[eigenvalues, qb["Dimension"]] . qb["Projectors"],
                    QuantumBasis[qb["OutputDimensions"], qb["InputDimensions"]],
                    args
                ],
                qb,
                With[{order = Join[target, Complement[Max[target] + 1 - Range[If[qb["HasInputQ"], qb["InputQudits"], qb["OutputQudits"]]], target]]},
                    Sort[order + Max[1 - order, 0]]
                ]
            ],
            QuantumOperatorQ
        ],
        target
    ]

QuantumMeasurementOperator[arg_ -> eigenvalues_ ? VectorQ, args___] :=
    Enclose @ QuantumMeasurementOperator[ConfirmBy[QuantumBasis[arg], QuantumBasisQ] -> eigenvalues, args]

QuantumMeasurementOperator[qo_ ? QuantumOperatorQ, args : PatternSequence[] | PatternSequence[___, Except[_ ? orderQ]]] :=
    QuantumMeasurementOperator[QuantumOperator[qo, args, qo["FullInputOrder"], qo["OutputOrder"]], qo["InputOrder"]]

QuantumMeasurementOperator[qo_ ? QuantumOperatorQ, args__, target_ ? orderQ] :=
    QuantumMeasurementOperator[
        With[{op = QuantumOperator[qo, args]},
            QuantumOperator[op, Sort @ Join[target, Complement[op["InputQuditOrder"] + Min[target] - 1, target]]]
        ],
        target
    ]

QuantumMeasurementOperator[tensor_ ? TensorQ /; 2 <= TensorRank[tensor] <= 3, args___] :=
    QuantumMeasurementOperator[QuantumOperator[tensor], args]

QuantumMeasurementOperator[tensor_ ? TensorQ /; 2 <= TensorRank[tensor] <= 3, qb_ ? QuantumBasisQ, args___] :=
    QuantumMeasurementOperator[QuantumOperator[tensor, qb], args]

QuantumMeasurementOperator[
    args : PatternSequence[] | 
        PatternSequence[Except[_ ? QuantumOperatorQ | (tensor_ ? TensorQ /; 2 <= TensorRank[tensor] <= 3)], ___],
    target : (_ ? orderQ) : {1}] :=
Enclose @ Module[{
    basis = ConfirmBy[QuantumBasis[args], QuantumBasisQ]
},
    If[ basis["Qudits"] < Length[target],
        basis = QuantumBasis[basis, Ceiling[Length[target] / basis["Qudits"]]]
    ];
    QuantumMeasurementOperator[basis -> Range[0, basis["Dimension"] - 1], target]
]


(* mutation *)

QuantumMeasurementOperator[qmo_ ? QuantumMeasurementOperatorQ, args : PatternSequence[___, Except[_ ? orderQ]]] :=
    QuantumMeasurementOperator[QuantumOperator[qmo["Operator"], args], qmo["Target"]]

QuantumMeasurementOperator[qmo_ ? QuantumMeasurementOperatorQ, args___, target_ ? orderQ] :=
    QuantumMeasurementOperator[
        If[ContainsAll[qmo["Operator"]["InputOrder"], target], QuantumOperator[qmo["Operator"], args], QuantumOperator[qmo["Operator"], args, target]],
        target
    ]

(* auto reassign bad target *)
QuantumMeasurementOperator[qo_ ? QuantumOperatorQ, target_ ? orderQ] /; !ContainsAll[qo["FullInputOrder"], target] :=
    QuantumMeasurementOperator[
        QuantumOperator[qo, Automatic, Join[target, Complement[qo["InputQuditOrder"] + Min[target] - 1, target]]],
        target
    ]


(* composition *)

(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[qs_ ? QuantumStateQ] := Enclose @ With[{
    qudits = qmo["Eigenqudits"],
    op = qmo["SuperOperator"]
},
    QuantumMeasurement[
        QuantumState[
            ConfirmBy[
                QuantumOperator[op,
                    ReplacePart[op["FullOutputOrder"], Thread[List /@ Range[qudits] -> 1 - Reverse @ Range[qudits]]],
                    op["InputOrder"]
                ][
                    {"OrderedInput", Range[qs["OutputQudits"]], qs["Output"]}
                ] @ qs,
                QuantumStateQ
            ][
                {"Split", qudits}
            ],
            "Label" -> qmo["Label"][qs["Label"]]
        ],
        qmo["Target"]
    ]
]

(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[qo_ ? QuantumOperatorQ] := With[{
    op = qmo["SuperOperator"]
},
    QuantumMeasurementOperator[
        QuantumOperator[op, {
            ReplacePart[op["FullOutputOrder"], Thread[List /@ Range[qmo["Eigenqudits"]] -> qo["FirstOutputQudit"] - Reverse @ Range[qmo["Eigenqudits"]]]],
            op["InputOrder"]
        }] @ qo,
        qmo["Target"]
    ]
]


(qmo1_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[qmo2_ ? QuantumMeasurementOperatorQ] := Enclose @ Module[{
    top, bottom
},

    top = qmo1["SuperOperator"];
    bottom = qmo2["SuperOperator"];

    If[ top["FirstOutputQudit"] + qmo1["Eigenqudits"] <= bottom["FirstOutputQudit"] + qmo2["Eigenqudits"],
        bottom = QuantumOperator[bottom, {
            ReplacePart[bottom["FullOutputOrder"], Thread[List /@ Range[qmo2["Eigenqudits"]] -> top["FirstOutputQudit"] - Reverse @ Range[qmo2["Eigenqudits"]]]],
            bottom["InputOrder"]
            }
        ],
        top = QuantumOperator[top, {
            ReplacePart[top["FullOutputOrder"], Thread[List /@ Range[qmo1["Eigenqudits"]] -> bottom["FirstOutputQudit"] - Reverse @ Range[qmo1["Eigenqudits"]]]],
            top["InputOrder"]
            }
        ]
    ];
    QuantumMeasurementOperator[
        top[bottom] //
            (* permute two sets of measured qudits based on given operator targets, left-most first *)
            (#[{"PermuteOutput", InversePermutation @ FindPermutation[DeleteDuplicates @
                Join[qmo1["Target"], qmo2["Target"]]]}] &)
            ,
        "Label" -> qmo1["Label"] @* qmo2["Label"],
        Union[qmo1["Target"], qmo2["Target"]]
    ]
]


(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[qm_QuantumMeasurement] := Enclose @ With[{
    state =
        (* prepending identity to propogate measurement eigenvalues *)
        ConfirmBy[
            QuantumTensorProduct[
                QuantumOperator[{"Identity",
                    First @ qm["Output"][{"Split", qm["Targets"]}]},
                    Range[qmo["FirstOutputQudit"] - qm["Targets"], qmo["FirstOutputQudit"] - 1]],
                With[{op = qmo["SuperOperator"]}, QuantumOperator[op,
                    ReplacePart[op["FullOutputOrder"], Thread[List /@ Range[qmo["Targets"]] -> 1 - qm["Targets"] - Reverse @ Range[qmo["Targets"]]]],
                    op["InputOrder"]
                ]][{"OrderedInput", Range[qm["InputQudits"]], qm["Input"]["Dual"]}]
            ],
            QuantumOperatorQ
        ][
            qm["State"][{"Split", qm["Qudits"]}]
        ],
    target = Union[qm["Target"], qmo["Target"]]
},
    QuantumMeasurement[
        QuantumState[
            state[{"PermuteOutput", InversePermutation @ FindPermutation @ Join[qm["Target"], qmo["Target"]]}][{"Split", Length @ target}],
            "Label" -> qmo["Label"] @ qm["Label"]
        ],
        target
    ]
]


(* equality *)

QuantumMeasurementOperator /: Equal[qmo : _QuantumMeasurementOperator ... ] :=
    Equal @@ (#["Picture"] & /@ {qmo}) && Equal @@ (#["OrderedMatrixRepresentation"] & /@ {qmo})

