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
                target
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
        PatternSequence[Except[_ ? QuantumFrameworkOperatorQ | (tensor_ ? TensorQ /; 2 <= TensorRank[tensor] <= 3)], ___],
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
        qo,
        Cases[qo["FullInputOrder"], Alternatives @@ target] /. {} -> Automatic
    ]

QuantumMeasurementOperator[qo_ ? QuantumOperatorQ, target_ ? orderQ] /; qo["InputOrder"] != qo["FullInputOrder"] :=
    QuantumMeasurementOperator[
        QuantumOperator[qo, qo["FullInputOrder"]],
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


(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[qm : _ ? QuantumMeasurementOperatorQ | _ ? QuantumMeasurementQ] := Enclose @ Module[{
    top, bottom, topIsLeft, result
},

    top = qmo["SuperOperator"];
    bottom = If[
        QuantumMeasurementOperatorQ[qm],
        qm["SuperOperator"],
        QuantumOperator[
            QuantumState[qm["State"], QuantumBasis[qm["Basis"], "Input" -> qm["Input"]["Dual"]]][
                {"Split", qm["Qudits"]}
            ],
            {Range[qm["Qudits"]] - qm["Eigenqudits"], Automatic}
        ]
    ];

    (* depending on relative positions of superoperator eigenqudits,
        change the right-most operator's eigenqudit order being to the left
        of the left-most operator
    *)
    topIsLeft = top["FirstOutputQudit"] + qmo["Eigenqudits"] <= bottom["FirstOutputQudit"] + qm["Eigenqudits"];
    If[ topIsLeft,
        bottom = QuantumOperator[bottom, {
            ReplacePart[bottom["FullOutputOrder"], Thread[List /@ Range[qm["Eigenqudits"]] -> top["FirstOutputQudit"] - Reverse @ Range[qm["Eigenqudits"]]]],
            bottom["InputOrder"]
            }
        ],
        top = QuantumOperator[top, {
            ReplacePart[top["FullOutputOrder"], Thread[List /@ Range[qmo["Eigenqudits"]] -> bottom["FirstOutputQudit"] - Reverse @ Range[qmo["Eigenqudits"]]]],
            top["InputOrder"]
            }
        ]
    ];
    result = top[bottom]["Sort"][
        (* permute two sets of eigenqudits based on given operator targets, left-most first *)
        {"PermuteOutput", InversePermutation @ FindPermutation[DeleteDuplicates @
            If[topIsLeft, Join[qm["Target"], qmo["Target"]], Join[qmo["Target"], qm["Target"]]]]}
    ];
    If[ QuantumMeasurementOperatorQ[qm],
        QuantumMeasurementOperator[
            result,
            Union[qmo["Target"], qm["Target"]]
        ],
        QuantumMeasurement[
            QuantumState[
                result["State"][{"Split", qmo["Eigenqudits"] + qm["Eigenqudits"]}],
                "Label" -> qmo["Label"][qm["Label"]]
            ],
            Union[qmo["Target"], qm["Target"]]
        ]
    ]
]


(* equality *)

QuantumMeasurementOperator /: Equal[qmo : _QuantumMeasurementOperator ... ] :=
    Equal @@ (#["Picture"] & /@ {qmo}) && Equal @@ (#["OrderedMatrixRepresentation"] & /@ {qmo})

