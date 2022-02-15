Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumMeasurementOperator"]

PackageScope["QuantumMeasurementOperatorQ"]



QuantumMeasurementOperatorQ[QuantumMeasurementOperator[qo_, _ ? orderQ]] := QuantumOperatorQ[qo]

QuantumMeasurementOperatorQ[___] := False


(* constructors *)

SetAttributes[QuantumMeasurementOperator, NHoldRest]

QuantumMeasurementOperator[arg_ -> eigenvalues_ ? VectorQ, args___] :=
    Enclose @ QuantumMeasurementOperator[ConfirmBy[QuantumBasis[arg], QuantumBasisQ] -> eigenvalues, args]

QuantumMeasurementOperator[args : PatternSequence[Except[_ ? QuantumFrameworkOperatorQ | _ ? QuantumBasisQ], ___], target_ ? orderQ] :=
    Enclose @ With[{qb = ConfirmBy[QuantumBasis[args], QuantumBasisQ]}, QuantumMeasurementOperator[qb, target]]

QuantumMeasurementOperator[target : _ ? orderQ] :=
    QuantumMeasurementOperator[QuantumBasis[], target]

QuantumMeasurementOperator[] := QuantumMeasurementOperator[{1}]

QuantumMeasurementOperator[qb_ ? QuantumBasisQ -> eigenvalues_ ? VectorQ, target : (_ ? orderQ) : Automatic, args___] := Enclose @ Module[{
    basis, op
},
    basis = If[ target === Automatic,
        qb,
        QuantumBasis[qb, Ceiling[Length[target] / qb["OutputQudits"]]]
    ];
    basis = QuantumBasis[basis, Ceiling[Length[eigenvalues] / basis["Dimension"]]];
    op = ConfirmBy[
        QuantumOperator[
            QuantumOperator[
                PadRight[eigenvalues, basis["Dimension"]] . basis["Projectors"],
                # - Min[#, 1] + 1 &[Max[Replace[target, Automatic -> 0]] - Reverse @ Range[basis["OutputQudits"]] + 1],
                QuantumBasis[basis["OutputDimensions"], basis["InputDimensions"]]
            ],
            basis
        ],
        QuantumOperatorQ
    ];
    QuantumMeasurementOperator[
        op,
        Replace[target, Automatic -> op["InputOrder"]],
        args
    ]
]

QuantumMeasurementOperator[qo_ ? QuantumOperatorQ, Automatic] := QuantumMeasurementOperator[qo, qo["InputOrder"]]

QuantumMeasurementOperator[qo_ ? QuantumOperatorQ] := QuantumMeasurementOperator[qo, Automatic]

QuantumMeasurementOperator[qo_ ? QuantumOperatorQ, args : PatternSequence[___, Except[_ ? orderQ]]] :=
    QuantumMeasurementOperator[QuantumOperator[qo, {qo["OutputOrder"], qo["FullInputOrder"]}, args], qo["InputOrder"]]

QuantumMeasurementOperator[qo_ ? QuantumOperatorQ, args__, target_ ? orderQ] :=
    QuantumMeasurementOperator[QuantumOperator[qo, {qo["OutputOrder"], qo["FullInputOrder"]}, args], target]


QuantumMeasurementOperator[qo_ ? QuantumOperatorQ, target : _ ? orderQ, args__] :=
    QuantumMeasurementOperator[
        With[{op = QuantumOperator[qo, args]},
            QuantumOperator[op, Sort @ Join[target, Complement[op["InputQuditOrder"] + Min[target] - 1, target]]]
        ],
        target
    ]

QuantumMeasurementOperator[tensor_ ? TensorQ /; 2 <= TensorRank[tensor] <= 3, target : (_ ? orderQ) : {1}, args___] :=
    QuantumMeasurementOperator[QuantumOperator[tensor], target, "Label" -> "Eigen", args]

QuantumMeasurementOperator[tensor_ ? TensorQ /; 2 <= TensorRank[tensor] <= 3, qb_ ? QuantumBasisQ, target : (_ ? orderQ) : {1}, args___] :=
    QuantumMeasurementOperator[QuantumOperator[tensor, qb], target, "Label" -> "Eigen", args]

QuantumMeasurementOperator[
    qb_ ? QuantumBasisQ,
    target : _ ? orderQ : {1},
    args___
    ] :=
Enclose @ Module[{
    basis = ConfirmBy[QuantumBasis[qb, args], QuantumBasisQ]
},
    If[ basis["Qudits"] < Length[target],
        basis = QuantumBasis[basis, Ceiling[Length[target] / basis["Qudits"]]]
    ];
    QuantumMeasurementOperator[basis -> Range[0, basis["Dimension"] - 1], target]
]

QuantumMeasurementOperator[args : PatternSequence[Except[_ ? QuantumFrameworkOperatorQ], ___]] := QuantumMeasurementOperator[QuantumBasis[args]]


(* mutation *)

QuantumMeasurementOperator[qmo_ ? QuantumMeasurementOperatorQ, args : PatternSequence[___, Except[_ ? orderQ]]] :=
    QuantumMeasurementOperator[QuantumOperator[qmo["Operator"], args], qmo["Target"]]

QuantumMeasurementOperator[qmo_ ? QuantumMeasurementOperatorQ, t : _ ? orderQ : Automatic, args___] := With[{
    target = Replace[t, Automatic -> qmo["Target"]]
},
    QuantumMeasurementOperator[
        If[ContainsAll[qmo["Operator"]["InputOrder"], target], QuantumOperator[qmo["Operator"], args], QuantumOperator[qmo["Operator"], target, args]],
        target
    ]
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
        QuantumOperator[#, "Label" -> qmo["Label"][qs["Label"]]] & @ QuantumOperator[
            ConfirmBy[
                QuantumOperator[op,
                    (* shove away eigen qudits to the left *)
                    ReplacePart[op["FullOutputOrder"], Thread[List /@ Range[qudits] -> 1 - Reverse @ Range[qudits]]],
                    op["FullInputOrder"] - Max[Max[op["FullInputOrder"]] - qs["OutputQudits"], 0]
                ][
                    {
                        "OrderedInput",
                        Range @ qs["OutputQudits"],
                        qs["Output"]
                    }
                ] @ qs,
                QuantumStateQ
            ][{"Split", qudits}],
            {
                Take[op["FullOutputOrder"], UpTo[qudits]],
                # - Min[#] + 1 &[Max[op["FullInputOrder"]] - Reverse @ Range[qs["OutputQudits"]] + 1]
            }
        ],
        If[ op["OutputQudits"] <= op["InputQudits"],
            Range[qmo["Eigenqudits"]],
            qmo["Target"]
        ]
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
        If[qo["OutputQudits"] < qo["InputQudits"] && qo["OutputDimension"] == qo["InputDimension"], Sort @ qo["FullInputOrder"], qmo["Target"]]
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
        QuantumMeasurement @ QuantumMeasurementOperator[QuantumOperator[
                result["State"][{"Split", qmo["Eigenqudits"] + qm["Eigenqudits"]}],
                TakeDrop[result["FullOutputOrder"], qmo["Eigenqudits"] + qm["Eigenqudits"]],
                "Label" -> qmo["Label"][qm["Label"]]
            ],
            Union[qmo["Target"], qm["Target"]]
        ]
    ]
]

(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ] :=
    QuantumCircuitOperator[Append[qco["Operators"], qmo]]



QuantumMeasurementOperator /: (qmo1_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ) + (qmo2_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ) /;
    qmo1["Dimension"] == qmo2["Dimension"] && qmo1["Order"] == qmo2["Order"] && qmo1["Target"] == qmo2["Target"] :=
    QuantumMeasurementOperator[
        qmo1["Operator"] + qmo2["Operator"],
        qmo1["Target"]
    ]

QuantumMeasurementOperator /: f_Symbol[left : Except[_QuantumMeasurementOperator] ...,
    qmo_QuantumMeasurementOperator, right : Except[_QuantumMeasurementOperator] ...] /; MemberQ[Attributes[f], NumericFunction] :=
    QuantumMeasurementOperator[f[left, qmo["Operator"], right], qmo["Target"]]



(* equality *)

QuantumMeasurementOperator /: Equal[qmo : _QuantumMeasurementOperator ... ] :=
    Equal @@ (#["Picture"] & /@ {qmo}) && Equal @@ (#["OrderedMatrixRepresentation"] & /@ {qmo})

