Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumTensorProduct"]



QuantumTensorProduct[args_List] := Fold[QuantumTensorProduct, args]

QuantumTensorProduct[args___] := QuantumTensorProduct[{args}]

resetIndex[qb_QuditBasis] := With[{index = Sort @ qb["Index"]},
    KeyMap[MapAt[Replace[Thread[index -> Range[Length[index]]]], 2], KeySortBy[{Last}] @ qb["Representations"]]
]

QuantumTensorProduct[qb1_QuditBasis, qb2_QuditBasis] := If[qb1["Length"] == 0 || qb2["Length"] == 0, QuditBasis[0],
    QuditBasis[
        Association[resetIndex[qb1], KeyMap[MapAt[# + qb1["FullQudits"] &, 2]] @ resetIndex[qb2]]
    ]
]


(* basis x basis *)

QuantumTensorProduct[qb1_QuantumBasis, qb2_QuantumBasis] /; qb1["Picture"] === qb2["Picture"] :=
QuantumBasis[<|
    "Output" -> QuantumTensorProduct[qb1["Output"], qb2["Output"]],
    "Input" -> QuantumTensorProduct[qb1["Input"], qb2["Input"]],
    "Picture" -> qb1["Picture"],
    "Label" ->
        Replace[
            {qb1["Label"], qb2["Label"]},
            {
                {OrderlessPatternSequence[Superscript[a_, CircleTimes[q_Integer]], a_]} :> Superscript[a, CircleTimes[q + 1]],
                {Superscript[a_, CircleTimes[q_Integer]], Superscript[a_, CircleTimes[r_Integer]]} :> Superscript[a, CircleTimes[q + r]],
                {a_, a_} :> Superscript[a, CircleTimes[2]],
                {a_, b_} :> Flatten @ CircleTimes[a, b] /. None -> Sequence[]
            }
        ],
    "ParameterSpec" -> Join[qb1["ParameterSpec"], qb2["ParameterSpec"]]
|>
]


(* state x state *)

QuantumTensorProduct[qs1_QuantumState, qs2_QuantumState] /; qs1["Input"] == qs2["Input"] && qs1["Output"] == qs2["Output"] := Enclose[
    ConfirmAssert[ConfirmBy[qs1, QuantumStateQ]["Picture"] === ConfirmBy[qs2, QuantumStateQ]["Picture"]];
    profile["Kronecker"] @ QuantumState[
        If[ qs1["StateType"] === qs2["StateType"] === "Vector",
            SparseArrayFlatten @ KroneckerProduct[
                qs1["StateMatrix"],
                qs2["StateMatrix"]
            ],
            KroneckerProduct[qs1["DensityMatrix"], qs2["DensityMatrix"]]
        ],
        QuantumTensorProduct[qs1["Basis"], qs2["Basis"]]
    ]
]

QuantumTensorProduct[qs1_QuantumState, qs2_QuantumState] := Enclose[
    ConfirmAssert[ConfirmBy[qs1, QuantumStateQ]["Picture"] === ConfirmBy[qs2, QuantumStateQ]["Picture"]];
    QuantumState[QuantumState[
        If[ qs1["StateType"] === qs2["StateType"] === "Vector",
            SparseArrayFlatten @ KroneckerProduct[
                qs1["Computational"]["StateMatrix"],
                qs2["Computational"]["StateMatrix"]
            ],
            KroneckerProduct[qs1["MatrixRepresentation"], qs2["MatrixRepresentation"]]
        ],
        QuantumBasis[Join[qs1["OutputDimensions"], qs2["OutputDimensions"]], Join[qs1["InputDimensions"], qs2["InputDimensions"]]]
    ],
        QuantumTensorProduct[qs1["Basis"], qs2["Basis"]]
    ]
]


(* operator x operator *)

QuantumTensorProduct[qo1_QuantumOperator, qo2_QuantumOperator] :=
    QuantumOperator[
        QuantumTensorProduct[qo1["State"], qo2["State"]],
        {
            If[ IntersectingQ[qo1["OutputOrder"], qo2["OutputOrder"]] && qo1["OutputQudits"] > 0 && qo2["OutputQudits"] > 0,
                DeleteDuplicates[Join[qo1["OutputOrder"], qo1["LastOutputQudit"] - qo2["FirstOutputQudit"] + qo2["OutputOrder"] + 1]],
                DeleteDuplicates[Join[qo1["OutputOrder"], qo2["OutputOrder"]]]
            ],
            If[ IntersectingQ[qo1["InputOrder"], qo2["InputOrder"]] && qo1["InputQudits"] > 0 && qo2["InputQudits"] > 0,
                DeleteDuplicates[Join[qo1["InputOrder"], qo1["LastInputQudit"] - qo2["FirstInputQudit"] + qo2["InputOrder"] + 1]],
                DeleteDuplicates[Join[qo1["InputOrder"], qo2["InputOrder"]]]
            ]
        }
    ]


QuantumTensorProduct[qmo1_QuantumMeasurementOperator, qmo2_QuantumMeasurementOperator] :=
    QuantumMeasurementOperator[QuantumTensorProduct[qmo1["QuantumOperator"], qmo2["QuantumOperator"]], Union[qmo1["Target"], qmo2["Target"]]]


QuantumTensorProduct[qm1_QuantumMeasurement, qm2_QuantumMeasurement] := QuantumMeasurement @ QuantumTensorProduct[qm1["State"], qm2["State"]]


QuantumTensorProduct[qo_QuantumOperator, qmo_QuantumMeasurementOperator] := QuantumTensorProduct[QuantumMeasurementOperator[qo], qmo]

QuantumTensorProduct[qmo_QuantumMeasurementOperator, qo_QuantumOperator] := QuantumTensorProduct[qmo, QuantumMeasurementOperator[qo]]


(* circuit x circuit *)

QuantumTensorProduct[qc1_QuantumCircuitOperator, qc2_QuantumCircuitOperator] :=
    QuantumCircuitOperator[Join[qc1["Operators"], qc2["Shift", qc1["Max"]]["Operators"]], CircleTimes[qc1["Label"], qc2["Label"]]]


QuantumTensorProduct[_, _] := Failure[QuantumTensorProduct, "Undefined"]
