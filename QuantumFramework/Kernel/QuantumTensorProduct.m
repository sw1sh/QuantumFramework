Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumTensorProduct"]



QuantumTensorProduct[args_List] := Fold[QuantumTensorProduct, args]

QuantumTensorProduct[args___] := QuantumTensorProduct[{args}]


QuantumTensorProduct[qb1_QuditBasis, qb2_QuditBasis] := Enclose @ QuditBasis[
    QuantumTensorProduct @@@ Tuples[{ConfirmBy[qb1, QuditBasisQ]["Names"], ConfirmBy[qb2, QuditBasisQ]["Names"]}],
    Association[qb1["BasisElements"], KeyMap[MapAt[# + qb1["NameRank"] &, 2]] @ qb2["BasisElements"]]
]["RemoveIdentities"]


(* basis x basis *)

QuantumTensorProduct[qb1_QuantumBasis, qb2_QuantumBasis] /; qb1["Picture"] === qb2["Picture"] :=
QuantumBasis[
    "Output" -> QuditBasis[
        QuantumTensorProduct @@@ Tuples[{qb1["OutputBasisElementNames"], qb2["OutputBasisElementNames"]}],
        Association[qb1["Output"]["BasisElements"], KeyMap[MapAt[# + qb1["Output"]["NameRank"] &, 2]] @ qb2["Output"]["BasisElements"]]
    ],
    "Input" -> QuditBasis[
        QuantumTensorProduct @@@ Tuples[{qb1["InputBasisElementNames"], qb2["InputBasisElementNames"]}],
        Association[qb1["Input"]["BasisElements"], KeyMap[MapAt[# + qb1["Input"]["NameRank"] &, 2]] @ qb2["Input"]["BasisElements"]]
    ],
    "Picture" -> qb1["Picture"],
    "Label" -> Flatten @ CircleTimes[qb1["Label"], qb2["Label"]] /. None -> Sequence[]
]


(* state x state *)

QuantumTensorProduct[qs1_QuantumState, qs2_QuantumState] := Enclose[
    ConfirmAssert[ConfirmBy[qs1, QuantumStateQ]["Picture"] === ConfirmBy[qs2, QuantumStateQ]["Picture"]];
    QuantumState[QuantumState[
        If[qs1["StateType"] === qs2["StateType"] === "Vector",
            Flatten[KroneckerProduct[
                qs1["PureMatrix"],
                qs2["PureMatrix"]
            ]],
            KroneckerProduct[qs1["MatrixRepresentation"], qs2["MatrixRepresentation"]]
        ],
        QuantumBasis[Join[qs1["OutputDimensions"], qs2["OutputDimensions"]], Join[qs1["InputDimensions"], qs2["InputDimensions"]]]
    ],
        QuantumTensorProduct[qs1["Basis"], qs2["Basis"]]
    ]
]


(* operator x operator *)

QuantumTensorProduct[qdo1_QuantumOperator, qdo2_QuantumOperator] :=
    QuantumOperator[QuantumTensorProduct[qdo1["State"], qdo2["State"]],
        If[ IntersectingQ[qdo1["InputOrder"], qdo2["InputOrder"]],
            Join[qdo1["Order"], Max[qdo1["Order"]] + qdo2["InputQuditOrder"]],
            Join[qdo1["Order"], qdo2["Order"]]
        ]
    ]


QuantumTensorProduct[qmo1_QuantumMeasurementOperator, qmo2_QuantumMeasurementOperator] :=
    QuantumMeasurementOperator[QuantumTensorProduct[qmo1["QuantumOperator"], qmo2["QuantumOperator"]], Union[qmo1["Target"], qmo2["Target"]]]


QuantumTensorProduct[qm1_QuantumMeasurement, qm2_QuantumMeasurement] := QuantumMeasurement @ QuantumTensorProduct[qm1["State"], qm2["State"]]

QuantumTensorProduct[_, _] := Failure[QuantumTensorProduct, "Undefined"]
