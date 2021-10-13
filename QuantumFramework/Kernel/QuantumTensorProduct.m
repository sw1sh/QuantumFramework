Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumTensorProduct"]



QuantumTensorProduct[args_List] := Fold[QuantumTensorProduct, args]

QuantumTensorProduct[args___] := QuantumTensorProduct[{args}]


QuantumTensorProduct[qb1_QuditBasis, qb2_QuditBasis] := Enclose @ QuditBasis[
    QuantumTensorProduct @@@ Tuples[{ConfirmBy[qb1, QuditBasisQ]["Names"], ConfirmBy[qb2, QuditBasisQ]["Names"]}],
    Association[qb1["Representations"], KeyMap[MapAt[# + qb1["NameRank"] &, 2]] @ qb2["Representations"]]
]["RemoveIdentities"]


(* basis x basis *)

QuantumTensorProduct[qb1_QuantumBasis, qb2_QuantumBasis] /; qb1["Picture"] === qb2["Picture"] :=
QuantumBasis[
    "Output" -> QuditBasis[
        QuantumTensorProduct @@@ Tuples[{qb1["OutputElementNames"], qb2["OutputElementNames"]}],
        Association[qb1["Output"]["Representations"], KeyMap[MapAt[# + qb1["Output"]["NameRank"] &, 2]] @ qb2["Output"]["Representations"]]
    ],
    "Input" -> QuditBasis[
        QuantumTensorProduct @@@ Tuples[{qb1["InputElementNames"], qb2["InputElementNames"]}],
        Association[qb1["Input"]["Representations"], KeyMap[MapAt[# + qb1["Input"]["NameRank"] &, 2]] @ qb2["Input"]["Representations"]]
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

QuantumTensorProduct[qo1_QuantumOperator, qo2_QuantumOperator] :=
    QuantumOperator[
        QuantumTensorProduct[qo1["State"], qo2["State"]],
        If[ IntersectingQ[qo1["OutputOrder"], qo2["OutputOrder"]],
            Join[qo1["OutputOrder"], qo1["LastOutputQudit"] - qo2["FirstOutputQudit"] + qo2["OutputOrder"] + 1],
            Join[qo1["OutputOrder"], qo2["OutputOrder"]]
        ],
        If[ IntersectingQ[qo1["InputOrder"], qo2["InputOrder"]],
            Join[qo1["InputOrder"], qo1["LastInputQudit"] - qo2["FirstInputQudit"] + qo2["InputOrder"] + 1],
            Join[qo1["InputOrder"], qo2["InputOrder"]]
        ]
    ]


QuantumTensorProduct[qmo1_QuantumMeasurementOperator, qmo2_QuantumMeasurementOperator] :=
    QuantumMeasurementOperator[QuantumTensorProduct[qmo1["QuantumOperator"], qmo2["QuantumOperator"]], Union[qmo1["Target"], qmo2["Target"]]]


QuantumTensorProduct[qm1_QuantumMeasurement, qm2_QuantumMeasurement] := QuantumMeasurement @ QuantumTensorProduct[qm1["State"], qm2["State"]]

QuantumTensorProduct[_, _] := Failure[QuantumTensorProduct, "Undefined"]
