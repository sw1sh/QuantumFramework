Package["QuantumFramework`"]

PackageExport["QuantumTensorProduct"]



QuantumTensorProduct[args_List] := Fold[QuantumTensorProduct, args]

QuantumTensorProduct[args___] := QuantumTensorProduct[{args}]



(* basis x basis *)

QuantumTensorProduct[qb1_QuantumBasis, qb2_QuantumBasis] /; qb1["Picture"] === qb2["Picture"] :=
QuantumBasis[<|
    "Input" -> AssociationThread[
        QuantumTensorProduct @@@ Tuples[{qb1["InputBasisElementNames"], qb2["InputBasisElementNames"]}],
        kroneckerProduct @@@ Tuples[{qb1["InputBasisElements"], qb2["InputBasisElements"]}]
    ],
    "Output" -> AssociationThread[
        QuantumTensorProduct @@@ Tuples[{qb1["OutputBasisElementNames"], qb2["OutputBasisElementNames"]}],
        kroneckerProduct @@@ Tuples[{qb1["OutputBasisElements"], qb2["OutputBasisElements"]}]
    ],
    "Picture" -> qb1["Picture"],
    "Label" -> Flatten @ CircleTimes[qb1["Label"], qb2["Label"]]
|>]


(* state x state *)

QuantumTensorProduct[qs1_QuantumState, qs2_QuantumState] := Enclose[
    ConfirmAssert[ConfirmBy[qs1, QuantumStateQ]["Picture"] === ConfirmBy[qs2, QuantumStateQ]["Picture"]];
    QuantumState[QuantumState[
        If[qs1["StateType"] === qs2["StateType"] === "Vector",
            Flatten[KroneckerProduct[
                qs1["Matrix"],
                qs2["Matrix"]
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
    QuantumOperator[QuantumTensorProduct[qdo1["State"], qdo2["State"]], Join[qdo1["Order"], qdo2["Order"] + Max[qdo1["TotalOrder"]]]]


QuantumTensorProduct[qmo1_QuantumMeasurementOperator, qmo2_QuantumMeasurementOperator] :=
    QuantumMeasurementOperator[QuantumTensorProduct[qmo1["QuantumOperator"], qmo2["QuantumOperator"]], Union[qmo1["Order"], qmo2["Order"]]]


QuantumTensorProduct[_, _] := Failure[QuantumTensorProduct, "Undefined"]
