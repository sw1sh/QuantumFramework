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

QuantumTensorProduct[qds1_QuantumState, qds2_QuantumState] := Enclose[
    ConfirmAssert[ConfirmBy[qds1, QuantumStateQ]["Picture"] === ConfirmBy[qds2, QuantumStateQ]["Picture"]];
    QuantumState[
        If[qds1["StateType"] === qds2["StateType"] === "Vector",
            Flatten[KroneckerProduct[
                ArrayReshape[qds1["StateVector"], qds1["MatrixNameDimensions"]],
                ArrayReshape[qds2["StateVector"], qds2["MatrixNameDimensions"]]
            ]],
            KroneckerProduct[qds1["DensityMatrix"], qds2["DensityMatrix"]]
        ],
        QuantumTensorProduct[qds1["Basis"], qds2["Basis"]]
    ]
]


(* operator x operator *)

QuantumTensorProduct[qdo1_QuantumOperator, qdo2_QuantumOperator] :=
    QuantumOperator[QuantumTensorProduct[qdo1["State"], qdo2["State"]], Join[qdo1["Order"], qdo2["Order"] + Max[qdo1["TotalOrder"]]]]


QuantumTensorProduct[qmo1_QuantumMeasurementOperator, qmo2_QuantumMeasurementOperator] :=
    QuantumMeasurementOperator[QuantumTensorProduct[qmo1["Operator"], qmo2["Operator"]]]


QuantumTensorProduct[_, _] := Failure[QuantumTensorProduct, "Undefined"]
