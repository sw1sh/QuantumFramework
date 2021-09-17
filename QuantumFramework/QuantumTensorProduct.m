Package["QuantumFramework`"]

PackageExport["QuantumTensorProduct"]

PackageScope["multiplyElements"]



multiplyElements[elements_Association, multiplicity_Integer] := AssociationThread[
    QuantumTensorProduct @@@ Tuples[Keys[elements], multiplicity],
    kroneckerProduct @@@ Tuples[Values[elements], multiplicity]
]


QuantumTensorProduct[args_List] := Fold[QuantumTensorProduct, args]

QuantumTensorProduct[args___] := QuantumTensorProduct[{args}]



(* basis x basis *)

QuantumTensorProduct[qb1_ ? QuantumBasisQ, qb2_ ? QuantumBasisQ] /; qb1["Picture"] === qb2["Picture"] :=
QuantumBasis[<|
    "Input" -> AssociationThread[
        QuantumTensorProduct @@@ Tuples[{qb1["InputBasisElementNames"], qb2["InputBasisElementNames"]}],
        kroneckerProduct @@@ Tuples[{qb1["InputBasisElements"], qb2["InputBasisElements"]}]
    ],
    "Output" -> AssociationThread[
        QuantumTensorProduct @@@ Tuples[{qb1["OutputBasisElementNames"], qb2["OutputBasisElementNames"]}],
        kroneckerProduct @@@ Tuples[{qb1["OutputBasisElements"], qb2["OutputBasisElements"]}]
    ],
    "Picture" -> qb1["Picture"]
|>]


(* state x state *)

QuantumTensorProduct[qds1_ ? QuantumStateQ, qds2_ ? QuantumStateQ] /; qds1["Picture"] === qds2["Picture"] :=
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


(* operator x operator *)

QuantumTensorProduct[qdo1_ ? QuantumOperatorQ, qdo2_ ? QuantumOperatorQ] :=
    QuantumOperator[QuantumTensorProduct[qdo1["State"], qdo2["State"]], Join[qdo1["Order"], qdo2["Order"] + Max[qdo1["TotalOrder"]]]]


QuantumTensorProduct[qmo1_ ? QuantumMeasurementOperatorQ, qmo2_ ? QuantumMeasurementOperatorQ] :=
    QuantumMeasurementOperator[QuantumTensorProduct[qmo1["Operator"], qmo2["Operator"]]]



(* name x name *)

QuantumTensorProduct[name1_, name2_] := If[name1 === $BasisNameZero || name2 === $BasisNameZero,
    $BasisNameZero,
    Join[normalBasisElementName[name1], normalBasisElementName[name2]](* /. {} -> $BasisNameIdentity*)
]

