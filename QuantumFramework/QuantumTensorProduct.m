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


(* name x name *)


QuantumTensorProduct[name1_, name2_] := If[name1 === $BasisNameZero || name2 === $BasisNameZero,
    $BasisNameZero,
    Join[normalBasisElementName[name1], normalBasisElementName[name2]](* /. {} -> $BasisNameIdentity*)
]

