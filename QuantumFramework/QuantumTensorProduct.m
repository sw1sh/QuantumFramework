Package["QuantumFramework`"]

PackageExport["QuantumTensorProduct"]



QuantumTensorProduct[args_List] := Fold[QuantumTensorProduct, args]

QuantumTensorProduct[args : Except[_List]] := QuantumTensorProduct[{args}]


(* basis x basis *)

QuantumTensorProduct[qb1_ ? ValidQuantumBasisQ, qb2_ ? ValidQuantumBasisQ] /; qb1["Picture"] === qb2["Picture"] :=
QuantumBasis[
    AssociationThread[
        Catenate @ Outer[Apply[CircleTimes] @* Join, qb1["NormalBasisElementNames"], qb2["NormalBasisElementNames"], 1],
        KroneckerProduct @@ {qb1["BasisElements"], qb2["BasisElements"]}
    ],
    qb1["Picture"]
]

