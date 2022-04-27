Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumEntangledQ"]
PackageExport["QuantumEntanglementMonotone"]



QuantumEntangledQ[qs_ ? QuantumStateQ, biPartition_ : Automatic] :=
    Enclose[ConfirmMatch[QuantumEntanglementMonotone[qs, biPartition, "Concurrence"], _ ? NumericQ] > 0, Indeterminate &]


QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition : Except[_String] : Automatic] :=
    QuantumEntanglementMonotone[qs, biPartition, "Concurrence"]


QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_ : Automatic, "Concurrence"] := Module[{
    s, ds, twoQuditPauliY
},

    s = qs["Bipartition", biPartition];
    ds = s["Dimensions"];
    twoQuditPauliY = QuantumTensorProduct[QuantumOperator[{"Y", ds[[1]]}], QuantumOperator[{"Y", ds[[2]]}]];

    Max[0, Fold[Subtract, SingularValueList[Sqrt[s]["DensityMatrix"] . Sqrt[twoQuditPauliY[s["Conjugate"]]]["DensityMatrix"]]]]
]


QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_ : Automatic, "Negativity"] :=
    (2 ^ QuantumEntanglementMonotone[qs["Bipartition", biPartition], "LogNegativity"] - 1) / 2


QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_ : Automatic, "LogNegativity"] :=
    Chop @ Log[2, Total @ SingularValueList @ QuantumPartialTrace[qs["Bipartition", biPartition], {1}]["NormalizedDensityMatrix"]]


QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_ : Automatic, "EntanglementEntropy"] :=
    QuantumPartialTrace[qs["Bipartition", biPartition], {1}]["VonNeumannEntropy"]


QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_ : Automatic, "RenyiEntanglementEntropy"] :=
    QuantumEntanglementMonotone[qs, biPartition, {"RenyiEntanglementEntropy", 1 / 2}]

QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_ : Automatic, {"RenyiEntanglementEntropy", alpha_}] :=
    (1 / (1 - alpha)) Log[2, Total[Select[Re[Eigenvalues[MatrixPower[QuantumPartialTrace[qs["Bipartition", biPartition], {1}]["NormalizedDensityMatrix"], alpha]]], # > 0 &]]]

