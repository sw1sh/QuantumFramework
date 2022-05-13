Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumEntangledQ"]
PackageExport["QuantumEntanglementMonotone"]



QuantumEntangledQ[qs_ ? QuantumStateQ, biPartition_ : Automatic] :=
    Enclose[ConfirmMatch[QuantumEntanglementMonotone[qs, biPartition, "Concurrence"], _ ? NumericQ] > 0, Indeterminate &]


QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition : Except[_String] : Automatic] :=
    QuantumEntanglementMonotone[qs, biPartition, "Concurrence"]


QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_ : Automatic, "Concurrence"] := Enclose @ Module[{
    s, ds, twoQuditPauliY
},

    s = ConfirmBy[qs["Bipartition", biPartition]["Normalized"], QuantumStateQ];
    ds = s["Dimensions"];
    twoQuditPauliY = QuantumTensorProduct[QuantumOperator[{"Y", ds[[1]]}], QuantumOperator[{"Y", ds[[2]]}]];

    Max[0, Fold[Subtract, SingularValueList[Sqrt[s]["DensityMatrix"] . Sqrt[twoQuditPauliY[s["Conjugate"]]]["DensityMatrix"]]]]
]


QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_ : Automatic, "Negativity"] :=
    Enclose[(ConfirmBy[qs["Bipartition", biPartition]["Normalized"], QuantumStateQ]["Transpose", {2}]["TraceNorm"] - 1) / 2]


QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_ : Automatic, "LogNegativity"] :=
    Enclose @ Log2 @ ConfirmBy[qs["Bipartition", biPartition]["Normalized"], QuantumStateQ]["Transpose", {2}]["TraceNorm"]


QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_ : Automatic, "EntanglementEntropy"] :=
    Enclose @ QuantumPartialTrace[ConfirmBy[qs["Bipartition", biPartition]["Normalized"], QuantumStateQ], {1}]["VonNeumannEntropy"]


QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_ : Automatic, "RenyiEntanglementEntropy" | "RenyiEntropy"] :=
    QuantumEntanglementMonotone[qs, biPartition, {"RenyiEntanglementEntropy", 1 / 2}]

QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_ : Automatic, {"RenyiEntanglementEntropy" | "RenyiEntropy", alpha_}] :=
    Enclose[
        (1 / (1 - alpha)) Log[2, Total[Select[Re[Eigenvalues[MatrixPower[
            QuantumPartialTrace[
                ConfirmBy[qs["Bipartition", biPartition], QuantumStateQ],
                {1}
            ]["NormalizedDensityMatrix"], alpha]]], # > 0 &]]]
    ]

