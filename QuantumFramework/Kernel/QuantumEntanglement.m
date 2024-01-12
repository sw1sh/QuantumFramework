Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumEntangledQ"]
PackageExport["QuantumEntanglementMonotone"]
PackageScope["$QuantumEntanglementMonotones"]



$QuantumEntanglementMonotones = {"Concurrence", "Negativity", "LogNegativity", "EntanglementEntropy", "RenyiEntropy", "Realignment"}

QuantumEntangledQ[qs_ ? QuantumStateQ, biPartition_ : Automatic, method_String : "Realignment"] /; MemberQ[$QuantumEntanglementMonotones, method] :=
    Enclose[Chop[ConfirmMatch[QuantumEntanglementMonotone[qs, biPartition, method], _ ? NumericQ]] > 0, Indeterminate &]



QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition : Except[_String] : Automatic] :=
    QuantumEntanglementMonotone[qs, biPartition, "Concurrence"]


y[{j_Integer, k_Integer}, n_Integer] /; 1 <= j < k <= n := SparseArray[{{j, k} -> -I, {k, j} -> I}, {n, n}]

Y[n_] := Y[n] = Catenate @ Table[y[{j, k}, n], {k, 2, n}, {j, k - 1}]

ConcurrenceVector[qs_ ? QuantumStateQ, biPartition_ : Automatic] := Block[{
	rho = qs["Bipartition", biPartition]["Operator"], d1, d2, y1, y2
},
	{d1, d2} = rho["OutputDimensions"];
	y1 = Y[d1];
	y2 = Y[d2];
	Catenate @ Table[
		With[{o = QuantumTensorProduct[QuantumOperator[y1[[n]], d1], QuantumOperator[y2[[m]], d2]]},
			Max[0, Fold[Subtract] @ SingularValueList[(Sqrt[rho] @ Sqrt[o @ rho["Conjugate"] @ o])["Matrix"]]]
		],
		{n, (d1 - 1) d1 / 2}, {m, (d2 - 1) d2 / 2}
	]
]

Concurrence[qs_ ? QuantumStateQ, biPartition_ : Automatic] := Norm @ ConcurrenceVector[qs, biPartition]

QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_ : Automatic, "ConcurrenceVector"] := ConcurrenceVector[qs, biPartition]

QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_ : Automatic, "Concurrence"] :=
    If[ qs["VectorQ"],
        Re @ Sqrt[2 (1 - (QuantumPartialTrace[qs["Bipartition", biPartition], {1}] ^ 2)["Norm"])],
        Concurrence[qs, biPartition]
    ]


QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_ : Automatic, "Negativity"] :=
    Enclose[(ConfirmBy[qs["Bipartition", biPartition]["Normalized"], QuantumStateQ[#] && #["Qudits"] == 2 &]["Transpose", {2}]["TraceNorm"] - 1) / 2]


QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_ : Automatic, "LogNegativity"] :=
    Enclose @ Log2 @ ConfirmBy[qs["Bipartition", biPartition]["Normalized"], QuantumStateQ[#] && #["Qudits"] == 2 &]["Transpose", {2}]["TraceNorm"]


QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_ : Automatic, "EntanglementEntropy"] := Enclose @ With[{
    bp = ConfirmBy[qs["Bipartition", biPartition]["Normalized"], QuantumStateQ[#] && #["Qudits"] == 2 &]
},
    If[ qs["VectorQ"],
        Quantity[Total[-# Log2[#] & @ Select[Confirm @ bp["Probability"], # > 0 &]], "Bits"],
        QuantumPartialTrace[bp, {1}]["VonNeumannEntropy"]
    ]
]

QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_ : Automatic, "RenyiEntanglementEntropy" | "RenyiEntropy"] :=
    QuantumEntanglementMonotone[qs, biPartition, {"RenyiEntanglementEntropy", 1 / 2}]

QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_ : Automatic, {"RenyiEntanglementEntropy" | "RenyiEntropy", alpha_}] :=
    Enclose[
        Re[(1 / (1 - alpha)) Log[2, Tr @ MatrixPower[
            QuantumPartialTrace[
                ConfirmBy[qs["Bipartition", biPartition]["Normalized"], QuantumStateQ[#] && #["Qudits"] == 2 &],
                {1}
            ]["DensityMatrix"], alpha]]
        ]
    ]

QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_ : Automatic, "Realignment"] :=
    With[{bqs = qs["Bipartition", biPartition]["Normalized"]},
        Total @ SingularValueList @ ArrayReshape[Transpose[bqs["Bend"]["Tensor"], 2 <-> 3], bqs["Dimensions"] ^ 2] - 1
    ]

