Package["QuantumFramework`"]

PackageExport["QuantumEntangledQ"]
PackageExport["QuantumEntanglementMonotone"]



bipartitionComplement[biPartition_List, qudits_Integer] := Complement[Range[qudits], Join @@ biPartition]

bipartitionTrace[qs_ ? QuantumStateQ, biPartition_List] := QuantumPartialTrace[qs, bipartitionComplement[biPartition, qs["Qudits"]]]

bipartitionReduce[qs_ ? QuantumStateQ, biPartition_List] :=
    QuantumPartialTrace[qs, Join[First[biPartition], bipartitionComplement[biPartition, qs["Qudits"]]]]



QuantumEntangledQ[qs_ ? QuantumStateQ, biPartition_List : {{1}, {2}}] :=
    Enclose[ConfirmMatch[QuantumEntanglementMonotone[qs, biPartition, "Concurrence"], _ ? NumericQ] > 0, Indeterminate &]



QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_List : {{1}, {2}}, name_ : "Concurrence"] :=
    QuantumEntanglementMonotone[qs, biPartition, name]


QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_List, "Concurrence"] := Module[{
    relevantState, twoQubitPauliY, concurrence
},

    relevantState = bipartitionTrace[qs, biPartition];

    If[ relevantState["PureStateQ"],

        Chop[Sqrt[2 (1 - Tr[MatrixPower[bipartitionReduce[qs, biPartition]["NormalizedDensityMatrix"], 2]])]],

        If[ relevantState["Dimensions"] == {2, 2},

            twoQubitPauliY = {{0, 0, 0, -1}, {0, 0, 1, 0}, {0, 1, 0, 0}, {-1, 0, 0, 0}};
            concurrence = ReverseSort[Eigenvalues[twoQubitPauliY . Conjugate[relevantState["NormalizedDensityMatrix"]] . ConjugateTranspose[twoQubitPauliY]]];
            concurrence = concurrence[[1]] - concurrence[[2]] - concurrence[[3]] - concurrence[[4]];
            Max[0, Re[concurrence]],

            Indeterminate
        ]
    ]
]


QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_List, "Negativity"] :=
    (2 ^ QuantumEntanglementMonotone[qs, biPartition, "LogNegativity"] - 1) / 2


QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_List, "LogNegativity"] :=
    Chop @ Log[2, Total @ SingularValueList @
        bipartitionTrace[qs, biPartition][{"Transpose", Take[Ordering[biPartition], 1]}]["NormalizedDensityMatrix"]
    ]


QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_List, "EntanglementEntropy"] :=
    If[ bipartitionTrace[qs, biPartition]["PureStateQ"],
        bipartitionReduce[qs, biPartition]["VonNeumannEntropy"],
        Indeterminate
    ]


QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_List, "RenyiEntanglementEntropy"] :=
    QuantumEntanglementMonotone[qs, biPartition, {"RenyiEntanglementEntropy", 1 / 2}]

QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_List, {"RenyiEntanglementEntropy", alpha_}] :=
    If[ bipartitionTrace[qs, biPartition]["PureStateQ"],
        (1 / (1 - alpha)) Log[2, Total[Select[Re[Eigenvalues[MatrixPower[bipartitionReduce[qs, biPartition]["NormalizedDensityMatrix"], alpha]]], # > 0 &]]],
        Indeterminate
    ]

