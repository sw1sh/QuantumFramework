(* ::Package:: *)

Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumEntangledQ"]
PackageExport["QuantumEntanglementMonotone"]


(*if no biPartition is given as argument, then set it as list of list of all qudits*)
normalBipartition[biPartition_, qudits_] := Replace[biPartition, Automatic -> List /@ Range[qudits]]

(*list of qudits other than biPartition*)
bipartitionComplement[biPartition_ : Automatic, qudits_Integer] :=
    Complement[Range[qudits], Join @@ normalBipartition[biPartition, qudits]]

(*the reduced density matrix for qudits specified by biPartition*)
bipartitionTrace[qs_ ? QuantumStateQ, biPartition_ : Automatic] := QuantumPartialTrace[qs, bipartitionComplement[biPartition, qs["Qudits"]]]

(*the reduced density matrix of 2nd qudit, specified by biPartition*)
bipartitionReduce[qs_ ? QuantumStateQ, biPartition_ : Automatic] := With[{p = normalBipartition[biPartition, qs["Qudits"]]},
    QuantumPartialTrace[qs, Join[First[p], bipartitionComplement[p, qs["Qudits"]]]]
]



QuantumEntangledQ[qs_ ? QuantumStateQ, biPartition_ : Automatic] :=
    Enclose[ConfirmMatch[QuantumEntanglementMonotone[qs, biPartition, "Concurrence"], _ ? NumericQ] > 0, Indeterminate &]



QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_ : Automatic, name_ : "Concurrence"] :=
    QuantumEntanglementMonotone[qs, biPartition, name]


QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_ : Automatic, "Concurrence"] := Module[{
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


QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_ : Automatic, "Negativity"] :=
    (2 ^ QuantumEntanglementMonotone[qs, biPartition, "LogNegativity"] - 1) / 2


QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_ : Automatic, "LogNegativity"] :=
    Chop @ Log[2, Total @ SingularValueList @
        bipartitionTrace[qs, biPartition][{"Transpose", Take[Ordering[biPartition], 1]}]["NormalizedDensityMatrix"]
    ]


QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_ : Automatic, "EntanglementEntropy"] :=
    If[ bipartitionTrace[qs, biPartition]["PureStateQ"],
        bipartitionReduce[qs, biPartition]["VonNeumannEntropy"],
        Indeterminate
    ]


QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_ : Automatic, "RenyiEntanglementEntropy"] :=
    QuantumEntanglementMonotone[qs, biPartition, {"RenyiEntanglementEntropy", 1 / 2}]

QuantumEntanglementMonotone[qs_ ? QuantumStateQ, biPartition_ : Automatic, {"RenyiEntanglementEntropy", alpha_}] :=
    If[ bipartitionTrace[qs, biPartition]["PureStateQ"],
        (1 / (1 - alpha)) Log[2, Total[Select[Re[Eigenvalues[MatrixPower[bipartitionReduce[qs, biPartition]["NormalizedDensityMatrix"], alpha]]], # > 0 &]]],
        Indeterminate
    ]

