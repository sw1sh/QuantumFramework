Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumDistance"]
PackageExport["QuantumSimilarity"]

PackageScope["$QuantumDistances"]



$QuantumDistances = {"Fidelity", "RelativeEntropy", "RelativePurity", "Trace", "Bures", "BuresAngle", "HilbertSchmidt", "Bloch"}


QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ] := QuantumDistance[qs1, qs2, "Fidelity"]

QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ, "Fidelity"] /; qs1["Dimension"] == qs2["Dimension"] := With[{
    rootDensityMatrix = MatrixPower[qs1["Computational"]["DensityMatrix"], 1 / 2]
},
    1 - Re[Tr[MatrixPower[rootDensityMatrix . qs2["Computational"]["DensityMatrix"] . rootDensityMatrix, 1 / 2]]]
]

QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ, "RelativeEntropy"] /; qs1["Dimension"] == qs2["Dimension"] := If[
    qs2["PureStateQ"],

    qs1["Entropy"],

    Block[{
        s = qs1["Computational"]["DensityMatrix"],
        t = qs2["Computational"]["DensityMatrix"],
        slog, tlog, entropy, crossTerm
    },
        slog = MatrixLog[s, Method -> "Jordan"];
        tlog = MatrixLog[t, Method -> "Jordan"];
        entropy = - Tr[s . slog];
        crossTerm = - Tr[s . tlog];
        Quantity[Chop[If[Abs[crossTerm] <= 2 Log[qs1["Dimension"]], crossTerm - entropy, If[Chop[entropy] == 0, - Tr[t . tlog], Tr[t . tlog] - Tr[t . slog]]] / Log[2]], "Bits"]
    ]
]

QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ, "RelativePurity"] /; qs1["Dimension"] == qs2["Dimension"] := With[{
    s = qs1["Computational"]["DensityMatrix"],
    t = qs2["Computational"]["DensityMatrix"]
},
    Chop[Tr[s . t]]
]


QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ, "Trace"] /; qs1["Dimension"] == qs2["Dimension"] := With[{
    m = qs1["Computational"]["DensityMatrix"] - qs2["Computational"]["DensityMatrix"]
},
    Re @ Tr[MatrixPower[ConjugateTranspose[m] . m, 1 / 2]] / 2
]

QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ, "Bures"] /; qs1["Dimension"] == qs2["Dimension"]  :=
    Sqrt[2 (1 - QuantumDistance[qs1, qs2, "Fidelity"])]

QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ, "BuresAngle"] /; qs1["Dimension"] == qs2["Dimension"]  :=
    Re @ ArcCos @ QuantumDistance[qs1, qs2, "Fidelity"]

QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ, "HilbertSchmidt"] /; qs1["Dimension"] == qs2["Dimension"] :=
    Sqrt @ Re @ Tr @ MatrixPower[qs1["Computational"]["DensityMatrix"] - qs2["Computational"]["DensityMatrix"], 2]

QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ, "Bloch"] /; qs1["Dimension"] == qs2["Dimension"] :=
    Re @ EuclideanDistance[qs1["BlochCartesianCoordinates"], qs2["BlochCartesianCoordinates"]] / 2


QuantumSimilarity[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ, distance_String : "Fidelity"] := 1 - QuantumDistance[qs1, qs2, distance]

