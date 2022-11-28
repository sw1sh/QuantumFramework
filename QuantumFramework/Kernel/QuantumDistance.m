Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumDistance"]
PackageScope["$QuantumDistances"]



$QuantumDistances = {"Fidelity", "RelativeEntropy", "Trace", "BuresAngle", "HilbertSchmidt", "Bloch"}


QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ] := QuantumDistance[qs1, qs2, "Fidelity"]

QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ, "Fidelity"] := With[{
    rootDensityMatrix = MatrixPower[qs1["Computational"]["DensityMatrix"], 1 / 2]
},
    Re[Tr[MatrixPower[rootDensityMatrix . qs2["Computational"]["DensityMatrix"] . rootDensityMatrix, 1 / 2]]]
]

QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ, "RelativeEntropy"] := With[{
    s = qs1["Computational"]["DensityMatrix"],
    t = qs2["Computational"]["DensityMatrix"]
},
    Quantity[Chop[Tr[s . (MatrixLog[s, Method -> "Jordan"] - MatrixLog[t, Method -> "Jordan"])]] / Log[2], "Bits"]
]


QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ, "Trace"] := With[{
    m = qs1["Computational"]["DensityMatrix"] - qs2["Computational"]["DensityMatrix"]
},
    Re @ Tr[MatrixPower[ConjugateTranspose[m] . m, 1 / 2]] / 2
]

QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ, "BuresAngle"] :=
    Re @ ArcCos @ QuantumDistance[qs1, qs2, "Fidelity"]

QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ, "HilbertSchmidt"] :=
    Sqrt @ Re @ Tr @ MatrixPower[qs1["Computational"]["DensityMatrix"] - qs2["Computational"]["DensityMatrix"], 2]

QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ, "Bloch"] :=
    Re @ EuclideanDistance[qs1["BlochCartesianCoordinates"], qs2["BlochCartesianCoordinates"]]

