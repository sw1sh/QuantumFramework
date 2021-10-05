Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumDistance"]



QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ] := QuantumDistance[qs1, qs2, "Fidelity"]

QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ, "Fidelity"] := With[{
    rootDensityMatrix = MatrixPower[qs1["Computational"]["DensityMatrix"], 1 / 2]
},
    Re[Tr[MatrixPower[rootDensityMatrix . qs2["Computational"]["DensityMatrix"] . rootDensityMatrix, 1 / 2]] ^ 2]
]

QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ, "RelativeEntropy"] := With[{
    m1 = qs1["Computational"]["DensityMatrix"], m2 = qs2["Computational"]["DensityMatrix"]
},
    Re[Tr[m1 . MatrixLog[m1]] - Tr[m2 . MatrixLog[m2]]]
]


QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ, "Trace"] := With[{
    m = qs1["Computational"]["DensityMatrix"] - qs2["Computational"]["DensityMatrix"]
},
    Re @ Tr[MatrixPower[ConjugateTranspose[m] . m, 1 / 2]] / 2
]

QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ, "BuresAngle"] :=
    Re @ ArcCos @ Sqrt @ QuantumDistance[qs1, qs2, "Fidelity"]

QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ, "HilbertSchmidt"] :=
    Re @ Sqrt @ Tr @ MatrixPower[qs1["Computational"]["DensityMatrix"] - qs2["Computational"]["DensityMatrix"], 2]

QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ, "Bloch"] :=
    Re @ EuclideanDistance[qs1["BlochCartesianCoordinates"], qs2["BlochCartesianCoordinates"]]

