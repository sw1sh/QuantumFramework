Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumDistance"]
PackageScope["$QuantumDistances"]



$QuantumDistances = {"Fidelity", "RelativeEntropy", "Trace", "BuresAngle", "HilbertSchmidt", "Bloch"}


QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ] := QuantumDistance[qs1, qs2, "Fidelity"]

QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ, "Fidelity"] := With[{
    rootDensityMatrix = MatrixPower[qs1["Computational"]["DensityMatrix"], 1 / 2]
},
    Re[Tr[MatrixPower[rootDensityMatrix . qs2["Computational"]["DensityMatrix"] . rootDensityMatrix, 1 / 2]] ^ 2]
]

QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ, "RelativeEntropy"] := Enclose @ Module[{
    e1 = qs1["Eigenvalues"] // Chop,
    e2 = qs2["Eigenvalues"] // Chop,
    positiveEigenvaluePositions
},
    positiveEigenvaluePositions = ConfirmBy[Position[e1, _ ? Positive], Length[#] > 0 &];
    e1 = Extract[e1, positiveEigenvaluePositions];
    e2 = ConfirmBy[Extract[e2, positiveEigenvaluePositions], AllTrue @ Positive];
    Quantity[Re[e1 . (Log[2, e1] - Log[2, e2])], "Bits"]
]


QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ, "Trace"] := With[{
    m = qs1["Computational"]["DensityMatrix"] - qs2["Computational"]["DensityMatrix"]
},
    Re @ Tr[MatrixPower[ConjugateTranspose[m] . m, 1 / 2]] / 2
]

QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ, "BuresAngle"] :=
    Re @ ArcCos @ Sqrt @ QuantumDistance[qs1, qs2, "Fidelity"]

QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ, "HilbertSchmidt"] :=
    Re @ Tr @ MatrixPower[qs1["Computational"]["DensityMatrix"] - qs2["Computational"]["DensityMatrix"], 2]

QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ, "Bloch"] :=
    Re @ EuclideanDistance[qs1["BlochCartesianCoordinates"], qs2["BlochCartesianCoordinates"]]

