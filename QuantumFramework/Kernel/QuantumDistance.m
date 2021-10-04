Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumDistance"]



QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ] := QuantumDistance[qs1, qs2, "Fidelity"]

QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ, "Fidelity"] := With[{
    rootDensityMatrix = MatrixPower[qs1["DensityMatrix"], 1 / 2]
},
    Tr[MatrixPower[rootDensityMatrix . qs2["DensityMatrix"] . rootDensityMatrix, 1 / 2]] ^ 2
]

QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ, "RelativeEntropy"] :=
    Tr[qs1["DensityMatrix"] Log[2, qs1["DensityMatrix"]]] - Tr[qs2["DensityMatrix"] Log[2, qs2["DensityMatrix"]]]


QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ, "Trace"] :=
    Re @ Total @ Abs @ Eigenvalues[qs1["DensityMatrix"] - qs2["DensityMatrix"]] / 2

QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ, "BuresAngle"] :=
    Re @ ArcCos @ Sqrt @ QuantumDistance[qs1, qs2, "Fidelity"]

QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ, "HilbertSchmidt"] :=
    Re @ Sqrt @ Tr @ MatrixPower[qs1["DensityMatrix"] - qs2["DensityMatrix"], 2]

QuantumDistance[qs1_ ? QuantumStateQ, qs2_ ? QuantumStateQ, "Bloch"] :=
    Re @ EuclideanDistance[qs1["BlochCartesianCoordinates"], qs2["BlochCartesianCoordinates"]]

