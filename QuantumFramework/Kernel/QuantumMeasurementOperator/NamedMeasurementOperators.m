Package["Wolfram`QuantumFramework`"]

PackageScope["$QuantumMeasurementOperatorNames"]



$QuantumMeasurementOperatorNames = {
    "ComputationalBasis",
    "BellBasis",
    "PauliXBasis", "PauliYBasis", "PauliZBasis",
    "FourierBasis",
    "RandomHermitian"
}


QuantumMeasurementOperator["ComputationalBasis" | "Computational", args___] := QuantumMeasurementOperator[{"ComputationalBasis"}, args]

QuantumMeasurementOperator["ComputationalBasis" | "Computational" | {"ComputationalBasis" | "Computational"} -> eigenvalues_ ? VectorQ, args___] :=
    QuantumMeasurementOperator[{"ComputationalBasis"} -> eigenvalues, args]

QuantumMeasurementOperator[{"ComputationalBasis" | "Computational", dimension_Integer : 2}, args___, target : (_ ? orderQ) : {1}] :=
    QuantumMeasurementOperator[{"ComputationalBasis", dimension} -> Range[0, (dimension ^ Length[target]) - 1], args, target]

QuantumMeasurementOperator[{"ComputationalBasis" | "Computational",
    dimension_Integer} -> eigenvalues_ ? VectorQ, args___, target : (_ ? orderQ) : {1}] :=
With[{
    qudits = Ceiling @ Log[dimension, Length @ eigenvalues]
},
QuantumMeasurementOperator[
    QuantumOperator[
        PadRight[eigenvalues, dimension ^ qudits] . QuantumBasis[dimension, qudits]["Projectors"],
        dimension, qudits,
        Join[target, Complement[Min[target] + Range[qudits] - 1, target]]
    ],
    args,
    "Label" -> "Computational",
    target
]
]

QuantumMeasurementOperator[{"ComputationalBasis" | "Computational",
    dimensions : {_Integer ..}}, args___] := QuantumMeasurementOperator[{"ComputationalBasis", dimensions} -> Range[Times @@ dimensions] - 1, args]

QuantumMeasurementOperator[{"ComputationalBasis" | "Computational",
    dimensions : {_Integer ..}} -> eigenvalues_ ? VectorQ, args___, target : (_ ? orderQ) : {1}] :=
QuantumMeasurementOperator[
    QuantumOperator[
        PadRight[eigenvalues, Times @@ dimensions] . QuantumBasis[dimensions]["Projectors"],
        QuantumBasis[dimensions],
        Join[target, Complement[Min[target] + Range[Length[dimensions]] - 1, target]]
    ],
    args,
    "Label" -> "Computational",
    target
]


QuantumMeasurementOperator["BellBasis", target : (_ ? orderQ) : {1}] :=
    QuantumMeasurementOperator["BellBasis" -> Range[0, (4 ^ Length[target]) - 1], target]

QuantumMeasurementOperator["BellBasis" -> eigenvalues_ ? VectorQ, args___, target : (_ ? orderQ) : {1}] :=
    QuantumMeasurementOperator[
        QuantumOperator[eigenvalues . QuantumBasis["Bell", Length[target]]["Projectors"], 4, Length[target], target],
        args, "Label" -> "Bell",
        target
    ]


QuantumMeasurementOperator[name : "PauliXBasis" | "PauliYBasis" | "PauliZBasis", args___, target : (_ ? orderQ) : {1}] :=
    QuantumMeasurementOperator[name -> Range[0, (2 ^ Length[target]) - 1], args, target]

QuantumMeasurementOperator[name : "PauliXBasis" | "PauliYBasis" | "PauliZBasis" -> eigenvalues_ ? VectorQ, args___, target : (_ ? orderQ) : {1}] :=
    QuantumMeasurementOperator[
        QuantumOperator[eigenvalues . QuantumBasis[StringDelete[name, "Basis"], Length[target]]["Projectors"], 2, Length[target], target], args,
        "Label" -> StringDelete[name, "Pauli" | "Basis"], target
    ]


 QuantumMeasurementOperator["FourierBasis", args___] := QuantumMeasurementOperator[{"FourierBasis"}, args]

QuantumMeasurementOperator[{"FourierBasis", dimension_Integer : 2}, args___, target : (_ ? orderQ) : {1}] :=
    QuantumMeasurementOperator[{"FourierBasis", dimension} -> Range[0, (dimension ^ Length[target]) - 1], args, target]

QuantumMeasurementOperator[{"FourierBasis", dimension_Integer : 2} -> eigenvalues_ ? VectorQ, args___, target : (_ ? orderQ) : {1}] :=
    QuantumMeasurementOperator[
        QuantumOperator[eigenvalues . QuantumBasis[{"Fourier", dimension}, Length[target]]["Projectors"], dimension, target], args, "Label" -> "F", target]


QuantumMeasurementOperator["RandomHermitian", args___] := QuantumMeasurementOperator[{"RandomHermitian", 2}, args]

QuantumMeasurementOperator[{"RandomHermitian", dimension_Integer : 2}, args___, target : (_ ? orderQ) : {1}] :=
    QuantumMeasurementOperator[
        QuantumOperator[Orthogonalize[
            Table[
                RandomReal[NormalDistribution[0, 1 / 2]] + I RandomReal[NormalDistribution[0, 1 / 2]],
                dimension ^ Length[target], dimension ^ Length[target]
            ]
        ],
            dimension,
            target
        ],
        args,
        target
    ]

QuantumMeasurementOperator[matrix_ ? MatrixQ -> eigenvalues_ ? VectorQ, args___, target : (_ ? orderQ) : {1}] :=
    QuantumMeasurementOperator[QuantumOperator[eigenvalues . (projector /@ matrix)], args, target]

QuantumMeasurementOperator[qb_ ? QuantumBasisQ -> eigenvalues_ ? VectorQ, args___, target : (_ ? orderQ) : {1}] :=
    QuantumMeasurementOperator[
        QuantumOperator[eigenvalues . qb["Projectors"],
        qb,
        Range[If[qb["HasInputQ"], qb["InputQudits"], qb["OutputQudits"]]]],
        args,
        target
    ]

QuantumMeasurementOperator[qb_ ? QuantumBasisQ, args___, target : (_ ? orderQ) : {1}] :=
    QuantumMeasurementOperator[qb -> Range[qb["Dimension"]] - 1, args, target]

