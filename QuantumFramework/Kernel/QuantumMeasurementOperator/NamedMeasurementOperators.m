Package["Wolfram`QuantumFramework`"]

PackageScope["$QuantumMeasurementOperatorNames"]



$QuantumMeasurementOperatorNames = {
    "ComputationalBasis",
    "BellBasis",
    "PauliXBasis", "PauliYBasis", "PauliZBasis",
    "FourierBasis",
    "RandomHermitian"
}


QuantumMeasurementOperator["ComputationalBasis", args___] := QuantumMeasurementOperator[{"ComputationalBasis"}, args]

QuantumMeasurementOperator[{"ComputationalBasis", dimension_Integer : 2}, args___, order : (_ ? orderQ) : {1}] :=
    QuantumMeasurementOperator[{"ComputationalBasis", dimension, Range[0, (dimension ^ Length[order]) - 1]}, args, order]

QuantumMeasurementOperator[{"ComputationalBasis", dimension_Integer : 2, eigenvalues_List}, args___, order : (_ ? orderQ) : {1}] :=
QuantumMeasurementOperator[eigenvalues . QuantumBasis[dimension, Length[order]]["Projectors"],
    dimension, Length[order],
    args,
    "Label" -> "Computational",
    order
]


QuantumMeasurementOperator["BellBasis", order : (_ ? orderQ) : {1}] := QuantumMeasurementOperator[{"BellBasis", Range[0, (4 ^ Length[order]) - 1]}, order]

QuantumMeasurementOperator[{"BellBasis", eigenvalues_List}, args___, order : (_ ? orderQ) : {1}] :=
 QuantumMeasurementOperator[eigenvalues . QuantumBasis["Bell", Length[order]]["Projectors"], args, order]


QuantumMeasurementOperator[name : "PauliXBasis" | "PauliYBasis" | "PauliZBasis", args___, order : (_ ? orderQ) : {1}] :=
 QuantumMeasurementOperator[{name, Range[0, (2 ^ Length[order]) - 1]}, args, order]

QuantumMeasurementOperator[{name : "PauliXBasis" | "PauliYBasis" | "PauliZBasis", eigenvalues_List}, args___, order : (_ ? orderQ) : {1}] :=
 QuantumMeasurementOperator[eigenvalues . QuantumBasis[StringDelete[name, "Basis"], Length[order]]["Projectors"], args, order]


 QuantumMeasurementOperator["FourierBasis", args___] := QuantumMeasurementOperator[{"FourierBasis"}, args]

QuantumMeasurementOperator[{"FourierBasis", dimension_Integer : 2}, args___, order : (_ ? orderQ) : {1}] :=
    QuantumMeasurementOperator[{"FourierBasis", dimension, Range[0, (dimension ^ Length[order]) - 1]}, args, order]

QuantumMeasurementOperator[{"FourierBasis", dimension_Integer : 2, eigenvalues_List}, args___, order : (_ ? orderQ) : {1}] :=
 QuantumMeasurementOperator[eigenvalues . QuantumBasis[{"Fourier", dimension}, Length[order]]["Projectors"], dimension, args, order]


QuantumMeasurementOperator["RandomHermitian", args___] := QuantumMeasurementOperator[{"RandomHermitian", 2}, args]

QuantumMeasurementOperator[{"RandomHermitian", dimension_Integer : 2}, args___, order : (_ ? orderQ) : {1}] :=
    QuantumMeasurementOperator[
        Orthogonalize[
            Table[
                RandomReal[NormalDistribution[0, 1 / 2]] + I RandomReal[NormalDistribution[0, 1 / 2]],
                dimension ^ Length[order], dimension ^ Length[order]
            ]
        ],
        dimension,
        args,
        order
    ]

QuantumMeasurementOperator[{matrix_ ? MatrixQ, eigenvalues_ ? VectorQ}, args___] :=
 QuantumMeasurementOperator[eigenvalues . (projector /@ matrix), args]

QuantumMeasurementOperator[{qb_ ? QuantumBasisQ, eigenvalues_ ? VectorQ}, args___] :=
 QuantumMeasurementOperator[eigenvalues . qb["Projectors"], qb, args]

QuantumMeasurementOperator[qb_ ? QuantumBasisQ, args___] := QuantumMeasurementOperator[{qb, Range[qb["Dimension"]] - 1}, args]

