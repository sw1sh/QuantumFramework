Package["Wolfram`QuantumFramework`"]


QuantumMeasurementOperator["RandomHermitian", args___, target : (_ ? orderQ) : {1}] := With[{
    basis = QuantumBasis[args]
},
    QuantumMeasurementOperator[
        QuantumOperator[Orthogonalize @
            Table[
                RandomReal[NormalDistribution[0, 1 / 2]] + I RandomReal[NormalDistribution[0, 1 / 2]],
                basis["Dimension"], basis["Dimension"]
            ],
            basis
        ],
        target
]
]

