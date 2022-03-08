Package["Wolfram`QuantumFramework`"]

PackageScope["$QuantumMeasurementOperatorNames"]


$QuantumMeasurementOperatorNames = {"RandomHermitian"}


QuantumMeasurementOperator["RandomHermitian", args___, target : (_ ? targetQ) : {1}] := With[{
    basis = QuantumBasis[args]
},
    QuantumMeasurementOperator[
        QuantumOperator[
            With[{m = RandomComplex[1 + I, {basis["Dimension"], basis["Dimension"]}]}, (m + ConjugateTranspose[m]) / 2],
            basis
        ],
        target
]
]

