Package["Wolfram`QuantumFramework`"]

PackageScope["$QuantumMeasurementOperatorNames"]



$QuantumMeasurementOperatorNames = {"RandomHermitian", "WignerMICPOVM"}


QuantumMeasurementOperator[{"RandomHermitian", args___}, opts___] := With[{
    basis = QuantumBasis[args]
},
    QuantumMeasurementOperator[
        QuantumOperator[
            With[{m = RandomComplex[1 + I, {basis["Dimension"], basis["Dimension"]}]}, (m + ConjugateTranspose[m]) / 2],
            basis
        ],
        opts
]
]

QuantumMeasurementOperator[{"WignerMICPOVM", args___}, opts___] := Enclose @ FullSimplify @ QuantumMeasurementOperator[
    QuantumMeasurementOperator[ConfirmBy[QuantumWignerMICPOVM[args], ArrayQ[#, 3] &]],
    With[{basis = QuditBasis[{"WignerMIC", args}]},
        QuantumBasis[QuantumTensorProduct[QuditBasis[basis["Names"]], QuditBasis[Sqrt[basis["Dimension"]]]], QuditBasis[Sqrt[basis["Dimension"]]]]
    ],
    opts
]


QuantumMeasurementOperator[name_String | name_String[args___], opts___] /; MemberQ[$QuantumMeasurementOperatorNames, name] := QuantumMeasurementOperator[{name, args}, opts]

