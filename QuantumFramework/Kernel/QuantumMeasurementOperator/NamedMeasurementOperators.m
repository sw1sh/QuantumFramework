Package["Wolfram`QuantumFramework`"]

PackageScope["$QuantumMeasurementOperatorNames"]



$QuantumMeasurementOperatorNames = {"RandomHermitian", "WignerMICPOVM"}


QuantumMeasurementOperator[{"RandomHermitian", args___}, target : _ ? targetQ : {1}, opts___] := With[{
    basis = QuantumBasis[args, "Label" -> "Random"]
},
    QuantumMeasurementOperator[
        QuantumOperator[
            With[{m = RandomComplex[1 + I, {basis["Dimension"], basis["Dimension"]}]}, (m + ConjugateTranspose[m]) / 2],
            target,
            basis
        ],
        opts
]
]

QuantumMeasurementOperator[{"WignerMICPOVM", args___}, target : _ ? targetQ : {1}, opts___] := Enclose @ FullSimplify @ QuantumMeasurementOperator[
    QuantumMeasurementOperator[
        QuantumMeasurementOperator[ConfirmBy[QuantumWignerMICPOVM[args], ArrayQ[#, 3] &], target],
        With[{basis = QuditBasis[{"WignerMIC", args}]},
            QuantumBasis[QuantumTensorProduct[QuditBasis[basis["Names"]], QuditBasis[Sqrt[basis["Dimension"]]]], QuditBasis[Sqrt[basis["Dimension"]]], "Label" -> "WignerMIC"]
        ]
    ],
    opts
]


QuantumMeasurementOperator[name_String | name_String[args___], opts___] /; MemberQ[$QuantumMeasurementOperatorNames, name] := QuantumMeasurementOperator[{name, args}, opts]

