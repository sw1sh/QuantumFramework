Package["Wolfram`QuantumFramework`"]

PackageScope["$QuantumMeasurementOperatorNames"]



$QuantumMeasurementOperatorNames = {"RandomHermitian", "WignerMICPOVM", "Tetrahedron"}


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

QuantumMeasurementOperator[{"WignerMICPOVM", args___}, target : _ ? targetQ : {1}, opts___] := Enclose @ Simplify @ QuantumMeasurementOperator[
    QuantumMeasurementOperator[
        QuantumMeasurementOperator[ConfirmBy[QuantumWignerMICPOVM[args], ArrayQ[#, 3] &], target],
        With[{basis = QuditBasis[{"WignerMIC", args}]},
            QuantumBasis[QuantumTensorProduct[QuditBasis[basis["Names"]], QuditBasis[Sqrt[basis["Dimension"]]]], QuditBasis[Sqrt[basis["Dimension"]]], "Label" -> "WignerMIC"]
        ]
    ],
    opts
]

QuantumMeasurementOperator[{"Tetrahedron", ___}, opts___] :=
    Simplify @ QuantumMeasurementOperator[
        KroneckerProduct[#, Conjugate[#]] / 2 & /@
        {
            {1, 0},
            {1, Sqrt[2]} / Sqrt[3],
            {1, Sqrt[2] E ^ (I 2 Pi / 3)} / Sqrt[3],
            {1, Sqrt[2] E ^ (I 4 Pi / 3)} / Sqrt[3]
        },
        opts
    ]


QuantumMeasurementOperator[name_String | name_String[args___], opts___] /; MemberQ[$QuantumMeasurementOperatorNames, name] := QuantumMeasurementOperator[{name, args}, opts]

