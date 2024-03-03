Package["Wolfram`QuantumFramework`"]

PackageScope["$QuantumMeasurementOperatorNames"]



$QuantumMeasurementOperatorNames = {"RandomHermitian", "WignerMICPOVM", "GellMannMICPOVM", "TetrahedronSICPOVM", "RandomPOVM"}


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

QuantumMeasurementOperator[{"GellMannMICPOVM", d : _Integer ? Positive : 2}, opts___] := 
    QuantumMeasurementOperator[
        QuantumMeasurementOperator[
            GellMannMICPOVM[d],
            QuantumBasis[QuantumTensorProduct[QuditBasis[Subscript["\[ScriptCapitalG]", #] & /@ Range[d ^ 2]], QuditBasis[d]], QuditBasis[d], "Label" -> "GellMannMIC"]
        ],
        opts
    ]

QuantumMeasurementOperator[{"RandomPOVM", d : _Integer ? Positive : 2, methodOpts : OptionsPattern[]}, opts___] := 
    Enclose @ QuantumMeasurementOperator[
        With[{
            povm = ConfirmBy[Replace[OptionValue[{methodOpts, Method -> "Haar"}, Method], {name_, args___} | name_ :>
                Switch[name, "Haar", RandomHaarPOVM, "Bloch", RandomBlochMICPOVM][d, args]], TensorQ]
        },
            QuantumMeasurementOperator[
                povm,
                QuantumBasis[QuantumTensorProduct[QuditBasis[Subscript["\[ScriptCapitalR]", #] & /@ Range[Length[povm]]], QuditBasis[d]], QuditBasis[d], "Label" -> "RandomMIC"]
            ]
        ],
        opts
    ]

QuantumMeasurementOperator[{"TetrahedronSICPOVM", HoldPattern[angles : PatternSequence[__] : Sequence[0, 0, 0]]}, opts___] :=
    Simplify @ QuantumMeasurementOperator[
        QuantumMeasurementOperator[
            KroneckerProduct[#, Conjugate[#]] / 2 & [QuantumOperator["U"[angles]]["Matrix"] . #] & /@ {
                {1, 0},
                {1, Sqrt[2] E ^ (I 4 Pi / 3)} / Sqrt[3],
                {1, Sqrt[2] E ^ (I 2 Pi / 3)} / Sqrt[3],
                {1, Sqrt[2]} / Sqrt[3]
            },
            QuantumBasis[QuantumTensorProduct[QuditBasis[Subscript["\[ScriptCapitalT]", #] & /@ Range[4]], QuditBasis[2]], QuditBasis[2], "Label" -> "TetrahedronSIC"]
        ],
        opts
    ]


QuantumMeasurementOperator[name_String | name_String[args___], opts___] /; MemberQ[$QuantumMeasurementOperatorNames, name] := QuantumMeasurementOperator[{name, args}, opts]

