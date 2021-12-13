Package["Wolfram`QuantumFramework`"]



QuantumHamiltonianOperator /: MakeBoxes[qho_QuantumHamiltonianOperator /; QuantumHamiltonianOperatorQ[Unevaluated @ qho], format_] := With[{
    icon = Plot[Evaluate @ qho["Eigenvalues"], Evaluate @ qho["ParameterSpec"],
        Axes -> False,
        ImageSize -> Dynamic @ {Automatic, 3.5 CurrentValue["FontCapHeight"] / AbsoluteCurrentValue[Magnification]},
        PlotRange -> All
    ]
},
    BoxForm`ArrangeSummaryBox["QuantumHamiltonianOperator", qho,
        icon, {
            {
                BoxForm`SummaryItem[{"Parameter: ", qho["Parameter"]}],
                BoxForm`SummaryItem[{"Arity: ", qho["Arity"]}]
            },
            {
                BoxForm`SummaryItem[{"Dimensions: ", qho["Dimensions"]}],
                BoxForm`SummaryItem[{"Order: ", qho["Order"]}]
            }
        },
        {
            {
                BoxForm`SummaryItem[{"Hermitian: ", qho["HermitianQ"]}]
            },
            {
                BoxForm`SummaryItem[{"Unitary: ", qho["UnitaryQ"]}]
            }
        },
        format,
        "Interpretable" -> Automatic
    ]
]

