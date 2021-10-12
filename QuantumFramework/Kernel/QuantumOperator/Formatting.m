Package["Wolfram`QuantumFramework`"]



QuantumOperator /: MakeBoxes[qo_QuantumOperator /; QuantumOperatorQ[Unevaluated @ qo], format_] := Enclose[With[{
    icon = MatrixPlot[
        Enclose[
            Map[Replace[x_ ? (Not @* NumericQ) :> BlockRandom[RandomColor[], RandomSeeding -> Hash[x]]], ConfirmBy[qo["Sort"]["MatrixRepresentation"], MatrixQ], {2}],
            RandomReal[{0, 1}, {qo["Dimension"], qo["Dimension"]}] &
        ],
        ImageSize -> Dynamic @ {Automatic, 3.5 CurrentValue["FontCapHeight"] / AbsoluteCurrentValue[Magnification]},
        Frame -> False,
        FrameTicks -> None
    ]
},
    BoxForm`ArrangeSummaryBox["QuantumOperator", qo,
        icon, {
            {
                BoxForm`SummaryItem[{"Picture: ", qo["Picture"]}],
                BoxForm`SummaryItem[{"Arity: ", qo["Arity"]}]
            },
            {
                BoxForm`SummaryItem[basisDimensionSummaryItem[qo]],
                BoxForm`SummaryItem[basisQuditsSummaryItem[qo]]
            }
        },
        {
            {
                BoxForm`SummaryItem[{"Hermitian: ", qo["HermitianQ"]}],
                BoxForm`SummaryItem[{"Output Order: ", qo["OutputOrder"]}],
                BoxForm`SummaryItem[{"Input Order: ", qo["InputOrder"]}]
            },
            {
                BoxForm`SummaryItem[{"Unitary: ", qo["UnitaryQ"]}],
                BoxForm`SummaryItem[{"Dimensions: ", qo["Dimensions"]}]
            }
        },
        format,
        "Interpretable" -> Automatic
    ]
],
    ToBoxes[QuantumOperator[$Failed], format] &
]