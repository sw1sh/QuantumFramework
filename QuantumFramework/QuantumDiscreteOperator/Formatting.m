Package["QuantumFramework`"]



QuantumDiscreteOperator /: MakeBoxes[qdo_QuantumDiscreteOperator /; QuantumDiscreteOperatorQ[Unevaluated @ qdo], format_] := With[{
    icon = MatrixPlot[
        Enclose[
            Map[Replace[x_ ? (Not @* NumericQ) :> BlockRandom[RandomColor[], RandomSeeding -> Hash[x]]], Confirm[qdo["OrderedMatrixRepresentation"]], {2}],
            RandomReal[{0, 1}, {qdo["Dimension"], qdo["Dimension"]}] &
        ],
        ImageSize -> Dynamic @ {Automatic, 3.5 CurrentValue["FontCapHeight"] / AbsoluteCurrentValue[Magnification]},
        Frame -> False,
        FrameTicks -> None
    ]
},
    BoxForm`ArrangeSummaryBox["QuantumDiscreteOperator", qdo,
        icon, {
            {
                BoxForm`SummaryItem[{"Picture: ", qdo["Picture"]}],
                BoxForm`SummaryItem[{"Arity: ", qdo["Arity"]}]
            },
            {
                BoxForm`SummaryItem[If[Equal @@ qdo["Dimensions"], {"Dimension: ", qdo["InputDimension"]}, {"Dimensions: ", qdo["Dimensions"]}]],
                BoxForm`SummaryItem[{"Qudits: ",
                    If[qdo["InputQudits"] === qdo["OutputQudits"], qdo["InputQudits"], {qdo["OutputQudits"], qdo["InputQudits"]}]
                }]
            },
            {
                SpanFromLeft,
                BoxForm`SummaryItem[{"Order: ", qdo["Order"]}]
            }
        },
        {
            {
                BoxForm`SummaryItem[{"Hermitian: ", qdo["HermitianQ"]}]
            },
            {
                BoxForm`SummaryItem[{"Unitary: ", qdo["UnitaryQ"]}]
            }
        },
        format,
        "Interpretable" -> Automatic
    ]
]