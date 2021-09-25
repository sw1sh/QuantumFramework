Package["QuantumFramework`"]



QuantumMeasurementOperator /: MakeBoxes[qmo_QuantumMeasurementOperator /; QuantumMeasurementOperatorQ[Unevaluated @ qmo], format_] := With[{
    icon = MatrixPlot[
        Enclose[
            Map[Replace[x_ ? (Not @* NumericQ) :> BlockRandom[RandomColor[], RandomSeeding -> Hash[x]]], Confirm[qmo["OrderedMatrixRepresentation"]], {2}],
            RandomReal[{0, 1}, {qmo["Dimension"], qmo["Dimension"]}] &
        ],
        ImageSize -> Dynamic @ {Automatic, 3.5 CurrentValue["FontCapHeight"] / AbsoluteCurrentValue[Magnification]},
        Frame -> False,
        FrameTicks -> None
    ]
},
    BoxForm`ArrangeSummaryBox["QuantumMeasurementOperator", qmo,
        icon, {
            {
                BoxForm`SummaryItem[{"Measurement Type: ", qmo["Type"]}],
                BoxForm`SummaryItem[{"Arity: ", qmo["Arity"]}]
            },
            {
                BoxForm`SummaryItem[{"Qudits: ", qmo["InputQudits"]}],
                BoxForm`SummaryItem[{"Dimension: ", qmo["InputDimension"]}]
            },
            {
                SpanFromLeft,
                BoxForm`SummaryItem[{"Order: ", qmo["Order"]}]
            }
        },
        {
            {
                BoxForm`SummaryItem[{"Hermitian: ", qmo["HermitianQ"]}]
            },
            {
                BoxForm`SummaryItem[{"Unitary: ", qmo["UnitaryQ"]}]
            }
        },
        format,
        "Interpretable" -> Automatic
    ]
]

