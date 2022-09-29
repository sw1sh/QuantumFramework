Package["Wolfram`QuantumFramework`"]



QuantumCircuitOperator /: MakeBoxes[qco_QuantumCircuitOperator, TraditionalForm] /; QuantumCircuitOperatorQ[qco] :=
    With[{diagram = TooltipBox[
            ToBoxes[qco["Diagram"], StandardForm],
            ToBoxes[
                Row[{"QuantumCircuitOperator: ", <|"Depth" -> qco["Depth"], "Width" -> qco["Width"]|>}]
            ]]
    },
        InterpretationBox[diagram, qco]
    ]

QuantumCircuitOperator /: MakeBoxes[qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ, format_] := Enclose[
BoxForm`ArrangeSummaryBox["QuantumCircuitOperator",
    qco,
    qco["Diagram", "ShowTextLabels" -> False, "ShowMeasurementWire" -> False, "SubcircuitOptions" -> {"ShowLabel" -> False}, ImageSize -> Tiny],
    {
        {}
    },
    {
        {
            BoxForm`SummaryItem[{"Gates: ", qco["Gates"]}]
        },
        {
            BoxForm`SummaryItem[{"Dimension: ", qco["InputDimension"]}]
        },
        {
            BoxForm`SummaryItem[{"Order: ", qco["InputOrder"]}]
        },
        {
            BoxForm`SummaryItem[{"Target: ", qco["Target"]}]
        }
    },
    format
],
    ToBoxes[QuantumCircuitOperator[$Failed], format] &
]

