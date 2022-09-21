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
    qco["Diagram", FontSize -> 1, "MeasurementWireLabel" -> "", ImageSize -> Tiny],
    {
        {
            BoxForm`SummaryItem[{"Gates: ", qco["Gates"]}]
        }
    },
    {
        {
            BoxForm`SummaryItem[{"Dimension: ", Row[{qco["InputDimension"], "\[RightArrow]", qco["InputDimension"]}]}],
            BoxForm`SummaryItem[{"Order: ", Row[{qco["InputOrder"], "\[RightArrow]", qco["OutputOrder"]}]}],
            BoxForm`SummaryItem[{"Target: ", qco["Target"]}]
        }
    },
    format,
    "Interpretable" -> Automatic
],
    ToBoxes[QuantumCircuitOperator[$Failed], format] &
]

