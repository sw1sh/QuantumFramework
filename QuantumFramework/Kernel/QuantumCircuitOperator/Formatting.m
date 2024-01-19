Package["Wolfram`QuantumFramework`"]



QuantumCircuitOperator /: MakeBoxes[qco_QuantumCircuitOperator, TraditionalForm] /; QuantumCircuitOperatorQ[qco] :=
    With[{diagram = ToBoxes[qco["Diagram"], StandardForm]},
        InterpretationBox[diagram, qco]
    ]

QuantumCircuitOperator /: MakeBoxes[qco_QuantumCircuitOperator /; QuantumCircuitOperatorQ[qco], format_] := Enclose[
BoxForm`ArrangeSummaryBox["QuantumCircuitOperator",
    qco,
    Tooltip[
        If[qco["GateCount"] <= 32, qco["Icon"], QuantumCircuitOperator[{{"Fourier", 3}}]["Icon", "GateBackgroundStyle" -> _ -> LightGray, "GateBoundaryStyle" -> _ -> Gray]],
        qco["Label"]
    ],
    {{}},
    {
        {
            BoxForm`SummaryItem[{"Gates: ", qco["GateCount"]}],
            BoxForm`SummaryItem[{"Depth: ", qco["Depth"]}]
        },
        {
            BoxForm`SummaryItem[{"Dimensions: ", qco["InputDimension"] -> qco["OutputDimension"]}]
        },
        {
            BoxForm`SummaryItem[{"Order: ", qco["InputOrder"] -> qco["OutputOrder"]}]
        },
        {
            BoxForm`SummaryItem[{"Measurement Target: ", qco["Target"]}]
        },
        {
            BoxForm`SummaryItem[{"Parameters: ", qco["Parameters"]}]
        }
    },
    format
],
    ToBoxes[QuantumCircuitOperator[$Failed], format] &
]

