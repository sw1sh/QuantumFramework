Package["Wolfram`QuantumFramework`"]



QuantumCircuitOperator /: MakeBoxes[qco_QuantumCircuitOperator /; QuantumCircuitOperatorQ[Unevaluated[qco]], TraditionalForm] :=
    With[{diagram = ToBoxes[qco["Diagram"], StandardForm]},
        InterpretationBox[diagram, qco]
    ]

QuantumCircuitOperator /: MakeBoxes[qco_QuantumCircuitOperator /; QuantumCircuitOperatorQ[Unevaluated[qco]], format_] := Enclose[
BoxForm`ArrangeSummaryBox["QuantumCircuitOperator",
    qco,
    Tooltip[
        If[qco["GateCount"] <= 32, qco["Icon"], QuantumCircuitOperator[{{"Fourier", 3}}]["Icon", "GateBackgroundStyle" -> _ -> LightGray, "GateBoundaryStyle" -> _ -> Gray]],
        qco["Label"]
    ],
    {{}},
    {
        BoxForm`SummaryItem[{"Gates: ", qco["GateCount"]}],
        BoxForm`SummaryItem[{"Depth: ", qco["Depth"]}],
        BoxForm`SummaryItem[{"Width: ", qco["Width"]}],
        BoxForm`SummaryItem[{"Order: ", qco["InputOrder"] -> qco["OutputOrder"]}],
        BoxForm`SummaryItem[{"Dimensions: ", qco["InputDimensions"] -> qco["OutputDimensions"]}],
        BoxForm`SummaryItem[{"Measurement Target: ", qco["Target"]}],
        BoxForm`SummaryItem[{"Parameters: ", qco["Parameters"]}]
    },
    format
],
    ToBoxes[QuantumCircuitOperator[$Failed], format] &
]

