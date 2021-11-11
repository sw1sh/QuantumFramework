Package["Wolfram`QuantumFramework`"]



QuantumCircuitOperator /: MakeBoxes[qco_QuantumCircuitOperator /; QuantumCircuitOperatorQ[Unevaluated @ qco], format_] := Enclose[
ConfirmQuiet @ BoxForm`ArrangeSummaryBox["QuantumCircuitOperator",
    qco,
    Magnify[qco["Diagram", FontSize -> 12], .2], {
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

