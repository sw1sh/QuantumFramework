Package["QuantumFramework`"]



QuantumCircuitOperator /: MakeBoxes[qco_QuantumCircuitOperator /; QuantumCircuitOperatorQ[Unevaluated @ qco], format_] :=
BoxForm`ArrangeSummaryBox["QuantumCircuitOperator",
    qco,
    qco["Diagram"], {
        {
            BoxForm`SummaryItem[{"Gates: ", qco["Gates"]}],
            BoxForm`SummaryItem[{"Arity: ", qco["Arity"]}]
        }, {
            BoxForm`SummaryItem[{"Dimension: ", qco["InputDimension"]}],
            BoxForm`SummaryItem[{"Order: ", First @ qco["Orders"]}]
        }
    }, {
        {
            BoxForm`SummaryItem[{"Hermitian: ", qco["HermitianQ"]}]
        }, {
            BoxForm`SummaryItem[{"Unitary: ", qco["UnitaryQ"]}]
        }
    },
    format,
    "Interpretable" -> Automatic
]

