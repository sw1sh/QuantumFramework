Package["QuantumFramework`"]



QuantumDiscreteState /: MakeBoxes[qds_QuantumDiscreteState /; Quiet @ QuantumDiscreteStateQ[Unevaluated @ qds], format_] := With[{
    icon = MatrixPlot[
        Map[Replace[x_ ? (Not @* NumericQ) :> BlockRandom[RandomColor[], RandomSeeding -> Hash[x]]], qds["DensityMatrix"], {2}],
        ImageSize -> Dynamic @ {Automatic, 3.5 CurrentValue["FontCapHeight"] / AbsoluteCurrentValue[Magnification]},
        Frame -> False,
        FrameTicks -> None
    ]
},
    BoxForm`ArrangeSummaryBox["QuantumDiscreteState", qds, icon,
    {
        {
            BoxForm`SummaryItem[{"StateType: ", qds["StateType"]}],
            BoxForm`SummaryItem[{"Qudits: ", qds["Qudits"]}]
        },
        {
            BoxForm`SummaryItem[{"Type: ", qds["Type"]}],
            BoxForm`SummaryItem[{"Dimension: ", qds["Dimension"]}]
        },
        {
            BoxForm`SummaryItem[{"Picture: ", qds["Picture"]}]
        }
    },
    {
        {
            BoxForm`SummaryItem[{"Purity: ", N @ qds["Purity"]}]
        },
        {
            BoxForm`SummaryItem[{"Von Neumann Entropy: ", N @ qds["VonNeumannEntropy"]}]
        }
    },
    format,
    "Interpretable" -> Automatic
    ]
]

