Package["QuantumFramework`"]



QuantumBasis /: MakeBoxes[qb_QuantumBasis /; ValidQuantumBasisQ[qb], format_] := With[{
    icon = MatrixPlot[
        Map[Replace[x_ ? (Not @* NumericQ) :> BlockRandom[RandomColor[], RandomSeeding -> Hash[x]]], qb["MatrixRepresentation"], {2}],
        ImageSize -> Dynamic @ {Automatic, 3.5 CurrentValue["FontCapHeight"] / AbsoluteCurrentValue[Magnification]},
        Frame -> False,
        FrameTicks -> None
    ]
},
    BoxForm`ArrangeSummaryBox["QuantumBasis", qb, icon,
    {
        {
            BoxForm`SummaryItem[{"Picture: ", qb["Picture"]}],
            BoxForm`SummaryItem[{"Rank: ", qb["Rank"]}]
        },
        {
            BoxForm`SummaryItem[{"Dimension: ", qb["Dimension"]}]
        }
    },
    {
        {
            BoxForm`SummaryItem[{"Qudits: ", qb["Qudits"]}]
        },
        {
            BoxForm`SummaryItem[{"Dimensions: ", qb["Dimensions"]}]
        },
        {
            BoxForm`SummaryItem[{"Element dimensions: ", qb["BasisElementDimensions"]}]
        }
    },
    format,
    "Interpretable" -> Automatic
    ]
]

