Package["QuantumFramework`"]



QuantumState /: MakeBoxes[qs_QuantumState /; Quiet @ QuantumStateQ[Unevaluated @ qs], format_] := With[{
    icon = MatrixPlot[
        Check[
            Map[Replace[x_ ? (Not @* NumericQ) :> BlockRandom[RandomColor[], RandomSeeding -> Hash[x]]], qs["Matrix"], {2}],
            RandomReal[{0, 1}, {qs["Dimension"], qs["Dimension"]}]
        ],
        ImageSize -> Dynamic @ {Automatic, 3.5 CurrentValue["FontCapHeight"] / AbsoluteCurrentValue[Magnification]},
        Frame -> False,
        FrameTicks -> None
    ]
},
    BoxForm`ArrangeSummaryBox["QuantumState", qs, icon,
    {
        {
            BoxForm`SummaryItem[{"StateType: ", qs["StateType"]}],
            BoxForm`SummaryItem[{"Qudits: ", qs["Qudits"]}]
        },
        {
            BoxForm`SummaryItem[{"Type: ", qs["Type"]}],
            BoxForm`SummaryItem[{"Dimension: ", qs["Dimension"]}]
        },
        {
            BoxForm`SummaryItem[{"Picture: ", qs["Picture"]}]
        }
    },
    {
        {
            BoxForm`SummaryItem[{"Purity: ", Enclose[ConfirmQuiet[N @ qs["Purity"]], $Failed &]}]
        },
        {
            BoxForm`SummaryItem[{"Von Neumann Entropy: ", Enclose[ConfirmQuiet[N @ qs["VonNeumannEntropy"]], $Failed &]}]
        },
        {
            BoxForm`SummaryItem[{"Dimensions: ", MapAt[Style[#, Bold] &, qs["Dimensions"], {;; qs["InputQudits"]}]}]
        }
    },
    format,
    "Interpretable" -> Automatic
    ]
]

