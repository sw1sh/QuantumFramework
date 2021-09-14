Package["QuantumFramework`"]

PackageScope["basisDimensionSummaryItem"]
PackageScope["basisQuditsSummaryItem"]



basisDimensionSummaryItem[o_] := If[o["InputDimension"] === o["OutputDimension"],
    {"Dimension: ", o["InputDimension"]},
    {"Dimensions: ", o["Dimensions"]}
]

basisQuditsSummaryItem[o_] := {"Qudits: ", If[o["InputQudits"] === o["OutputQudits"], o["InputQudits"], {o["OutputQudits"], o["InputQudits"]}]}


QuantumBasis /: MakeBoxes[qb_QuantumBasis /; QuantumBasisQ[Unevaluated @ qb], format_] := With[{
    icon = MatrixPlot[
        Check[
            Map[Replace[x_ ? (Not @* NumericQ) :> BlockRandom[RandomColor[], RandomSeeding -> Hash[x]]], qb["MatrixRepresentation"], {2}],
            RandomReal[{0, 1}, PadRight[qb["BasisElementDimensions"], 2, 2]]
        ],
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
            BoxForm`SummaryItem[basisDimensionSummaryItem[qb]]
        }
    },
    {
        {
            BoxForm`SummaryItem[basisQuditsSummaryItem[qb]]
        },
        {
            BoxForm`SummaryItem[{"Dimensions: ",
                If[qb["InputQudits"] > 0, MapAt[Style[#, Bold] &, qb["Dimensions"], {- qb["InputQudits"] ;; }], qb["Dimensions"]]}]
        },
        {
            BoxForm`SummaryItem[{"Element dimensions: ", qb["BasisElementDimensions"]}]
        }
    },
    format,
    "Interpretable" -> Automatic
    ]
]

