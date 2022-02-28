Package["Wolfram`QuantumFramework`"]



QuantumChannel /: MakeBoxes[qc_QuantumChannel /; QuantumChannelQ[Unevaluated @ qc], format_] := Enclose[With[{
    icon = If[
        qc["Dimension"] < 2 ^ 9,
        MatrixPlot[
            Map[Replace[x_ ? (Not @* NumericQ) :> BlockRandom[RandomColor[], RandomSeeding -> Hash[x]]], qc["Sort"]["MatrixRepresentation"], {2}],
            ImageSize -> Dynamic @ {Automatic, 3.5 CurrentValue["FontCapHeight"] / AbsoluteCurrentValue[Magnification]},
            Frame -> False,
            FrameTicks -> None
        ],
        RawBoxes @ $SparseArrayBox
    ]
},
    BoxForm`ArrangeSummaryBox["QuantumChannel", qc,
        icon, {
            {
                BoxForm`SummaryItem[{"Picture: ", qc["Picture"]}],
                BoxForm`SummaryItem[{"Arity: ", qc["Arity"]}]
            },
            {
                BoxForm`SummaryItem[{"Dimension: ", Row[{qc["InputDimension"], "\[RightArrow]", qc["OutputDimension"]}]}],
                BoxForm`SummaryItem[{"Qudits: ", Row[{qc["InputQudits"], "\[RightArrow]", qc["OutputQudits"]}]}]
            }
        },
        {
            {
                BoxForm`SummaryItem[{"Hermitian: ", qc["HermitianQ"]}],
                BoxForm`SummaryItem[{"Order: ", Row[{qc["InputOrder"], "\[RightArrow]", qc["OutputOrder"]}]}]
            },
            {
                BoxForm`SummaryItem[{"Unitary: ", qc["UnitaryQ"]}],
                BoxForm`SummaryItem[{"Dimensions: ",
                    Row[{qc["InputDimensions"], "\[RightArrow]", qc["OutputDimensions"]}]}
                ]
            },
            {
                BoxForm`SummaryItem[{"ParameterArity: ", qc["ParameterArity"]}],
                BoxForm`SummaryItem[{"Parameters: ", qc["Parameters"]}]
            }
        },
        format,
        "Interpretable" -> Automatic
    ]
],
    ToBoxes[QuantumChannel[$Failed], format] &
]

