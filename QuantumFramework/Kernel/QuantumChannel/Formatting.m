Package["Wolfram`QuantumFramework`"]



QuantumChannel /: MakeBoxes[qc_QuantumChannel, TraditionalForm] /; QuantumChannelQ[qc] :=
    With[{boxes = ToBoxes[qc["QuantumOperator"], TraditionalForm]},
        InterpretationBox[boxes, qc]
    ]

QuantumChannel /: MakeBoxes[qc_QuantumChannel /; QuantumChannelQ[qc], format_] := Enclose[With[{
    icon = If[
        qc["Dimension"] < 2 ^ 9,
        ComplexArrayPlot[
            Map[Replace[{x_ ? (Not @* NumericQ) :> BlockRandom[RandomComplex[], RandomSeeding -> Hash[x]], x_ :> N[x]}], qc["Sort"]["MatrixRepresentation"], {2}],
            ImageSize -> Dynamic @ {Automatic, 3.5 CurrentValue["FontCapHeight"] / AbsoluteCurrentValue[Magnification]},
            Frame -> False,
            FrameTicks -> None
        ],
        RawBoxes @ $SparseArrayBox
    ]
},
    BoxForm`ArrangeSummaryBox["QuantumChannel", qc,
        Tooltip[icon, qc["Label"]], {
            {
                BoxForm`SummaryItem[{"Picture: ", qc["Picture"]}],
                BoxForm`SummaryItem[{"Arity: ", qc["Arity"]}]
            },
            {
                BoxForm`SummaryItem[{"Dimension: ", Row[{qc["InputDimension"]}]}],
                BoxForm`SummaryItem[{"Qudits: ", Row[{qc["InputQudits"]}]}]
            }
        },
        {
            {
                BoxForm`SummaryItem[{"Trace preserving: ", TimeConstrained[qc["TracePreservingQ"], 1]}]
            },
            {
                BoxForm`SummaryItem[{"Order: ", Row[{qc["InputOrder"]}]}],
                BoxForm`SummaryItem[{"Dimensions: ", Row[{qc["InputDimensions"]}]}]
            },
            {
                BoxForm`SummaryItem[{"Parameters: ", qc["Parameters"]}]
            }
        },
        format,
        "Interpretable" -> Automatic
    ]
],
    ToBoxes[QuantumChannel[$Failed], format] &
]

