Package["Wolfram`QuantumFramework`"]



QuantumOperator /: MakeBoxes[qo_QuantumOperator, TraditionalForm] /; QuantumOperatorQ[qo] :=
    With[{formula = TooltipBox[
            StyleBox[First[If[qo["MatrixQ"], qo["State"]["Operator"], qo]["Formula"]], "ShowStringCharacters" -> False],
            ToBoxes[
                Column[{
                    qo["Label"],
                    If[ qo["Dimension"] == 0,
                        Nothing,
                        Row[{"QuantumOperator: ", If[qo["InputOrder"] === {}, {}, Thread[Subscript[qo["InputDimensions"], qo["FullInputOrder"]]]] ->
                            If[qo["OutputOrder"] === {}, {}, Thread[Subscript[qo["OutputDimensions"], qo["FullOutputOrder"]]]]}]
                    ]
                },
                    Alignment -> Center
                ]
            ]
        ]
    },
        InterpretationBox[formula, qo]
    ]

QuantumOperator /: MakeBoxes[qo_QuantumOperator /; QuantumOperatorQ[qo], format_] := Enclose[With[{
    icon = If[
        qo["Dimension"] < 2 ^ 9,
        ComplexArrayPlot[
            Map[Replace[{x_ ? (Not @* NumericQ) :> BlockRandom[RandomComplex[], RandomSeeding -> Hash[x]], x_ :> N[x]}], qo["Sort"]["MatrixRepresentation"], {2}],
            ImageSize -> Dynamic @ {Automatic, 3.5 CurrentValue["FontCapHeight"] / AbsoluteCurrentValue[Magnification]},
            Frame -> False,
            FrameTicks -> None
        ],
        RawBoxes @ $SparseArrayBox
    ]
},
    BoxForm`ArrangeSummaryBox["QuantumOperator", qo,
        If[qo["Label"] === None, icon, Tooltip[icon, qo["Label"]]],
        {
            {
                BoxForm`SummaryItem[{"Picture: ", qo["Picture"]}],
                BoxForm`SummaryItem[{"Arity: ", qo["Arity"]}]
            },
            {
                BoxForm`SummaryItem[{"Dimension: ", Row[{qo["InputDimension"], "\[RightArrow]", qo["OutputDimension"]}]}],
                BoxForm`SummaryItem[{"Qudits: ", Row[{qo["InputQudits"], "\[RightArrow]", qo["OutputQudits"]}]}]
            }
        },
        {
            {
                BoxForm`SummaryItem[{"Hermitian: ", TimeConstrained[qo["HermitianQ"], 1]}],
                BoxForm`SummaryItem[{"Order: ", Row[{qo["InputOrder"], "\[RightArrow]", qo["OutputOrder"]}]}]
            },
            {
                BoxForm`SummaryItem[{"Unitary: ", TimeConstrained[qo["UnitaryQ"], 1]}],
                BoxForm`SummaryItem[{"Dimensions: ",
                    Row[{qo["InputDimensions"], "\[RightArrow]", qo["OutputDimensions"]}]}
                ]
            },
            {
                BoxForm`SummaryItem[{"Parameters: ", qo["Parameters"]}]
            }
        },
        format,
        "Interpretable" -> Automatic
    ]
],
    ToBoxes[QuantumOperator[$Failed], format] &
]