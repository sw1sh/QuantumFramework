Package["Wolfram`QuantumFramework`"]



QuantumOperator /: MakeBoxes[qo_QuantumOperator, TraditionalForm] /; QuantumOperatorQ[qo] :=
    With[{formula = TooltipBox[
            ToBoxes[qo["Formula"], StandardForm],
            ToBoxes[
                Row[{"QuantumOperator: ", Thread[Subscript[qo["InputDimensions"], qo["InputOrder"]]] ->
                    Thread[Subscript[qo["OutputDimensions"], qo["OutputOrder"]]]}]
            ]
        ]
    },
        InterpretationBox[formula, qo]
    ]

QuantumOperator /: MakeBoxes[qo_QuantumOperator /; QuantumOperatorQ[Unevaluated @ qo], format_] := Enclose[With[{
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
        icon, {
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
                BoxForm`SummaryItem[{"ParameterArity: ", qo["ParameterArity"]}],
                BoxForm`SummaryItem[{"Parameters: ", qo["Parameters"]}]
            }
        },
        format,
        "Interpretable" -> Automatic
    ]
],
    ToBoxes[QuantumOperator[$Failed], format] &
]