Package["Wolfram`QuantumFramework`"]



QuantumState /: MakeBoxes[qs_QuantumState, TraditionalForm] /; QuantumStateQ[qs] :=
    With[{formula = TooltipBox[
            StyleBox[ToBoxes[qs["Formula"], StandardForm], "ShowStringCharacters" -> False],
            ToBoxes[
                Column[{
                    qs["Label"],
                    Row[{"QuantumState: ", qs["InputDimensions"] -> qs["OutputDimensions"]}]
                },
                    Alignment -> Center
                ]
            ]
        ]
    },
        InterpretationBox[formula, qs]
    ]

QuantumState /: MakeBoxes[qs_QuantumState /; QuantumStateQ[Unevaluated @ qs], format_] := Enclose[With[{
    icon = If[
        qs["Dimension"] < 2 ^ 9,
        ComplexArrayPlot[
            Map[Replace[{x_ ? (Not @* NumericQ) :> BlockRandom[RandomComplex[], RandomSeeding -> Hash[x]], x_ :> N[x]}], qs["MatrixRepresentation"], {2}],
            ImageSize -> Dynamic @ {Automatic, 3.5 CurrentValue["FontCapHeight"] / AbsoluteCurrentValue[Magnification]},
            Frame -> False,
            FrameTicks -> None
        ],
        RawBoxes @ $SparseArrayBox
    ]
},
    BoxForm`ArrangeSummaryBox["QuantumState", qs, Tooltip[icon, qs["Label"]],
    {
        {
            BoxForm`SummaryItem[{Row @ {qs["Type"], " ", ToLowerCase @ qs["Kind"]}}],
            BoxForm`SummaryItem[{"Qudits: ", qs["Qudits"]}]
        },
        {
            BoxForm`SummaryItem[{"Type: ", qs["StateType"]}],
            BoxForm`SummaryItem[{"Dimension: ", qs["Dimension"]}]
        },
        {
            BoxForm`SummaryItem[{"Picture: ", qs["Picture"]}]
        }
    },
    {
        {
            BoxForm`SummaryItem[{"Purity: ",
                If[qs["Dimension"] < 2 ^ 9, TimeConstrained[Enclose[ConfirmQuiet[N @ qs["Purity"]], Indeterminate &], 1], $Aborted]}
            ]
        },
        {
            BoxForm`SummaryItem[{"Von Neumann Entropy: ",
                If[qs["Dimension"] < 2 ^ 9, TimeConstrained[Enclose[ConfirmQuiet[N @ qs["VonNeumannEntropy"]], Indeterminate &], 1], $Aborted]}
            ]
        },
        {
            BoxForm`SummaryItem[{"Dimensions: ",
                If[qs["InputQudits"] > 0, MapAt[Style[#, Bold] &, qs["Dimensions"], {- qs["InputQudits"] ;; }], qs["Dimensions"]]}]
        },
        {
            BoxForm`SummaryItem[{"ParameterArity: ", qs["ParameterArity"]}],
            BoxForm`SummaryItem[{"Parameters: ", qs["Parameters"]}]
        }
    },
    format,
    "Interpretable" -> Automatic
    ]
],
    ToBoxes[QuantumState[$Failed], format] &
]

