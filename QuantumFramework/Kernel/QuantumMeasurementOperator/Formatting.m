Package["Wolfram`QuantumFramework`"]



QuantumMeasurementOperator /: MakeBoxes[qmo_QuantumMeasurementOperator, TraditionalForm] /; QuantumMeasurementOperatorQ[Unevaluated[qmo]] :=
    With[{formula = ToBoxes[qmo["SuperOperator"], TraditionalForm]}, InterpretationBox[formula, qmo]]

QuantumMeasurementOperator /: MakeBoxes[qmo_QuantumMeasurementOperator /; QuantumMeasurementOperatorQ[Unevaluated[qmo]], format_] := With[{
    icon = If[
        qmo["Dimension"] < 2 ^ 9,
        ComplexArrayPlot[
            Map[Replace[{x_ ? (Not @* NumericQ) :> BlockRandom[RandomComplex[], RandomSeeding -> Hash[x]], x_ :> N[x]}],
                If[qmo["ProjectionQ"], QuantumOperator, Identity][qmo]["Sort"]["MatrixRepresentation"],
                {2}
            ],
            ImageSize -> Dynamic @ {Automatic, 3.5 CurrentValue["FontCapHeight"] / AbsoluteCurrentValue[Magnification]},
            Frame -> False,
            FrameTicks -> None
        ],
        RawBoxes @ $SparseArrayBox
    ]
},
    BoxForm`ArrangeSummaryBox["QuantumMeasurementOperator", qmo,
        Tooltip[icon, qmo["Label"]], {
            {
                BoxForm`SummaryItem[{"Measurement Type: ", qmo["Type"]}],
                BoxForm`SummaryItem[{"Target: ", qmo["Target"]}]
            },
            {
                BoxForm`SummaryItem[{"Dimension: ", Row[{qmo["InputDimension"], "\[RightArrow]", qmo["OutputDimension"]}]}],
                BoxForm`SummaryItem[{"Order: ", Row[{qmo["InputOrder"], "\[RightArrow]", qmo["OutputOrder"]}]}]
            },
            If[ qmo["Picture"] === "Schrodinger",
                Nothing, 
                {
                    BoxForm`SummaryItem[{"Picture: ", qmo["Picture"]}]
                }
            ]
        },
        {
            {
                BoxForm`SummaryItem[{"Hermitian: ", TimeConstrained[qmo["HermitianQ"], 1]}],
                BoxForm`SummaryItem[{"Qudits: ", Row[{qmo["InputQudits"], "\[RightArrow]", qmo["OutputQudits"]}]}]
            },
            {
                BoxForm`SummaryItem[{"Unitary: ", TimeConstrained[qmo["UnitaryQ"], 1]}],
                BoxForm`SummaryItem[{"Dimensions: ",
                    Row[{qmo["InputDimensions"], "\[RightArrow]", qmo["OutputDimensions"]}]}
                ]
            },
            {
                BoxForm`SummaryItem[{"Parameters: ", qmo["Parameters"]}]
            }
        },
        format,
        "Interpretable" -> Automatic
    ]
]

