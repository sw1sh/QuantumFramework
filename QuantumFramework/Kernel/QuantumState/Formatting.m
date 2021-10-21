Package["Wolfram`QuantumFramework`"]



QuantumState /: MakeBoxes[qs_QuantumState /; Quiet @ QuantumStateQ[Unevaluated @ qs], format_] := Enclose[With[{
    icon = MatrixPlot[
        Enclose[
            ConfirmAssert[qs["Dimension"] < 2 ^ 11];
            Map[Replace[x_ ? (Not @* NumericQ) :> BlockRandom[RandomColor[], RandomSeeding -> Hash[x]]], ConfirmBy[qs["MatrixRepresentation"], MatrixQ], {2}],
            RandomReal[{0, 1}, If[qs["Dimension"] < 2 ^ 11, {qs["Dimension"], qs["Dimension"]}, {2 ^ 11, 2 ^ 11}]] &
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
            BoxForm`SummaryItem[{"Type: ", Quiet @ qs["Type"]}],
            BoxForm`SummaryItem[{"Dimension: ", qs["Dimension"]}]
        },
        {
            BoxForm`SummaryItem[{"Picture: ", qs["Picture"]}]
        }
    },
    {
        {
            BoxForm`SummaryItem[{"Purity: ", Enclose[ConfirmQuiet[N @ qs["Purity"]], Indeterminate &]}]
        },
        {
            BoxForm`SummaryItem[{"Von Neumann Entropy: ", Enclose[ConfirmQuiet[N @ qs["VonNeumannEntropy"]], Indeterminate &]}]
        },
        {
            BoxForm`SummaryItem[{"Dimensions: ",
                If[qs["InputQudits"] > 0, MapAt[Style[#, Bold] &, qs["Dimensions"], {- qs["InputQudits"] ;; }], qs["Dimensions"]]}]
        }
    },
    format,
    "Interpretable" -> Automatic
    ]
],
    ToBoxes[QuantumState[$Failed], format] &
]

