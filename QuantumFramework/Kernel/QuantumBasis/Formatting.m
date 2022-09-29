Package["Wolfram`QuantumFramework`"]

PackageScope["basisDimensionSummaryItem"]
PackageScope["basisQuditsSummaryItem"]

PackageScope["QuantumDiagramGraphics"]



basisDimensionSummaryItem[o_] := If[o["InputDimension"] === o["OutputDimension"],
    {"Dimension: ", o["InputDimension"]},
    {"Dimensions: ", If[o["InputQudits"] > 0, MapAt[Style[#, Bold] &, o["Dimensions"], {- o["InputQudits"] ;; }], o["Dimensions"]]}
]

basisQuditsSummaryItem[o_] := {"Qudits: ", If[o["InputQudits"] === o["OutputQudits"], o["InputQudits"], {o["OutputQudits"], o["InputQudits"]}]}


QuantumBasis /: MakeBoxes[qb_QuantumBasis /; QuantumBasisQ[Unevaluated @ qb], format_] := With[{
    icon = If[
        qb["ElementDimension"] < 2 ^ 9,
        ComplexArrayPlot[
            Map[Replace[x_ ? (Not @* NumericQ) :> BlockRandom[RandomColor[], RandomSeeding -> Hash[x]]], qb["MatrixRepresentation"], {2}],
            ImageSize -> Dynamic @ {Automatic, 3.5 CurrentValue["FontCapHeight"] / AbsoluteCurrentValue[Magnification]},
            Frame -> False,
            FrameTicks -> None
        ],
        RawBoxes @ $SparseArrayBox
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
            BoxForm`SummaryItem[{"Qudits: ", {qb["OutputQudits"], qb["InputQudits"]}}]
        },
        {
            BoxForm`SummaryItem[{"Dimensions: ",
                If[qb["InputQudits"] > 0, MapAt[Style[#, Bold] &, qb["Dimensions"], {- qb["InputQudits"] ;; }], qb["Dimensions"]]}]
        },
        {
            BoxForm`SummaryItem[{"Element dimensions: ", qb["ElementDimensions"]}]
        }
    },
    format,
    "Interpretable" -> Automatic
    ]
]


Options[QuantumDiagramGraphics] = {
    "PointSize" -> 0.2,
    "CenterRatio" -> 1,
    "LabelStyle" -> {FontSize -> 24, FontFamily -> "Times"},
    "Rotate" -> 0,
    "Points" -> True,
    "Arrows" -> True,
    "Arrowheads" -> 0.004,
    "ArrowExtent" -> 1 / 5,
    "Shape" -> "Rectangle"
}

QuantumDiagramGraphics[qb_QuantumBasis ? QuantumBasisQ, OptionsPattern[]] :=
Module[{
    w, h,
    r = OptionValue["CenterRatio"],
    scale = Dynamic[3.5 CurrentValue["FontCapHeight"] / AbsoluteCurrentValue[Magnification]],
    labelPos,
    outputX, inputX,
    outputXY, inputXY,
    shape,
    box
},
    w = Max[qb["OutputQudits"], qb["InputQudits"], 1];
    h = Switch[OptionValue["Shape"], "Triangle" | "UpsideDownTriangle", 1 / 2, "Diamond", w, _, 1];

    outputX = Subdivide[(1 - r) / 2 w, (1 + r) / 2 w, qb["OutputQudits"] + 1][[2 ;; -2]];
    inputX = Subdivide[(1 - r) / 2 w, (1 + r) / 2 w, qb["InputQudits"] + 1][[2 ;; -2]];

    Switch[OptionValue["Shape"],

        "Triangle",
        shape = RegionBoundary @ Triangle[{{0, 0}, {w, 0}, {w / 2 , h}}];
        outputXY =  {#, If[# < w / 2, #, w - #] 2 h / w} & /@ outputX;
        inputXY = {#, 0} & /@ inputX,

        "UpsideDownTriangle",
        shape = RegionBoundary @ Triangle[{{0, h}, {w, h}, {w / 2 , 0}}];
        outputXY = {#, h} & /@ outputX;
        inputXY =  {#, h - If[# < w / 2, #, w - #] 2 h / w} & /@ inputX,

        "Diamond",
        shape = RegionBoundary @ Polygon[{{0, h / 2}, {w / 2, h}, {w , h / 2}, {w / 2, 0}}];
        outputXY =  {#, h / 2 + If[# < w / 2, #, w - #] h / w} & /@ outputX;
        inputXY =  {#, h / 2 - If[# < w / 2, #, w - #] h / w} & /@ inputX,

        _,
        shape = RegionBoundary @ Rectangle[{0, 0}, {w, h}];
        outputXY = {#, h} & /@ outputX;
        inputXY = {#, 0} & /@ inputX
    ];
    labelPos = {w / 2, h / 2};
    box = {
        shape,

        If[ TrueQ[OptionValue["Points"]], {
                PointSize[scale OptionValue["PointSize"]],
                Point @ outputXY,
                Point @ inputXY
            }
        ],

        If[ TrueQ[OptionValue["Arrows"]],
            {
                Arrowheads[scale OptionValue["Arrowheads"] h / w],
                MapThread[Arrow[If[#2, Reverse, Identity] @ {#1, #1 + {0, h OptionValue["ArrowExtent"]}}] &,
                    {outputXY, #["DualQ"] & /@ qb["Output"]["Decompose"]}],
                MapThread[Arrow[If[#2, Identity, Reverse] @ {#1 - {0, h OptionValue["ArrowExtent"]}, #1}] &,
                    {inputXY, #["DualQ"] & /@ qb["Input"]["Decompose"]}]
            }
        ]


    };
    box = GeometricTransformation[box, RotationTransform[OptionValue["Rotate"], labelPos]];
    Graphics[{box, Text[Style[qb["Label"], OptionValue["LabelStyle"]], labelPos]}]
]

