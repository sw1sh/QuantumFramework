Package["Wolfram`QuantumFramework`"]

PackageScope["drawGateGraphics"]



drawNotGate[coordinates_List, _] := Module[{whiteRadius, whiteCircle, lines},
    whiteRadius = 0.8;
    whiteCircle = Graphics[{EdgeForm[Thickness[.0025]], Circle[coordinates, whiteRadius]}];
    lines = Graphics[{
        Line[{{First[coordinates] - whiteRadius, Last[coordinates]}, {First[coordinates] + whiteRadius, Last[coordinates]}}],
        Line[{{First[coordinates], Last[coordinates] - whiteRadius}, {First[coordinates], Last[coordinates] + whiteRadius}}]}
    ];
    Show[whiteCircle, lines]
]


drawUnaryGate[coordinates_List, name_, opts : OptionsPattern[Style]] := Module[{width, height, textGraphics, rectangle},
    width = 4;
    height = 3;
    textGraphics = Graphics[Text[Style[name, opts], coordinates]];
    rectangle = Graphics[{
        EdgeForm[Thickness[0.0025]], White,
        Rectangle[{First[coordinates] - width / 2, Last[coordinates] - height / 2}, {First[coordinates] + width / 2, Last[coordinates] + height / 2}]
    }];
    Show[rectangle, textGraphics]
]


drawBinaryGate[{coordinates1_List, coordinates2_List}, name_, opts : OptionsPattern[Style]] := Module[{width, height, averageCoordinates, textGraphics, rectangle},
    width = 4;
    height = 3;
    averageCoordinates = (coordinates1 + coordinates2) / 2;
    textGraphics = Graphics[Text[Style[name, opts], averageCoordinates]];
    rectangle = Graphics[{EdgeForm[Thickness[0.0025]], White,
        Rectangle[{First[coordinates1] - width / 2, Last[coordinates1] - height / 2}, {First[coordinates2] + width / 2, Last[coordinates2] + height / 2}]}];
    Show[rectangle, textGraphics]
]


drawSwapGate[qudit1Coordinates_List, qudit2Coordinates_List, opts : OptionsPattern[Style]] := Module[{cross1, cross2, verticalLine},
    cross1 = Graphics[{
        Line[{qudit1Coordinates + {-1, -1}, qudit1Coordinates + {1, 1}}],
        Line[{qudit1Coordinates + {1, -1}, qudit1Coordinates + {-1, 1}}]}];
    cross2 = Graphics[{
        Line[{qudit2Coordinates + {-1, -1}, qudit2Coordinates + {1, 1}}],
        Line[{qudit2Coordinates + {1, -1}, qudit2Coordinates + {-1, 1}}]}];
    verticalLine = Graphics[Line[{qudit1Coordinates, qudit2Coordinates}]];
    Show[cross1, cross2, verticalLine]
]


drawRootSwapGate[qudit1Coordinates_List, qudit2Coordinates_List, opts : OptionsPattern[Style]] := Module[{
    width, height, cross1, cross2, verticalLine, averageCoordinates, textGraphics, rectangle
},
        width = 4;
        height = 3;
        cross1 = Graphics[{
            Line[{qudit1Coordinates + {-1, -1}, qudit1Coordinates + {1, 1}}],
            Line[{qudit1Coordinates + {1, -1}, qudit1Coordinates + {-1, 1}}]}
        ];
        cross2 = Graphics[{
            Line[{qudit2Coordinates + {-1, -1}, qudit2Coordinates + {1, 1}}],
            Line[{qudit2Coordinates + {1, -1}, qudit2Coordinates + {-1, 1}}]}]
        ;
        verticalLine = Graphics[Line[{qudit1Coordinates, qudit2Coordinates}]];
        averageCoordinates = (qudit1Coordinates + qudit2Coordinates) / 2;
        textGraphics = Graphics[Text[Style["1/2", opts], averageCoordinates]];
        rectangle = Graphics[{
            EdgeForm[Thickness[0.005]], White,
            Rectangle[
                {First[averageCoordinates] - width / 2, Last[averageCoordinates] - height / 2},
                {First[averageCoordinates] + width / 2, Last[averageCoordinates] + height / 2}
            ]}
        ];
        Show[cross1, cross2, verticalLine, rectangle, textGraphics]
    ]


drawControlGateTop[coordinates_List, controlOrder_List, targetOrder_List] := Module[{blackCircleRadius, blackCircles, lines},
    blackCircleRadius = 0.5;
    blackCircles = Graphics[Table[Disk[coordinates[[i]], blackCircleRadius], {i, Length[controlOrder]}]];
    lines = Graphics[
        Table[
            Line[{coordinates[[i]], {First[coordinates[[i]]], Last[coordinates[[i]]] + 5 (controlOrder[[i]]-Max[targetOrder])}}],
            {i, Length[controlOrder]}
        ]
    ];
    Show[blackCircles, lines]
] /; (Length[coordinates] > 0 && Length[controlOrder] > 0 && Length[coordinates] == Length[controlOrder])


drawControlGateBottom[coordinates_List, controlOrder_List, targetOrder_List] := Module[{blackCircleRadius, blackCircles, lines},
    blackCircleRadius = 0.5;
    blackCircles = Graphics[Table[Disk[coordinates[[i]], blackCircleRadius], {i, Length[controlOrder]}]];
    lines = Graphics[
        Table[
            Line[{coordinates[[i]], {First[coordinates[[i]]], Last[coordinates[[i]]] - 5 (Min[targetOrder]-controlOrder[[i]])}}],
            {i, Length[controlOrder]}
        ]
    ];
    Show[blackCircles, lines]
] /;(Length[coordinates] > 0 && Length[controlOrder] > 0 && Length[coordinates] == Length[controlOrder])


drawMeasurementGate[coordinates_List, order_List, name_, opts : OptionsPattern[Style]] := Module[{width, height, radius, semiCircle, arrow, frame},
    width = 4;
    height = 5 Length[order] - 3;
    radius = - 0.3 + width / 2;
    semiCircle = Graphics[{Thickness[0.0025], Circle[coordinates, radius, {0, Pi}]}];
    arrow = Graphics[{Thickness[0.0025], Arrowheads[0.02], Arrow[{coordinates, {First[coordinates] + 1, Last[coordinates] + 1.4}}]}];
    frame = Graphics[{
        Text[
            Style[name, FontSize -> OptionValue[FontSize] / 2, opts],
            {First[coordinates], Last[coordinates] - 0.9}
        ],
        EdgeForm[Thickness[0.0025]],
        FaceForm[],
        Rectangle[{First[coordinates]-width / 2, Last[coordinates] - 5 / 3}, {First[coordinates] + width / 2, Last[coordinates] + height}]}];
    Show[semiCircle, arrow, frame]
]

drawMeasurement[coordinates_] := Module[{width = 4},
    Graphics[{
        Thickness[0.0025],
        Line[{coordinates + {.125, width / 2 + 0.05}, coordinates {1, 0}  + {.125, - 1}}],
        Line[{coordinates + {-.125, width / 2 + 0.05}, coordinates {1, 0} + {-.125, - 1}}],
        Polygon[coordinates {1, 0} + {0, - 1.4} + # & /@ {{-.75, 0}, {.75, 0}, {0, 1.25}}]
    }
    ]
]


drawWireGraphics[positionIndices_List, opts : OptionsPattern[Style]] := Module[{quditCount, lineList},
    quditCount = Length[positionIndices];
    lineList = Table[
        Graphics[{
            Text[Style[ToString[i], opts], {0, - 5 i - 1}],
            Line[{{0, - 5 i}, {6 Max[positionIndices], - 5 i}}]
            }
        ],
        {i, Range[quditCount]}
    ]
]

drawMeasurementWire[positionIndices_List, opts : OptionsPattern[Style]] := Graphics[{
    Text[Style["c", opts], {0, - 1.25}],
    Thickness[0.0025],
    Line[{{0, -.125}, {6 Max[positionIndices], -.125}}],
    Line[{{0, 0.125}, {6 Max[positionIndices], .125}}]
}]


Options[drawGateGraphics] = {"LabelStyle" -> {FontSize -> 24, FontFamily -> "Times"}}

drawGateGraphics[gates_List, OptionsPattern[]] := Module[{
    width, height, orders, dimensions, graphicsList, index, positionIndices, gatePositionIndices,
    targetQuditsOrder, controlQuditsOrder, controlQuditsOrderTop, controlQuditsOrderBottom, controlQuditsTopCoordinates, controlQuditsBottomCoordinates,
    includeMeasurement = False,
    styleOpts
},
    width = 4;
    height = 3;
    orders = #["InputOrder"] & /@ gates;
    dimensions = First[gates]["InputDimensions"];
    styleOpts = OptionValue["LabelStyle"];

    graphicsList = {};
    index = 1;
    positionIndices = ConstantArray[1, Max[Flatten[orders]]];
    Do[

    If[ QuantumMeasurementOperatorQ[gates[[i]]],
        gatePositionIndices = Table[Max[positionIndices], {j, Min[orders[[i]]], Max[orders[[i]]]}];
        includeMeasurement = True;
        Which[
            gates[[i]]["POVMQ"],
            AppendTo[graphicsList, drawMeasurementGate[{-2 + 6 Max[gatePositionIndices], - 5 Max[orders[[i]]]}, orders[[i]], "POVM", styleOpts]],
            True,
            AppendTo[
                graphicsList,
                drawMeasurementGate[
                    {-2 + 6 Max[gatePositionIndices], - 5 Max[orders[[i]]]},
                    orders[[i]],
                    gates[[i]]["Label"] /. None | "Computational" | "Computational"[_] -> "M",
                    styleOpts
                ]
            ]
        ];
        AppendTo[graphicsList, drawMeasurement[{-2 + 6 Max[gatePositionIndices], - 5 Min[orders[[i]]]}]];
        positionIndices = ConstantArray[Max[positionIndices] + 1, Length[positionIndices]];
    ];
    If[ QuantumOperatorQ[gates[[i]]],
        gatePositionIndices = Table[positionIndices[[j]], {j, Min[orders[[i]]], Max[orders[[i]]]}];
        If[ MatchQ[gates[[i]]["Label"], "CX" | "CY" | "CZ" | "CNOT" | "CPHASE" | "CSWAP" | "Controlled"[__]],
            If[ MatchQ[gates[[i]]["Label"], "Controlled"[__]],
                controlQuditsOrder = If[Length[gates[[i]]["Label"]] > 1, gates[[i]]["Label"][[2]], orders[[i]][[;; 1]]];
                targetQuditsOrder = Complement[orders[[i]], controlQuditsOrder],
                targetQuditsOrder = Rest @ orders[[i]];
                controlQuditsOrder = {First @ orders[[i]]}
            ];
            controlQuditsOrderTop = {};
            controlQuditsOrderBottom = {};
            Do[ Which[
                controlQuditsOrder[[j]] > Max[targetQuditsOrder],
                AppendTo[controlQuditsOrderTop, controlQuditsOrder[[j]]],
                controlQuditsOrder[[j]] < Min[targetQuditsOrder],
                AppendTo[controlQuditsOrderBottom, controlQuditsOrder[[j]]]],
                {j, Length[controlQuditsOrder]}
            ];
            controlQuditsTopCoordinates = Table[{-2 + 6 Max[gatePositionIndices], - 5 controlQuditsOrderTop[[j]]}, {j, Length[controlQuditsOrderTop]}];
            controlQuditsBottomCoordinates = Table[{-2 + 6 Max[gatePositionIndices], - 5 controlQuditsOrderBottom[[j]]}, {j, Length[controlQuditsOrderBottom]}];
            If[ Length[controlQuditsOrderTop] > 0,
                AppendTo[graphicsList, drawControlGateBottom[controlQuditsTopCoordinates, controlQuditsOrderTop, targetQuditsOrder]]
            ];
            If[ Length[controlQuditsOrderBottom] > 0,
                AppendTo[graphicsList, drawControlGateTop[controlQuditsBottomCoordinates, controlQuditsOrderBottom, targetQuditsOrder]]
            ],

            targetQuditsOrder = orders[[i]]
        ];
        AppendTo[
            graphicsList,
            Switch[
                gates[[i]]["Label"] /. "Controlled"[x_, ___] :> x,
                "SWAP",
                drawSwapGate[{-2 + 6 Max[gatePositionIndices], - 5 First[targetQuditsOrder]}, {-2 + 6 Max[gatePositionIndices], - 5 Last[targetQuditsOrder]}, styleOpts],
                "RootSWAP",
                drawRootSwapGate[{-2 + 6 Max[gatePositionIndices], - 5 First[targetQuditsOrder]}, {-2 + 6 Max[gatePositionIndices], - 5 Last[targetQuditsOrder]}, styleOpts],
                "NOT",
                drawNotGate[{-2 + 6 Max[gatePositionIndices], - 5 First[targetQuditsOrder]}, styleOpts],
                _,
                If[ gates[[i]]["Arity"] == 1,
                    drawUnaryGate[
                        {-2 + 6 Max[gatePositionIndices], - 5 First[targetQuditsOrder]},
                        gates[[i]]["Label"] /. {Composition -> SmallCircle, "Controlled"[x_, ___] :> x} /. None :> Subscript["U", index++],
                        styleOpts
                    ],
                    drawBinaryGate[
                        {{-2 + 6 Max[gatePositionIndices], - 5 Max[targetQuditsOrder]}, {-2 + 6 Max[gatePositionIndices], - 5 Min[targetQuditsOrder]}},
                        gates[[i]]["Label"] /. {Composition -> SmallCircle, "Controlled"[x_, ___] :> x} /. None :> Subscript["U", index++],
                        styleOpts
                    ]
                ]
            ]
        ];
        positionIndices[[ Range[Min[orders[[i]]], Max[orders[[i]]]] ]] = Max[positionIndices[[ Range[Min[orders[[i]]], Max[orders[[i]]]] ]]] + 1;
    ],
        {i, Length[gates]}
    ];
    (*
        Do[positionIndices[[j]]=positionIndices[[orders[[i, First[First[Position[gatePositionIndices, Max[gatePositionIndices]]]]]]]], {j, Min[orders[[i]]],
        Max[orders[[i]]]}]
    *)
    PrependTo[graphicsList, drawWireGraphics[positionIndices, styleOpts]];
    If[ TrueQ[includeMeasurement],
        PrependTo[graphicsList, drawMeasurementWire[positionIndices, styleOpts]]
    ];
    graphicsList
]

