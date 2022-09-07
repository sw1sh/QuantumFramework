Package["Wolfram`QuantumFramework`"]

PackageScope["drawGateGraphics"]



drawNotGate[coordinates_List, _] := Module[{whiteRadius, whiteCircle, lines},
    whiteRadius = 0.8;
    whiteCircle = Graphics[{EdgeForm[Thickness[Medium]], Circle[coordinates, whiteRadius]}];
    lines = Graphics[{
        Line[{{First[coordinates] - whiteRadius, Last[coordinates]}, {First[coordinates] + whiteRadius, Last[coordinates]}}],
        Line[{{First[coordinates], Last[coordinates] - whiteRadius}, {First[coordinates], Last[coordinates] + whiteRadius}}]}
    ];
    Show[whiteCircle, lines]
]


drawPhaseShiftGate[coordinates_List, name_, opts : OptionsPattern[Style]] := Module[{width, textGraphics, circle, phase = Replace[name, "PhaseShift"[p_] :> p]},
    width = 4;
    textGraphics = Graphics[Text[Tooltip[Sow @ Style[phase, opts], Superscript["P"[If[TrueQ[Negative[phase]], -Pi, Pi]], 2 ^ (1 - Abs[phase])]], coordinates]];
    circle = Graphics[{
        EdgeForm[Black],
        FaceForm[White],
        Disk[{First[coordinates], Last[coordinates]}, width / 2]
    }];
    Show[circle, textGraphics]
]


Options[drawUnaryGate] = Join[{"Width" -> 4, "Height" -> 3}, Options[Style]];
drawUnaryGate[coordinates_List, name_, opts : OptionsPattern[]] := Module[{
    width = OptionValue["Width"], height = OptionValue["Height"], textGraphics, rectangle
},
    textGraphics = Graphics[Text[Sow @ Style[name, FilterRules[{opts}, Options[Style]]], coordinates]];
    rectangle = Graphics[{
        EdgeForm[Black],
        FaceForm[White],
        Rectangle[{First[coordinates] - width / 2, Last[coordinates] - height / 2}, {First[coordinates] + width / 2, Last[coordinates] + height / 2}]
    }];
    Show[rectangle, textGraphics]
]

Options[drawBinaryGate] = Join[{"Width" -> 4, "Height" -> 3}, Options[Style]];
drawBinaryGate[{coordinates1_List, coordinates2_List}, name_, opts : OptionsPattern[]] := Module[{
    width = OptionValue["Width"], height = OptionValue["Height"], averageCoordinates, textGraphics, rectangle
},
    averageCoordinates = (coordinates1 + coordinates2) / 2;
    textGraphics = Graphics[Text[Sow @ Rotate[Style[name, FilterRules[{opts}, Options[Style]]], - Pi / 2], averageCoordinates]];
    rectangle = Graphics[{EdgeForm[Black], FaceForm[White],
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
            Line[{qudit2Coordinates + {1, -1}, qudit2Coordinates + {-1, 1}}]}];
        verticalLine = Graphics[Line[{qudit1Coordinates, qudit2Coordinates}]];
        averageCoordinates = (qudit1Coordinates + qudit2Coordinates) / 2;
        textGraphics = Graphics[Text[Style["1/2", opts], averageCoordinates]];
        rectangle = Graphics[{
            EdgeForm[Black], FaceForm[White],
            Rectangle[
                {First[averageCoordinates] - width / 2, Last[averageCoordinates] - height / 2},
                {First[averageCoordinates] + width / 2, Last[averageCoordinates] + height / 2}
            ]}
        ];
        Show[cross1, cross2, verticalLine, rectangle, textGraphics]
    ]


drawControlGateTop[coordinates_List, controlOrder_List, targetOrder_List, OptionsPattern["Colors" -> {Black}]] := Module[{circleRadius, circles, lines},
    circleRadius = 0.5;
    circles = Graphics[Table[{EdgeForm[Black], FaceForm[OptionValue["Colors"][[i]]], Disk[coordinates[[i]], circleRadius]}, {i, Length[controlOrder]}]];
    lines = Graphics[
        Table[
            Line[{coordinates[[i]] + {0, - circleRadius}, {First[coordinates[[i]]], Last[coordinates[[i]]] + 5 (controlOrder[[i]] - Max[targetOrder])}}],
            {i, Length[controlOrder]}
        ]
    ];
    Show[lines, circles]
] /; (Length[coordinates] > 0 && Length[controlOrder] > 0 && Length[coordinates] == Length[controlOrder])


drawControlGateBottom[coordinates_List, controlOrder_List, targetOrder_List, OptionsPattern["Colors" -> {Black}]] := Module[{circleRadius, circles, lines},
    circleRadius = 0.5;
    circles = Graphics[Table[{EdgeForm[Black], FaceForm[OptionValue["Colors"][[i]]], Disk[coordinates[[i]], circleRadius]}, {i, Length[controlOrder]}]];
    lines = Graphics[
        Table[
            Line[{coordinates[[i]] + {0, circleRadius}, {First[coordinates[[i]]], Last[coordinates[[i]]] - 5 (Min[targetOrder] - controlOrder[[i]])}}],
            {i, Length[controlOrder]}
        ]
    ];
    Show[lines, circles]
] /; (Length[coordinates] > 0 && Length[controlOrder] > 0 && Length[coordinates] == Length[controlOrder])


drawMeasurementGate[coordinates_List, order_List, name_, opts : OptionsPattern[Style]] := Module[{width, height, radius, semiCircle, arrow, frame},
    width = 4;
    height = 5 (Max[order] - Min[order] + 1) - 3;
    radius = - 0.3 + width / 2;
    semiCircle = Graphics[{Circle[coordinates, radius, {0, Pi}]}];
    arrow = Graphics[{Arrowheads[Medium], Arrow[{coordinates, {First[coordinates] + 1, Last[coordinates] + 1.4}}]}];
    frame = Graphics[{
        Text[
            Style[name, FontSize -> OptionValue[FontSize] / 2, opts],
            {First[coordinates], Last[coordinates] - 0.9}
        ],
        EdgeForm[Black], FaceForm[],
        Rectangle[{First[coordinates]-width / 2, Last[coordinates] - 5 / 3}, {First[coordinates] + width / 2, Last[coordinates] + height}]}];
    Show[semiCircle, arrow, frame]
]

drawMeasurement[coordinates_] := Module[{width = 4},
    Graphics[{
        Line[{coordinates + {.125, width / 2 + 0.05}, coordinates {1, 0}  + {.125, - 1}}],
        Line[{coordinates + {-.125, width / 2 + 0.05}, coordinates {1, 0} + {-.125, - 1}}],
        Polygon[coordinates {1, 0} + {0, - 1.4} + # & /@ {{-.75, 0}, {.75, 0}, {0, 1.25}}]
    }
    ]
]


jumpLine[x_, y_, jumps_, dy_ : 1, step_ : 6] := With[{
    points = {
        {0, y},
        Splice[
            Splice[
                MapAt[step # &, {All, 1}] @
                {{# - .5, y}, {# - .45, y}, {# - .125, y - dy}, {# + .125, y - dy}, {# + .45, y}, {# + .5, y}}
            ] & /@ jumps
        ],
        {x, y}
    }
},
    BSplineCurve[points, SplineDegree -> 3]
]


Options[drawWireGraphics] = Join[Options[Style], {
    "WireLabels" -> Automatic
}]

drawWireGraphics[positionIndices_List, jumpWires_List, opts : OptionsPattern[]] := Module[{
    quditCount,
    lines,
    labels
},
    quditCount = Length[positionIndices];
    labels = Replace[OptionValue["WireLabels"], {l : Placed[Automatic, _] :> Table[l, quditCount], Automatic -> Range[quditCount], None -> {}}];
    labels = MapIndexed[{label, index} |-> With[{i = First @ index},
        Map[
            Replace[{
                Placed[l_, p_] :>
                    Text[
                        Style[Replace[l, Automatic -> i], FilterRules[{opts}, Options[Style]]],
                        Replace[p, {
                            Above -> {0, - 5 i + 2},
                            Below -> {0, - 5 i - 2},
                            Automatic | Left -> {-1 , - 5 i},
                            Right -> {6 Max[positionIndices] + 1, - 5 i}
                        }]
                    ],
                l_ :> Text[Style[l, FilterRules[{opts}, Options[Style]]], {-1 , - 5 i}]
                }
            ],
            Replace[label, l : Except[_List] :> {l}]
        ]
    ],
        labels
    ];
    lines = Table[
        {
            jumpLine[6 Max[positionIndices], - 5 i, Select[jumpWires, #[[1]] == i &][[All, 2]] - .375]
            (* Line[{{0, - 5 i}, {6 Max[positionIndices], - 5 i}}] *)
        },
        {i, Range[quditCount]}
    ];
    Graphics @ Join[lines, labels]
]


Options[drawMeasurementWire] = Join[Options[Style], {"MeasurementWireLabel" -> "c"}]

drawMeasurementWire[positionIndices_List, opts : OptionsPattern[]] := Graphics[{
    Replace[
        OptionValue["MeasurementWireLabel"], {
        Placed[label_, pos_] :> Text[
            Style[Replace[label, None -> ""], FilterRules[{opts}, Options[Style]]],
            Replace[pos, {
                Above -> {0, 2},
                Below -> {0, -2},
                Left | Automatic -> {- 1, 0},
                Right -> {6 Max[positionIndices] + 1, 0}
            }]
        ],
        label_ :> Text[
            Style[Replace[label, None -> ""], FilterRules[{opts}, Options[Style]]],
            {- 1, 0}
        ]
    }],
    Thickness[Medium],
    Line[{{0, -.125}, {6 Max[positionIndices], -.125}}],
    Line[{{0, 0.125}, {6 Max[positionIndices], .125}}]
}]


Options[drawGateGraphics] = Join[Options[Style], Options[drawWireGraphics], Options[drawMeasurementWire]]

drawGateGraphics[gates_List, opts : OptionsPattern[]] := Module[{
    orders, targetOrders, graphicsList, index, positionIndices, gatePositionIndices,
    targetQuditsOrder,
    controlQuditsOrder,
    controlQuditsOrderTop, controlQuditsOrderBottom,
    controlQuditsTopCoordinates, controlQuditsBottomCoordinates,
    includeMeasurement = False,
    labels,
    label, colors, bottomColors, topColors,
    styleOpts = Join[FilterRules[{opts}, Options[Style]], {FontSize -> 24, FontFamily -> "Times"}],
    jumpWires = {}
},
    orders = #["InputOrder"] & /@ gates;
    targetOrders = #["TargetOrder"] & /@ gates;

    graphicsList = {};
    index = 1;
    positionIndices = ConstantArray[1, Max[Flatten[orders], 1]];
    labels = First[#[[2]], {}] & @ Reap @ Do[

    With[{pos = Complement[Range[Min[targetOrders[[i]]], Max[targetOrders[[i]]]], targetOrders[[i]]]},
        If[Length[pos] > 0 && !MatchQ[gates[[i]]["Label"], "Controlled"[__] | "SWAP"], jumpWires = Join[jumpWires, Thread[pos -> Max @ positionIndices[[Join[targetOrders[[i]], pos]]]]]]
    ];
    label = Replace[gates[[i]]["Label"], "Controlled"[x_, ___] :> x];
    If[ QuantumMeasurementOperatorQ[gates[[i]]] || QuantumChannelQ[gates[[i]]],
        gatePositionIndices = Table[Max[positionIndices], {j, Min[orders[[i]]], Max[orders[[i]]]}];
        includeMeasurement = True;
        Which[
            QuantumChannelQ[gates[[i]]],
            AppendTo[graphicsList,
                If[ gates[[i]]["TargetArity"] == 1,
                    drawUnaryGate[
                        {-2 + 6 Max[gatePositionIndices], - 5 Max[orders[[i]]]},
                        label,
                        styleOpts,
                        "Height" -> 4
                    ],
                    drawBinaryGate[
                        {{-2 + 6 Max[gatePositionIndices], - 5 Max[orders[[i]]]}, {-2 + 6 Max[gatePositionIndices], - 5 Min[orders[[i]]]}},
                        label,
                        styleOpts,
                        "Height" -> 4
                    ]
                ]
            ],
            gates[[i]]["POVMQ"],
            AppendTo[graphicsList, drawMeasurementGate[{-2 + 6 Max[gatePositionIndices], - 5 Max[orders[[i]]]}, orders[[i]], "POVM", styleOpts]],
            True,
            AppendTo[
                graphicsList,
                drawMeasurementGate[
                    {-2 + 6 Max[gatePositionIndices], - 5 Max[orders[[i]]]},
                    orders[[i]],
                    label /. None | "Computational" | "Computational"[_] -> "M",
                    styleOpts
                ]
            ]
        ];
        AppendTo[graphicsList, drawMeasurement[{-2 + 6 Max[gatePositionIndices], - 5 Min[orders[[i]]]}]];
        positionIndices = ConstantArray[Max[positionIndices] + 1, Length[positionIndices]];
    ];

    If[ QuantumOperatorQ[gates[[i]]] || QuantumCircuitOperatorQ[gates[[i]]],
        gatePositionIndices = Table[positionIndices[[j]], {j, Min[orders[[i]]], Max[orders[[i]]]}];
        If[ MatchQ[gates[[i]]["Label"], "CX" | "CY" | "CZ" | "CNOT" | "CPHASE" | "CSWAP" | "Controlled"[__]],
            If[ MatchQ[gates[[i]]["Label"], "Controlled"[__]],
                controlQuditsOrder = If[Length[gates[[i]]["Label"]] > 1, Join @@ List @@ gates[[i]]["Label"][[2 ;; ]], orders[[i]][[;; 1]]];
                colors = Join[Table[Black, Length[gates[[i]]["Label"][[2]]]], Table[White, Length[gates[[i]]["Label"][[3]]]]];
                targetQuditsOrder = Complement[orders[[i]], controlQuditsOrder],
                targetQuditsOrder = Rest @ orders[[i]];
                controlQuditsOrder = {First @ orders[[i]]};
                colors = {Black}
            ];
            controlQuditsOrderTop = {};
            controlQuditsOrderBottom = {};
            topColors = {};
            bottomColors = {};
            Do[ Which[
                controlQuditsOrder[[j]] > Max[targetQuditsOrder],
                AppendTo[controlQuditsOrderTop, controlQuditsOrder[[j]]];
                AppendTo[topColors, colors[[j]]],
                controlQuditsOrder[[j]] < Min[targetQuditsOrder],
                AppendTo[controlQuditsOrderBottom, controlQuditsOrder[[j]]];
                AppendTo[bottomColors, colors[[j]]]
            ],
                {j, Length[controlQuditsOrder]}
            ];
            controlQuditsTopCoordinates = Table[{-2 + 6 Max[gatePositionIndices], - 5 controlQuditsOrderTop[[j]]}, {j, Length[controlQuditsOrderTop]}];
            controlQuditsBottomCoordinates = Table[{-2 + 6 Max[gatePositionIndices], - 5 controlQuditsOrderBottom[[j]]}, {j, Length[controlQuditsOrderBottom]}];
            If[ Length[controlQuditsOrderTop] > 0,
                AppendTo[graphicsList, drawControlGateBottom[controlQuditsTopCoordinates, controlQuditsOrderTop, targetQuditsOrder, "Colors" -> topColors]]
            ];
            If[ Length[controlQuditsOrderBottom] > 0,
                AppendTo[graphicsList, drawControlGateTop[controlQuditsBottomCoordinates, controlQuditsOrderBottom, targetQuditsOrder, "Colors" -> bottomColors]]
            ],

            targetQuditsOrder = orders[[i]]
        ];
        AppendTo[
            graphicsList,
            Switch[
                label,
                "SWAP",
                drawSwapGate[{-2 + 6 Max[gatePositionIndices], - 5 First[targetQuditsOrder]}, {-2 + 6 Max[gatePositionIndices], - 5 Last[targetQuditsOrder]}, styleOpts],
                "RootSWAP",
                drawRootSwapGate[{-2 + 6 Max[gatePositionIndices], - 5 First[targetQuditsOrder]}, {-2 + 6 Max[gatePositionIndices], - 5 Last[targetQuditsOrder]}, styleOpts],
                "NOT",
                Show[drawNotGate[{-2 + 6 Max[gatePositionIndices], - 5 #}, styleOpts] & /@ targetQuditsOrder],
                "PhaseShift"[_] | _Integer,
                drawPhaseShiftGate[{-2 + 6 Max[gatePositionIndices], - 5 First[targetQuditsOrder]}, label, styleOpts],
                _,
                If[ gates[[i]]["TargetArity"] == 1,
                    drawUnaryGate[
                        {-2 + 6 Max[gatePositionIndices], - 5 First[targetQuditsOrder]},
                        label /. Composition -> SmallCircle /. None | CircleTimes[] :> Subscript["U", index++],
                        styleOpts
                    ],
                    drawBinaryGate[
                        {{-2 + 6 Max[gatePositionIndices], - 5 Max[targetQuditsOrder]}, {-2 + 6 Max[gatePositionIndices], - 5 Min[targetQuditsOrder]}},
                        label /. Composition -> SmallCircle /. None | CircleTimes[] :> Subscript["U", index++],
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
    PrependTo[graphicsList, drawWireGraphics[positionIndices, jumpWires, styleOpts, FilterRules[{opts}, Options[drawWireGraphics]]]];
    If[ TrueQ[includeMeasurement],
        PrependTo[graphicsList, drawMeasurementWire[positionIndices, styleOpts, FilterRules[{opts}, Options[drawMeasurementWire]]]]
    ];
    {labels, positionIndices, graphicsList}
]

