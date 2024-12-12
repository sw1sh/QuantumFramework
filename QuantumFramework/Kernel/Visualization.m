Package["Wolfram`QuantumFramework`"]

PackageScope[BlochPlot]
PackageScope[AmplitudeChart]
PackageScope[ProbabilityChart]
PackageScope[QuditPieChart]
PackageScope[QuditSectorChart]
PackageScope[QuantumStatePauliTree]


Options[BlochPlot] = Join[{"ShowLabels" -> True, "ShowGreatCircles" -> True, "ShowAxes" -> True, "ShowArrow" -> True, "ShowFlag" -> False}, Options[Graphics3D]]

BlochPlot[{u_, v_, w_, a_ : 0}, opts : OptionsPattern[]] := Block[{
    greatCircles, referenceStates
},
    greatCircles = If[
        TrueQ[OptionValue["ShowGreatCircles"]],
        ParametricPlot3D[
            {{Cos[t], Sin[t], 0}, {0, Cos[t], Sin[t]}, {Cos[t], 0, Sin[t]}},
            {t, 0, 2 Pi},
            PlotStyle -> ConstantArray[{Black, Thin}, 3]
        ],
        Nothing
    ];
    referenceStates = Graphics3D[{
        Opacity[0.4], Sphere[],Black, Thickness[0.005], Opacity[1.0],
        If[ TrueQ[OptionValue["ShowAxes"]],
            Splice @ {Line[{{0, 1, 0}, {0, -1, 0}}], Line[{{0, 0, 1}, {0, 0, -1}}], Line[{{1, 0, 0}, {-1, 0, 0}}]},
            Nothing
        ],
        If[ TrueQ[OptionValue["ShowLabels"]],
            Splice @ {
                Text[Ket[{0}], {0, 0, 1.3}],  Text[Ket[{1}], {0, 0, -1.3}],
                Text[Ket[{"R"}], {0, 1.3, 0}], Text[Ket[{"L"}], {0, -1.3, 0}],
                Text[Ket[{"+"}], {1.3, 0, 0}], Text[Ket[{"-"}], {-1.3, 0, 0}]
            },
            Nothing
        ],
        Which[
            TrueQ[OptionValue["ShowFlag"]],
            Splice @ {ColorData["MidShiftBalancedHue"][Mod[a / (2 Pi) - 1 / 2, 1]], Tube[{{0, 0, 0}, {u, v, w}}, 0.03],
                With[{r = RotationTransform[{{1, 0, 0}, {u, v, w}}][{0, .4, 0}]},
                    GeometricTransformation[Polygon[{{u, v, w}, 0.8 {u, v, w}, 0.8 {u, v, w} + r, {u, v, w} + r}], RotationTransform[a, {u, v, w}]]
                ]
            },
            TrueQ[OptionValue["ShowArrow"]],
            Splice @ {Red, Arrowheads[0.05], Arrow[Tube[{{0, 0, 0}, {u, v, w}}, 0.03], {0, -0.01}]},
            True,
            Nothing
        ]
    }
    ];
    Show[{greatCircles, referenceStates},
        FilterRules[{opts}, Options[Graphics3D]],
        PlotRange -> All,
        ViewPoint -> {1, 1, 1},
        Axes -> False,
        Boxed -> False,
        PlotRange -> {{-1.7, 1.7}, {-1.7, 1.7}, {-1.7, 1.7}}
    ]
]


Options[AmplitudeChart] = Join[{"Sort" -> None, "LabelsAngle" -> 0, "Range" -> {0, 1}}, Options[BarChart]];

AmplitudeChart[amplitudes_Association, opts : OptionsPattern[]] := Block[{amps, absArg, legend, chart},
	amps = Replace[OptionValue["Sort"], {False | None :> amplitudes, True | Automatic :> SortBy[amplitudes, Abs], f_ :> SortBy[amplitudes, f]}];
	absArg = MapAt[Mod[#, 2 Pi] &, AbsArg[amps], {All, 2}];
    absArg = Select[absArg, First /* Between[OptionValue["Range"]]];
	chart = BarChart[
		KeyValueMap[Tooltip[#2[[1]], Grid[{{#1, ""}, {#2, ""}, {"Abs:", #2[[1]]}, {"\!\(\*SuperscriptBox[\(Abs\), \(2\)]\):", #2[[1]] ^ 2}, {"Arg:", #2[[2]]}}, Alignment -> Left]] &, absArg],
		FilterRules[FilterRules[{opts}, Options[BarChart]], Except[ChartLegends]],
		ChartLabels -> (Rotate[#, OptionValue["LabelsAngle"]] & /@ Keys[amps]),
		ChartStyle -> ColorData["MidShiftBalancedHue"] /@ Mod[Values[absArg[[All, 2]]] / (2 Pi) - 1 / 2, 1]
	];
	If[ MatchQ[OptionValue[ChartLegends], Automatic | True],
		legend = Show[
			DensityPlot[Arg[x + I y], {x, y} \[Element] Disk[], ColorFunction -> "MidShiftBalancedHue", Exclusions -> None, PlotPoints -> 50, Background -> None],
			RegionPlot[Region @ RegionDifference[Rectangle[{-1, -1}, {1, 1}], Disk[]], PlotStyle -> Transparent, BoundaryStyle -> None, Background -> None],
			Frame -> True, FrameStyle -> White, FrameTicksStyle -> Black, FrameTicks -> {{{{0, "\[Pi]"}}, {{0, "0"}}}, {{{0, "3\[Pi]/2"}}, {{0, "\[Pi]/2"}}}},
			Background -> None,
			ImageSize -> Tiny
		];
		Legended[chart, legend]
		,
		chart
	]
]


Options[ProbabilityChart] = Join[{"Sort" -> None, "LabelsAngle" -> 0,  "Range" -> {0, 1}}, Options[BarChart]];

ProbabilityChart[probabilities_Association, opts : OptionsPattern[]] := Block[{proba = probabilities},
    proba = Replace[OptionValue["Sort"], {False | None :> proba, True | Automatic :> Sort[proba], f_ :> SortBy[proba, f]}];
    proba = Select[proba, Between[OptionValue["Range"]]];
    BarChart[Values[proba], FilterRules[{opts}, Options[BarChart]], ChartLabels -> (Rotate[#, OptionValue["LabelsAngle"]] & /@ Keys[proba])]
]


QuditPieChart[qs_QuantumState, args___] := With[{proba = Normal @ Reverse @ QuantumPositiveTransform[QuantumPhaseSpaceTransform[qs, args]["StateTensor"]]},
	PieChart[proba, ChartStyle -> {{ColorData[97][1], ColorData[97][2]}, None}]
]

QuditSectorChart[qs_QuantumState, args___] := With[{proba = Normal @ Reverse @ QuantumPositiveTransform[QuantumPhaseSpaceTransform[qs, args]["StateTensor"]]},
	SectorChart[Map[{1, #} &] /@ proba, ChartStyle -> {{ColorData[97][1], ColorData[97][2]}, None}]
]


Options[QuantumStatePauliTree] = Options[Graph]

QuantumStatePauliTree[qs_QuantumState, opts : OptionsPattern[]] := Block[{n = qs["Qudits"], decomp, keys, values, selector, leaves, leaveValues, vertices, tree, weights},
	decomp = Chop /@ Join[AssociationThread[StringJoin /@ Tuples[{"I", "X", "Y", "Z"}, n] -> 0], qs["Operator"]["PauliDecompose"]];
	keys = (FirstPosition[{"I", "X", "Y", "Z"}, #][[1]] & /@ Characters[#]) & /@ Keys[decomp];
	values = Normalize[Values[decomp]];
    selector = ! NumberQ[#] || # != 0 & /@ values;
	leaves = Pick[keys, selector];
	leaveValues = If[AllTrue[#, NumericQ], Rescale[#], #] & @ Pick[values, selector];
	vertices = Union @@ (NestList[Most, #, n] & /@ leaves);
	tree = NestTree[{"I", "X", "Y", "Z"} &, Null, n];
	weights = Fold[
		{ws, key} |-> <|ws, key -> Total @ Lookup[ws, Select[Keys @ ws, key == Take[#, UpTo[Length[key]]] &]]|>,
		<|AssociationThread[VertexList[tree][[All, 2]], 0], Thread[leaves -> leaveValues]|>,
		ReverseSortBy[VertexList[tree][[All, 2]], Length]
	];
	Subgraph[
		Graph[
			UndirectedEdge[#1[[2]], #2[[2]], #2[[1]]] & @@@ EdgeList[tree],
			opts,
			EdgeLabels -> UndirectedEdge[_,_,pauli_] :> Framed[pauli, Background -> White, FrameStyle -> None, FrameMargins -> None],
			EdgeStyle -> UndirectedEdge[root_,_,pauli_] :> Directive[Thickness[0.002 Log[1 + Replace[weights[root], _ ? (Not @* NumericQ) -> 0]]], Switch[pauli, "I", Black, "X", Red, "Y", Green, "Z", Blue]],
			EdgeShapeFunction -> "Line",
			VertexSize -> Thread[leaves -> Scaled[.03]],
			VertexStyle -> Thread[leaves -> ColorData["TemperatureMap"] /@ Replace[leaveValues, _ ? (Not @* NumericQ) -> 1, 1]],
			VertexShapeFunction -> Prepend[_ -> None] @ Thread[leaves -> Automatic],
			GraphLayout -> "BalloonEmbedding"
		],
		vertices
	]
]
