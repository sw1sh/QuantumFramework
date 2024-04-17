Package["Wolfram`QuantumFramework`"]

PackageScope[BlochPlot]
PackageScope[AmplitudeChart]
PackageScope[ProbabilityChart]



Options[BlochPlot] = Join[{"ShowLabels" -> True, "ShowGreatCircles" -> True, "ShowAxes" -> True, "ShowArrow" -> True}, Options[Graphics3D]]

BlochPlot[{u_, v_, w_}, opts : OptionsPattern[]] := Block[{
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
        Opacity[0.4], Sphere[],Black, Thickness[0.0125], Opacity[1.0],
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
        If[ TrueQ[OptionValue["ShowArrow"]],
            Splice @ {Red, Arrowheads[0.05], Arrow[Tube[{{0, 0, 0}, {u, v, w}}, 0.03], {0, -0.01}]},
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


Options[AmplitudeChart] = Join[{"Sort" -> None, "LabelsAngle" -> 0}, Options[BarChart]];

AmplitudeChart[amplitudes_Association, opts : OptionsPattern[]] := Block[{amps, absArg, legend, chart},
	amps = Replace[OptionValue["Sort"], {False | None :> amplitudes, True | Automatic :> SortBy[amplitudes, Abs], f_ :> SortBy[amplitudes, f]}];
	absArg = MapAt[Mod[#, 2 Pi] &, AbsArg[amps], {All, 2}];
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


Options[ProbabilityChart] = Join[{"LabelsAngle" -> 0}, Options[BarChart]];

ProbabilityChart[probabilities_Association, opts : OptionsPattern[]] :=
    BarChart[probabilities, opts, ChartLabels -> (Rotate[#, OptionValue["LabelsAngle"]] & /@ Keys[probabilities])]


