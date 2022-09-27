Package["Wolfram`QuantumFramework`"]

PackageScope["CircuitDraw"]



$DefaultGray = RGBColor[0.537254, 0.537254, 0.537254];

$GateDefaultBoundaryStyle = {
	"H" -> RGBColor[0.368417, 0.506779, 0.709798],
	"T" | "S" -> RGBColor[0.922526, 0.385626, 0.209179],
	"X" | "Y" | "Z" -> RGBColor[0.880722, 0.611041, 0.142051],
	"P"[_] | (Superscript | Power)["P"[_], _] | "PhaseShift"[_] | _Integer -> RGBColor[0.560181, 0.691569, 0.194885],
	Subscript["R", _][_] -> RGBColor[0.528488, 0.470624, 0.701351],
	"Measurement" -> RGBColor[0.7367, 0.358, 0.5030],
	_ -> $DefaultGray
};

$GateDefaultBackgroundStyle = MapAt[Directive[#, Opacity[0.15]] &, Append[Most[$GateDefaultBoundaryStyle], _ -> $DefaultGray], {All, 2}]

$DefaultFontStyleOptions = {FontFamily -> "Roboto", FontSize -> 11, FontColor -> Black};

$DefaultWireLabelStyle = {FontSlant -> Italic, FontSize -> 10, FontColor -> $DefaultGray};

positionCorners[{vpos_, hpos_}, size_, gapSize_] := Thread[{MinMax[hpos] + size / 2 {-1, 1}, MinMax[-vpos gapSize] + size / 2 {-1, 1}}]

simpleLabel[label_] := Replace[label,
	{
		Subscript["R", _][_] -> "Rotate",
		_Integer | "PhaseShift"[_] -> "PhaseShift"
	}
]

Options[drawGate] := DeleteDuplicatesBy[First] @ Join[{
	"RotateLabel" -> Automatic,
	"Size" -> .75,
	"GapSize" -> 1,
	"GateBackgroundStyle" -> $GateDefaultBackgroundStyle,
	"GateBoundaryStyle" -> $GateDefaultBoundaryStyle,
	"GateShapeFunction" -> Automatic
},
	Options[Style], Options[Rectangle]
];
drawGate[pos : {vpos_, hpos_}, label_, opts : OptionsPattern[]] := Block[{
	size = OptionValue["Size"],
	gapSize = OptionValue["GapSize"],
	rotateLabel = Replace[OptionValue["RotateLabel"], Automatic -> If[Length[vpos] > 1, Pi / 2, 0]],
	labelStyleOpts = {FilterRules[{opts}, Options[Style]], $DefaultFontStyleOptions},
	gateBackgroundStyle = Flatten[{OptionValue["GateBackgroundStyle"]}],
	gateBoundaryStyle = Flatten[{OptionValue["GateBoundaryStyle"]}],
	gateShapeFunction = Replace[label, Flatten[{Replace[OptionValue["GateShapeFunction"], Automatic -> Nothing], _ -> None}]],
	corners,
	center,
	vposIndex = PositionIndex[Developer`ToList[vpos]]
},
	corners = positionCorners[pos, size, gapSize];
	center = Mean[corners];
	Replace[label, {
		_ /; gateShapeFunction =!= None -> gateShapeFunction[center, label, size, vpos],
		"Controlled"[subLabel_, control1_, control0_] :> With[{target = Complement[vpos, control1, control0]},
			{
				$DefaultGray,
				Line[{
					{center[[1]], - #[[1]] - size If[MemberQ[target, #[[1]]], Switch[subLabel, "NOT", 1 / 5, "SWAP", 0, _, 1 / 2], 1 / 5]},
					{center[[1]], - #[[2]] + size If[MemberQ[target, #[[2]]], Switch[subLabel, "NOT", 1 / 5, "SWAP", 0, _, 1 / 2], 1 / 5]}
				}] & /@ Select[Partition[Sort[vpos], 2, 1], Not @* ContainsExactly[target]],
				If[ Length[target] > 1,
					If[	MatchQ[subLabel, "SWAP"],
						drawGate[{target, hpos}, subLabel],
						MapIndexed[drawGate[{{#1}, hpos}, Labeled[subLabel, First[#2]], opts] &, target]
					],
					Map[drawGate[{{#}, hpos}, subLabel, opts] &, target]
				],
				drawGate[{{#}, hpos}, "1", opts] & /@ control1,
				drawGate[{{#}, hpos}, "0", opts] & /@ control0
			}
		],
		"1" -> {
			$DefaultGray,
			Line[{center - {size / 2, 0}, center - {size / 5, 0}}],
			Line[{center + {size / 5, 0}, center + {size / 2, 0}}],
			FaceForm[$DefaultGray], Opacity[0.8],
			Disk[center, size / 5]
		},
		"0" -> {
			$DefaultGray,
			Line[{center - {size / 2, 0}, center - {size / 5, 0}}],
			Line[{center + {size / 5, 0}, center + {size / 2, 0}}],
			EdgeForm[RGBColor[0.749019, 0.749019, 0.749019, .8]], FaceForm[White], Opacity[1],
			Disk[center, size / 5]},
		"NOT" -> {
			$DefaultGray,
			Line[{center - {size / 2, 0}, center - {size / 5, 0}}],
			Line[{center + {size / 5, 0}, center + {size / 2, 0}}],
			EdgeForm[$DefaultGray], FaceForm[RGBColor[0.960784, 0.960784, 0.960784]],
			Disk[center, size / 5],
			RGBColor[0.650980, 0.650980, 0.650980],
			Line[{center + size / 5 {-1, 0}, center + size / 5 {1, 0}}], Line[{center + size / 5 {0, -1}, center + size / 5 {0, 1}}]
		},
		"SWAP" -> With[{bottom = {center[[1]], -vpos[[1]] gapSize}, top = {center[[1]], -vpos[[-1]] gapSize}}, {
			$DefaultGray,
		    Line[{bottom - {size / 2, 0}, bottom + {size / 2, 0}}],
		    Line[{top - {size / 2, 0}, top + {size / 2, 0}}],
		    Line[{bottom, top}],
			$DefaultGray, Opacity[0.8], Thickness[Large],
		    Line[{bottom + size / 4 {-1, -1}, bottom + size / 4 {1, 1}}],
		    Line[{bottom + size / 4 {1, -1}, bottom + size / 4 {-1, 1}}],
			Line[{top + size / 4 {-1, -1}, top + size / 4 {1, 1}}],
		    Line[{top + size / 4 {1, -1}, top + size / 4 {-1, 1}}]
		}],
		"RootSWAP" -> With[{bottom = {center[[1]], -vpos[[1]] gapSize}, top = {center[[1]], -vpos[[-1]] gapSize}}, {
			$DefaultGray,
		    Line[{bottom - {size / 2, 0}, bottom + {size / 2, 0}}],
		    Line[{top - {size / 2, 0}, top + {size / 2, 0}}],
		    Line[{bottom, center + {0, size / 4}}],
			Line[{center - {0, size / 4}, top}],
			$DefaultGray, Opacity[0.8], Thickness[Large],
		    Line[{bottom + size / 4 {-1, -1}, bottom + size / 4 {1, 1}}],
		    Line[{bottom + size / 4 {1, -1}, bottom + size / 4 {-1, 1}}],
			Line[{top + size / 4 {-1, -1}, top + size / 4 {1, 1}}],
		    Line[{top + size / 4 {1, -1}, top + size / 4 {-1, 1}}],
			EdgeForm[Replace[subLabel, gateBoundaryStyle]],
			FaceForm[Replace[subLabel, gateBackgroundStyle]],
			Rectangle[center - size / 4, center + size / 4, FilterRules[{opts}, Options[Rectangle]]],
			Text[Style["1 / 2", labelStyleOpts], center]
		}],
		"PhaseShift"[n_] | n_Integer :> {
			EdgeForm[Replace[label, gateBoundaryStyle]],
			FaceForm[Replace[label, gateBackgroundStyle]],
			Disk[center, size / 2],
			(* Text[Tooltip[Style[n, labelStyleOpts], Superscript["P"[If[TrueQ[Negative[n]], -Pi, Pi]], 2 ^ (1 - Abs[n])]], center] *)
			Text[Style[Superscript[If[TrueQ[Negative[n]], -Pi, Pi], InputForm[2 ^ (1 - Abs[n])]], labelStyleOpts], center]
		},
		SuperDagger[subLabel_] | subLabel_ :> {
			EdgeForm[Replace[subLabel, gateBoundaryStyle]],
			FaceForm[Replace[subLabel, gateBackgroundStyle]],
			Rectangle[Sequence @@ corners, FilterRules[{opts}, Options[Rectangle]]],
			Rotate[Text[Style[label, labelStyleOpts], center], rotateLabel]
		}
	}]
]

Options[drawMeasurement] = Join[{
	"RotateLabel" -> Automatic,
	"Size" -> .75,
	"GapSize" -> 1,
	"Arrowhead" -> 0.005,
	"MeasurementBackgroundStyle" -> Replace["Measurement", $GateDefaultBackgroundStyle],
	"MeasurementBoundaryStyle" -> Replace["Measurement", $GateDefaultBoundaryStyle],
	"MeasurementWirePosition" -> Top
},
	Options[Style],
	Options[Rectangle]
];
drawMeasurement[pos_, width_, opts : OptionsPattern[]] := Block[{
	size = OptionValue["Size"],
	gapSize = OptionValue["GapSize"],
	arrowhead = OptionValue["Arrowhead"],
	corners,
	center
},
	corners = positionCorners[pos, size, gapSize];
	center = Mean[corners];
	{
		EdgeForm[OptionValue["MeasurementBoundaryStyle"]], FaceForm[OptionValue["MeasurementBackgroundStyle"]],
		Rectangle[Sequence @@ corners, FilterRules[{opts}, Options[Rectangle]]],
		(* Arrowheads[arrowhead], Arrow[{center, (center + corners[[2]]) / 2}], Circle[center, size / 3, {0, Pi}], *)
		{
			Thickness[Small],
			Table[Line[{center + 0.25 size {Cos[a], Sin[a]} - {0, size / 4}, center + 0.35 size {Cos[a], Sin[a]} - {0, size / 4}}], {a, Pi Subdivide[.2, .8, 7]}],
			Thickness[Medium],
			With[{a = 0.35 Pi}, Line[{center - {0, size / 4}, center + 0.5 size {Cos[a], Sin[a]} - {0, size / 4}}]]
		},
		$DefaultGray,
		If[ OptionValue["MeasurementWirePosition"] === Top, {
			Line[{{center[[1]] - size / 32, corners[[2, 2]]}, {center[[1]] - size / 32, - size / 4 - size / 32}}],
			Line[{{center[[1]] + size / 32, corners[[2, 2]]}, {center[[1]] + size / 32, - size / 4 - size / 32}}]
		}, {
			Line[{{center[[1]] - size / 32, corners[[1, 2]]}, {center[[1]] - size / 32, - width - size + size / 4 + size / 32}}],
			Line[{{center[[1]] + size / 32, corners[[1, 2]]}, {center[[1]] + size / 32, - width - size + size / 4 + size / 32}}]
		}
		],
        EdgeForm[Directive[$DefaultGray, Opacity[0.3]]], FaceForm[Directive[$DefaultGray, Opacity[0.8]]],
		If[ OptionValue["MeasurementWirePosition"] === Top,
        	Polygon[{center[[1]], - size / 4 - size / 32} + size # / 4 & /@ {{- 1 / 2, 0}, {1 / 2, 0}, {0, 1}}],
			Polygon[{center[[1]], - width - size + size / 4 + size / 32} + size # / 4 & /@ {{- 1 / 2, 0}, {1 / 2, 0}, {0, -1}}]
		]

	}
]
Options[drawWires] = {"Size" -> .75};
drawWires[wires_List, OptionsPattern[]] := With[{size = OptionValue["Size"]},
	{$DefaultGray, Map[Apply[Line[{{#1[[1]] + size / 2, -#1[[2]]}, {#2[[1]] - size / 2, -#2[[2]]}}] &], wires]}
]

Options[drawMeasurementWire] = {"Size" -> .75, "MeasurementWirePosition" -> Top, "MeasurementWireLabel" -> "c"};
drawMeasurementWire[x_, width_, OptionsPattern[]] := With[{
	size = OptionValue["Size"],
	label = OptionValue["MeasurementWireLabel"]
}, With[{
	y = If[OptionValue["MeasurementWirePosition"] === Top, 0, - width - size]
},
	{
		$DefaultGray,
		Line[{{size / 2, y - size / 32}, {x - size / 2, y - size / 32}}],
		Line[{{size / 2, y + size / 32}, {x - size / 2, y + size / 32}}],
		Replace[label, {
			Placed[l_, p_] :> Text[l,
				{Replace[p, {Left -> 3 size / 8, Right -> x - 3 size / 8}], y},
				Replace[p, {Left -> {1, 0}, Right -> {-1, 0}}]
			],
			l_ :> Text[l, {3 size / 8, y}, {1, 0}]
		}]
	}
]]

Options[drawWireLabels] = Join[{"Size" -> .75}, Options[Style]];
drawWireLabels[wireLabels_, width_, height_, pad_, opts : OptionsPattern[]] := Block[{size = OptionValue["Size"], labels},
	labels = Replace[wireLabels, {l : Placed[Automatic, _] :> Table[l, width], Automatic -> Range[width], None -> {}}];
    labels = MapIndexed[{label, index} |-> With[{i = pad + First @ index},
        Map[
            Replace[{
                Placed[l_, p_] :> With[{
					text = Style[Replace[l, Automatic -> i], FilterRules[{opts}, Options[Style]], $DefaultWireLabelStyle],
					ps = Developer`ToList[Replace[p, Automatic -> {Left, Center}]]
				},
                    Text[
                        text,
                        Total @ Replace[If[MemberQ[#, Center], #, Append[#, Center]] & @ ps, {
							Center -> {0, -i},
                            Above -> {0, -i + size / 2},
                            Below -> {0, -i - size / 2},
                            Left -> {3 size / 8, 0},
                            Right -> {height - 3 size / 8, 0}
                        },
						{1}],
						If[MemberQ[ps, Left], {1, 0}, {-1, 0}]
					]
				],
                l_ :> Text[Style[l, FilterRules[{opts}, Options[Style]], $DefaultWireLabelStyle], {3 size / 8, -i}, {1, 0}]
                }
            ],
            Developer`ToList[label]
        ]
    ],
        labels
    ];
    labels
]

Options[drawOutline] = Join[{"GapSize" -> 1, "OutlineStyle" -> Directive[
	EdgeForm[Directive[Dashing[{Tiny, Tiny}], $DefaultGray, Opacity[0.8]]],
	FaceForm[RGBColor[0.898039, 0.898039, 0.898039, .3]]
	]},
	Options[Rectangle]
];
drawOutline[width_, height_, pad_, opts : OptionsPattern[]] := With[{gapSize = OptionValue["GapSize"]},
	{OptionValue["OutlineStyle"], Rectangle[{.5, - pad - gapSize / 2}, {height - .5, - pad - width - gapSize / 2}, FilterRules[{opts}, Options[Rectangle]]]}
]

Options[drawLabel] = Join[{"GapSize" -> 1}, Options[Style]];
drawLabel[label_, height_, pad_, opts : OptionsPattern[]] := With[{gapSize = OptionValue["GapSize"]},
	Text[Style[label, FilterRules[{opts}, Options[Style]], Background -> White], {height / 2, - gapSize (pad + 1 / 2)}]
]

Options[circuitDraw] := DeleteDuplicatesBy[First] @
	Join[{
		"WireLabels" -> Automatic, "MeasurementWireLabel" -> "c", "ShowWires" -> True, "ShowLabel" -> False,
		"ShowMeasurementWire" -> True, "WirePadding" -> True,
		"Outline" -> False,
		"SubcircuitLevel" -> 1,
		"GatePacking" -> None,
		"MeasurementWirePosition" -> Top
	},
	Options[drawGate], Options[drawMeasurement], Options[drawWires], Options[drawWireLabels], Options[drawOutline], Options[Style]];
circuitDraw[circuit_QuantumCircuitOperator, opts : OptionsPattern[]] := Block[{
	numGates = circuit["Gates"],
	width = circuit["Width"],
	order = circuit["InputOrder"],
	level = OptionValue["SubcircuitLevel"],
	span,
	pad,
	height,
	orders = #["InputOrder"] & /@ circuit["Operators"],
	labels = #["Label"] & /@ circuit["Operators"],
	positions,
	gatePositions,
	wires,
	wirePaddingQ = TrueQ[OptionValue["WirePadding"]],
	showMeasurementWireQ = TrueQ[OptionValue["ShowMeasurementWire"]] && circuit["Measurements"] > 0,
	labelCount = 0,
	labelCounter
},
	labelCounter = ReplaceAll[None :> (labelCount++; Subscript["U", labelCount])];
	span = If[showMeasurementWireQ, width, #2 - #1 + 1 & @@ MinMax @ order];
	pad = width - span;
	positions = circuitPositions[circuit, level, OptionValue["GatePacking"] === Automatic];
	wires = circuitWires[circuit];
	If[ !wirePaddingQ,
		wires = DeleteCases[wires, _[_, _, i_ /; ! MemberQ[order, i]]]
	];
	gatePositions = MapThread[With[{gateOrder = #1["InputOrder"]}, {gateOrder, #2[[gateOrder]]}] &, {circuit["Operators"], positions[[All, 2]]}];
	height = Max[positions] + 1;
	wires = Replace[wires, _[from_, to_, i_] :> {{If[from == 0, 0, positions[[from, 2, i]]], i}, {If[to == -1, height, positions[[to, 1, i]] + 1], i}}, {1}];
	{
		If[TrueQ[OptionValue["Outline"]], drawOutline[If[wirePaddingQ, width, span], height, pad, FilterRules[{opts}, Options[drawOutline]]], Nothing],
		If[TrueQ[OptionValue["ShowWires"]], drawWires[wires, FilterRules[{opts}, Options[drawWires]]], Nothing],
		If[showMeasurementWireQ, drawMeasurementWire[height, width, FilterRules[{opts}, Options[drawMeasurementWire]]], Nothing],
		MapThread[
			Which[
				QuantumCircuitOperatorQ[#1],
				If[ level > 0,
					Translate[
						circuitDraw[#1,
							"SubcircuitLevel" -> level - 1,
							"WireLabels" -> None, "Outline" -> True, "ShowLabel" -> True, "ShowMeasurementWire" -> False, "WirePadding" -> False,
							opts
						],
						{Max[#3[[#1["InputOrder"]]]], 0}
					],
					drawGate[#2, labelCounter @ #1["Label"], FilterRules[{opts}, Options[drawGate]]]
				],
				QuantumMeasurementOperatorQ[#1],
				drawMeasurement[#2, width, FilterRules[{opts}, Options[drawMeasurement]]],
				True,
				drawGate[#2, labelCounter @ #1["Label"], FilterRules[{opts}, Options[drawGate]]]
			] &,
			{circuit["Operators"], gatePositions, positions[[All, 1]]}
		],
		drawWireLabels[
			If[wirePaddingQ, Identity, Replace[Automatic -> Range[width - span + 1, width]]] @ OptionValue["WireLabels"],
			If[wirePaddingQ, width, span], height, If[wirePaddingQ, 0, pad],
			FilterRules[{opts}, Options[drawWireLabels]]
		],
		If[TrueQ[OptionValue["ShowLabel"]], drawLabel[circuit["Label"], height, pad, FilterRules[{opts}, Options[drawLabel]]]]
	}
]

circuitPositions[circuit_QuantumCircuitOperator, level_Integer : 1, pack_ : False] := With[{width = circuit["Width"]},
	Rest @ FoldList[
		Block[{
			order = #2["InputOrder"],
			gatePos = #1[[-1]],
			shift
		},
			shift = Which[
				QuantumMeasurementOperatorQ[#2],
				ReplacePart[ConstantArray[0, width], Thread[Range[Max[order]] -> 1]],
				! TrueQ[pack] && MatchQ[#2["Label"], "Controlled"[__] | "SWAP"],
				ReplacePart[ConstantArray[0, width], Thread[Range @@ MinMax[order] -> 1]],
				level > 0 && QuantumCircuitOperatorQ[#2],
				ReplacePart[ConstantArray[0, width], Thread[order -> Max[circuitPositions[#2, level - 1][[-1, 2]]]]],
				True,
				ReplacePart[ConstantArray[0, width], Thread[order -> 1]]
			];
			{
				gatePos = Which[
					QuantumMeasurementOperatorQ[#2],
					SubsetMap[ConstantArray[Max[#], Length[#]] &, gatePos, List /@ Range[Max[order]]],
					! TrueQ[pack] && MatchQ[#2["Label"], "Controlled"[__] | "SWAP"],
					SubsetMap[ConstantArray[Max[#], Length[#]] &, gatePos, List /@ Range @@ MinMax[order]],
					level > 0 && QuantumCircuitOperatorQ[#2],
					SubsetMap[ConstantArray[Max[gatePos[[Span @@ MinMax[order]]]], Length[order]] &, gatePos, List /@ order],
					True,
					SubsetMap[ConstantArray[Max[#], Length[order]] &, gatePos, List /@ order]
				],
				gatePos + shift
			}
		] &,
		{ConstantArray[0, width], ConstantArray[0, width]},
		circuit["Operators"]
	]
]

circuitWires[qc_QuantumCircuitOperator] := Block[{
	width = qc["Width"],
	orders = Select[#["InputOrder"], Positive] & /@ qc["Operators"]
},
	Catenate @ ReplacePart[{-1, _, 2} -> -1] @ FoldPairList[
		{prev, order} |-> Block[{next = prev},
			next[[ order ]] = Max[prev] + 1;
			{DirectedEdge[prev[[#]], next[[#]], #] & /@ order, next}
		],
		Table[0, width],
		Append[orders, Range[width]]
	]
]

Options[CircuitDraw] := Join[Options[circuitDraw], Options[Graphics]];
CircuitDraw[circuit_QuantumCircuitOperator, opts : OptionsPattern[]] := Graphics[
	circuitDraw[
		circuit,
		FilterRules[{opts}, Options[circuitDraw]],
		RoundingRadius -> 0.1
	],
	FilterRules[{opts}, Options[Graphics]]
]

