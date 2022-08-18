Package["Wolfram`QuantumFramework`"]

PackageExport["CircuitDraw"]



$GateDefaultBackgroundStyle = <|
	"H" -> LightOrange,
	"T" -> LightPurple,
	"S" -> LightPurple,
	"PhaseShift" -> LightBlue,
	"Measurement" -> LightPink
|>;

$GateDefaultBoundaryStyle = <|
	"H" -> Orange,
	"T" -> Purple,
	"S" -> Purple,
	"PhaseShift" -> Blue,
	"Measurement" -> Pink
|>;

positionCorners[{vpos_, hpos_}, size_, gapSize_] := Thread[{MinMax[hpos] + size / 2 {-1, 1}, MinMax[-vpos gapSize] + size / 2 {-1, 1}}]

Options[drawGate] := DeleteDuplicatesBy[First] @ Join[{
	"RotateLabel" -> Automatic,
	"Size" -> .75,
	"GapSize" -> 1,
	"GateBackgroundStyle" -> $GateDefaultBackgroundStyle,
	"GateBoundaryStyle" -> $GateDefaultBoundaryStyle},
	Options[Style], Options[Rectangle]
];
drawGate[pos : {vpos_, hpos_}, label_, opts : OptionsPattern[]] := Block[{
	size = OptionValue["Size"],
	gapSize = OptionValue["GapSize"],
	rotateLabel = Replace[OptionValue["RotateLabel"], Automatic -> If[Length[vpos] > 1, Pi / 2, 0]],
	labelStyleOpts = FilterRules[{opts}, Options[Style]],
	gateBackgroundStyle = OptionValue["GateBackgroundStyle"],
	gateBoundaryStyle = OptionValue["GateBoundaryStyle"],
	corners,
	center,
	vposIndex = PositionIndex[Developer`ToList[vpos]]
},
	corners = positionCorners[pos, size, gapSize];
	center = Mean[corners];
	Replace[label, {
		"Controlled"[subLabel_, control1_, control0_] :> With[{target = Complement[vpos, control1, control0]},
			{
				Line[{{center[[1]], - Min[vpos]}, {center[[1]], - Max[vpos]}}],
				If[ Length[target] > 1,
					MapIndexed[drawGate[{{#1}, hpos}, Labeled[subLabel, First[#2]], opts] &, target],
					Map[drawGate[{{#}, hpos}, subLabel, opts] &, target]
				],
				drawGate[{{#}, hpos}, "1", opts] & /@ control1,
				drawGate[{{#}, hpos}, "0", opts] & /@ control0
			}
		],
		"1" -> {
			Line[{center - {size / 2, 0}, center + {size / 2, 0}}],
			FaceForm[Black],
			Disk[center, size / 5]
		},
		"0" -> {
			Line[{center - {size / 2, 0}, center + {size / 2, 0}}],
			EdgeForm[Black], FaceForm[White],
			Disk[center, size / 5]},
		"NOT" -> {
			Line[{center - {size / 2, 0}, center + {size / 2, 0}}],
			EdgeForm[Black], FaceForm[White],
			Disk[center, size / 5],
			Line[{center + size / 5 {-1, 0}, center + size / 5 {1, 0}}], Line[{center + size / 5 {0, -1}, center + size / 5 {0, 1}}]
		},
		"SWAP" -> With[{bottom = {center[[1]], -vpos[[1]] gapSize}, top = {center[[1]], -vpos[[-1]] gapSize}}, {
		    Line[{bottom - {size / 2, 0}, bottom + {size / 2, 0}}],
		    Line[{top - {size / 2, 0}, top + {size / 2, 0}}],
		    Line[{bottom + size / 4 {-1, -1}, bottom + size / 4 {1, 1}}],
		    Line[{bottom + size / 4 {1, -1}, bottom + size / 4 {-1, 1}}],
		    Line[{top + size / 4 {-1, -1}, top + size / 4 {1, 1}}],
		    Line[{top + size / 4 {1, -1}, top + size / 4 {-1, 1}}],
		    Line[{bottom, top}]
		}],
		"PhaseShift"[n_] | n_Integer :> {
			EdgeForm[Lookup[gateBoundaryStyle, "PhaseShift", Black]],
			FaceForm[Lookup[gateBackgroundStyle, "PhaseShift", White]],
			Disk[center, size / 2],
			Text[Tooltip[Style[n, labelStyleOpts], "P"[Pi] ^ (2 ^ (1 - n))], center]
		},
		SuperDagger[subLabel_] | Superscript[subLabel_, "\[Dagger]"] | subLabel_ :> {
			EdgeForm[Lookup[gateBoundaryStyle, subLabel, Black]],
			FaceForm[Lookup[gateBackgroundStyle, subLabel, White]],
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
	"MeasurementBackgroundStyle" -> $GateDefaultBackgroundStyle["Measurement"],
	"MeasurementBoundaryStyle" -> $GateDefaultBoundaryStyle["Measurement"]},
	Options[Style],
	Options[Rectangle]
];
drawMeasurement[pos_, opts : OptionsPattern[]] := Block[{
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
		Arrowheads[arrowhead], Arrow[{center, (center + corners[[2]]) / 2}], Circle[center, size / 3, {0, Pi}],

        Line[{{center[[1]] - size / 32, corners[[2, 2]]}, {center[[1]] - size / 32, - size / 4 - size / 32}}],
        Line[{{center[[1]] + size / 32, corners[[2, 2]]}, {center[[1]] + size / 32, - size / 4 - size / 32}}],
        EdgeForm[Black], FaceForm[Black],
        Polygon[{center[[1]],  - size / 4 - size / 32} + size # / 4 & /@ {{- 1 / 2, 0}, {1 / 2, 0}, {0, 1}}]

	}
]

Options[drawWires] = {"Size" -> .75};
drawWires[wires_List, OptionsPattern[]] := With[{size = OptionValue["Size"]},
	Map[Apply[Line[{{#1[[1]] + size / 2, -#1[[2]]}, {#2[[1]] - size / 2, -#2[[2]]}}] &], wires]
]

Options[drawMeasurementWire] = {"Size" -> .75};
drawMeasurementWire[x_, OptionsPattern[]] := With[{size = OptionValue["Size"]},
	{
		Line[{{size / 2, - size / 32}, {x - size / 2, - size / 32}}],
		Line[{{size / 2, size / 32}, {x - size / 2, size / 32}}]
	}
]

Options[drawWireLabels] = Join[{"Size" -> .75}, Options[Style]];
drawWireLabels[wireLabels_, width_, height_, pad_, opts : OptionsPattern[]] := Block[{size = OptionValue["Size"], labels},
	labels = Replace[wireLabels, {l : Placed[Automatic, _] :> Table[l, width], Automatic -> Range[width], None -> {}}];
    labels = MapIndexed[{label, index} |-> With[{i = pad + First @ index},
        Map[
            Replace[{
                Placed[l_, p_] :>
                    Text[
                        Style[Replace[l, Automatic -> i], FilterRules[{opts}, Options[Style]]],
                        Replace[p, {
                            Above -> {0, -i + size / 2},
                            Below -> {0, -i - size / 2},
                            Automatic | Left -> {- size / 2, -i},
                            Right -> {height + size / 2, -i}
                        }]
                    ],
                l_ :> Text[Style[l, FilterRules[{opts}, Options[Style]]], {- size / 2 , -i}]
                }
            ],
            Developer`ToList[label]
        ]
    ],
        labels
    ];
    labels
]


Options[drawOutline] = Join[{"GapSize" -> 1, "OutlineStyle" -> Directive[EdgeForm[Black], FaceForm[Transparent]]}, Options[Rectangle]];
drawOutline[width_, height_, pad_, opts : OptionsPattern[]] := With[{gapSize = OptionValue["GapSize"]},
	{OptionValue["OutlineStyle"], Rectangle[{.5, - pad - gapSize / 2}, {height - .5, - pad - width - gapSize / 2}, FilterRules[{opts}, Options[Rectangle]]]}
]

Options[drawLabel] = Join[{"GapSize" -> 1}, Options[Style]];
drawLabel[label_, height_, pad_, opts : OptionsPattern[]] := With[{gapSize = OptionValue["GapSize"]},
	Text[Style[label, FilterRules[{opts}, Options[Style]], Background -> White], {height / 2, - gapSize (pad + 1 / 2)}]
]

Options[circuitDraw] := DeleteDuplicatesBy[First] @
	Join[{
		"WireLabels" -> Automatic, "ShowWires" -> True, "ShowLabel" -> False,
		"ShowMeasurementWire" -> True, "WirePadding" -> True,
		"Outline" -> False,
		"SubcircuitLevel" -> 1
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
	showMeasurementWireQ = TrueQ[OptionValue["ShowMeasurementWire"]] && circuit["Measurements"] > 0
},
	span = If[showMeasurementWireQ, width, #2 - #1 + 1 & @@ MinMax @ order];
	pad = width - span;
	positions = circuitPositions[circuit, level];
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
		If[showMeasurementWireQ, drawMeasurementWire[height, FilterRules[{opts}, Options[drawMeasurementWire]]], Nothing],
		MapThread[
			Which[
				QuantumCircuitOperatorQ[#1],
				If[ level > 0,
					Translate[
						circuitDraw[#1,
							"SubcircuitLevel" -> level - 1,
							opts,
							"WireLabels" -> None, "Outline" -> True, "ShowLabel" -> True, "ShowMeasurementWire" -> False, "WirePadding" -> False
						],
						{Max[#3[[#1["InputOrder"]]]], 0}
					],
					drawGate[#2, #1["Label"], FilterRules[{opts}, Options[drawGate]]]
				],
				QuantumMeasurementOperatorQ[#1],
				drawMeasurement[#2, FilterRules[{opts}, Options[drawMeasurement]]],
				True,
				drawGate[#2, #1["Label"], FilterRules[{opts}, Options[drawGate]]]
			] &,
			{circuit["Operators"], gatePositions, positions[[All, 1]]}
		],
		drawWireLabels[
			If[wirePaddingQ, Identity, Replace[Automatic -> Range[width - span + 1, width]]] @ OptionValue["WireLabels"],
			If[wirePaddingQ, width, span], height, pad,
			FilterRules[{opts}, Options[drawWireLabels]]
		],
		If[TrueQ[OptionValue["ShowLabel"]], drawLabel[circuit["Label"], height, pad, FilterRules[{opts}, Options[drawLabel]]]]
	}
]

circuitPositions[circuit_QuantumCircuitOperator, level_Integer : 1] := With[{width = circuit["Width"]},
	Rest @ FoldList[
		Block[{
			order = #2["InputOrder"],
			gatePos = #1[[-1]],
			shift
		},
			shift = If[ level > 0 && QuantumCircuitOperatorQ[#2],
				ReplacePart[ConstantArray[0, width], Thread[order -> Max[circuitPositions[#2, level - 1][[-1, 2]]]]],
				ReplacePart[ConstantArray[0, width], Thread[order -> 1]]
			];
			{
				gatePos = Which[
					QuantumMeasurementOperatorQ[#2],
					SubsetMap[ConstantArray[Max[#], Length[#]] &, gatePos, List /@ Range[Max[order]]],
					level > 0 && QuantumCircuitOperatorQ[#2],
					SubsetMap[ConstantArray[Max[gatePos[[Span @@ MinMax[order]]]], Length[order]] &, gatePos, List /@ order],
					True,
					SubsetMap[ConstantArray[Max[#], Length[order]] &, gatePos, List /@ order]
				],
				If[
					QuantumMeasurementOperatorQ[#2],
					gatePos + 1,
					gatePos + shift
				]
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
		RoundingRadius -> 0.2, FontFamily -> "Source Serif Pro", FontSize -> 20
	],
	FilterRules[{opts}, Options[Graphics]]
]

