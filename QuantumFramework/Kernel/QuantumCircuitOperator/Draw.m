Package["Wolfram`QuantumFramework`"]

PackageScope["CircuitDraw"]



$DefaultGray = RGBColor[0.537254, 0.537254, 0.537254];

$GateDefaultBoundaryStyle = {
	"H" -> RGBColor[0.368417, 0.506779, 0.709798],
	"T" | "S" -> RGBColor[0.922526, 0.385626, 0.209179],
	"X" | "Y" | "Z" | "NOT" | "0" | "1" -> RGBColor[0.880722, 0.611041, 0.142051],
	"P"[_] | (Superscript | Power)["P"[_], _] | "PhaseShift"[_] | _Integer -> RGBColor[0.560181, 0.691569, 0.194885],
	Subscript["R", _][_] -> RGBColor[0.528488, 0.470624, 0.701351],
	"Measurement" -> RGBColor[0.7367, 0.358, 0.5030],
	"Channel" -> $DefaultGray,
	_ -> $DefaultGray
};

$GateDefaultBackgroundStyle = Append[
	MapAt[Directive[#, Opacity[0.3]] &, Most[$GateDefaultBoundaryStyle], {All, 2}],
	_ -> Directive[RGBColor[0.898039, 0.898039, 0.898039], Opacity[.5]]
]

$DefaultFontStyleOptions = {FontFamily -> "Roboto", FontSize -> 11, FontColor -> Black};

$DefaultWireLabelStyle = {FontSlant -> Italic, FontSize -> 10, FontColor -> $DefaultGray};

positionCorners[{vpos_, hpos_}, size_, vGapSize_, hGapSize_] := Thread[{MinMax[hGapSize hpos] + size / 2 {-1, 1}, MinMax[-vpos vGapSize] + size / 2 {-1, 1}}]

simpleLabel[label_] := Replace[label,
	{
		Subscript["R", _][_] -> "Rotate",
		_Integer | "PhaseShift"[_] -> "PhaseShift"
	}
]

selectNonOverlappingIntervals[l_List] := Fold[If[Length[#1] == 0 || #1[[-1, 2]] < #2[[1]], Append[#1, #2], #1] &, {}, l]

Options[drawGate] := DeleteDuplicatesBy[First] @ Join[{
	"RotateGateLabel" -> Automatic,
	"ShowGateLabels" -> True,
	"GateLabels" -> {},
	"Size" -> .75,
	"VerticalGapSize" -> 1,
	"HorizontalGapSize" -> 1,
	"GateBackgroundStyle" -> Automatic,
	"GateBoundaryStyle" -> Automatic,
	"GateShapeFunction" -> Automatic,
	"ShowConnectors" -> False,
	"WireStyle" -> Automatic,
	"ThickWire" -> False
},
	Options[Style], Options[Rectangle]
];
drawGate[{vpos_, hpos_}, args___] := drawGate[{vpos, vpos, hpos}, args]
drawGate[pos : {vposOut_, vposIn_, hpos_}, label_, opts : OptionsPattern[]] := Block[{
	vpos, vposIndex,
	size = OptionValue["Size"],
	vGapSize = OptionValue["VerticalGapSize"],
	hGapSize = OptionValue["HorizontalGapSize"],
	rotateLabel,
	labelStyleOpts = {Background -> Transparent, FilterRules[{opts}, Options[Style]], $DefaultFontStyleOptions},
	gateBackgroundStyle = DeleteDuplicatesBy[First] @
		Flatten[{Replace[OptionValue["GateBackgroundStyle"], Automatic -> $GateDefaultBackgroundStyle], $GateDefaultBackgroundStyle}],
	gateBoundaryStyle = DeleteDuplicatesBy[First] @
		Flatten[{Replace[OptionValue["GateBoundaryStyle"], Automatic -> $GateDefaultBoundaryStyle], $GateDefaultBoundaryStyle}],
	gateShapeFunction = Replace[label, Flatten[{Replace[OptionValue["GateShapeFunction"], Automatic -> Nothing], _ -> None}]],
	corners,
	center,
	gateLabelsQ = TrueQ[OptionValue["ShowGateLabels"]],
	connectorsQ = TrueQ[OptionValue["ShowConnectors"]],
	thickWireQ = TrueQ[OptionValue["ThickWire"]],
	backgroundStyle,
	boundaryStyle,
	drawControlWires,
	wireStyle = Replace[OptionValue["WireStyle"], Automatic -> Directive[$DefaultGray, Opacity[.3]]]
},
	vpos = Union[vposOut, vposIn];
	vposIndex = PositionIndex[Developer`ToList[vpos]];
	rotateLabel = Replace[OptionValue["RotateGateLabel"], {True | Automatic -> If[Length[vpos] > 1, Pi / 2, 0], False | None -> 0}];
	corners = positionCorners[{vpos, hpos}, size, vGapSize, hGapSize];
	center = Mean[corners];
	backgroundStyle = Replace[label, gateBackgroundStyle];
	boundaryStyle = Replace[label, gateBoundaryStyle];
	drawControlWires = Function[{
		boundaryStyle,
		Line[{
			{center[[1]], - vGapSize #1[[1]] - size Switch[#2[[1]], "NOT", 1 / 5, "SWAP", 0, "1" | "0", 1 / 8, _, 1 / 2]},
			{center[[1]], - vGapSize #1[[2]] + size Switch[#2[[2]], "NOT", 1 / 5, "SWAP", 0, "1" | "0", 1 / 8, _, 1 / 2]}
		}]
	}];
	Replace[label, {
		_ /; gateShapeFunction =!= None -> gateShapeFunction[center, label, hGapSize hpos, - vGapSize vpos],
		Subscript["C", subLabel_][control1_, control0_] :> Block[{
			target = DeleteCases[vpos, Alternatives @@ Join[control1, control0]],
			control = Join[control1, control0],
			index
		},
			index = First /@ PositionIndex[target];
			{
				Replace[subLabel, {
					subSubLabels_CircleTimes :> With[{
						labels = Catenate @ Replace[List @@ subSubLabels, {Superscript[subSubLabel_, CircleTimes[n_Integer]] :> Table[subSubLabel, n], subSubLabel_ :> {subSubLabel}}, {1}]
					},
						{
							With[{boundaryStyles = Replace[labels, gateBoundaryStyle, {1}]},
								If[	Equal @@ boundaryStyles,
									backgroundStyle = Replace[First[labels], gateBackgroundStyle];
									boundaryStyle = First[boundaryStyles];
								]
							];
							Map[drawGate[{{#}, hpos}, labels[[index[#]]], opts] &, target],
							drawControlWires[#, {If[MemberQ[target, #[[1]]], labels[[index[#[[1]]]]], "1"], If[MemberQ[target, #[[2]]], labels[[index[#[[2]]]]], "1"]}] & /@ Partition[Sort[vpos], 2, 1]
						} /; Length[labels] == Length[target]
					],
					Superscript[subSubLabel_, CircleTimes[n_Integer]] /; n == Length[target] :>
						{
							backgroundStyle = Replace[subSubLabel, gateBackgroundStyle];
							boundaryStyle = Replace[subSubLabel, gateBoundaryStyle];
							MapIndexed[drawGate[{{#1}, hpos}, subSubLabel, opts] &, target],
							drawControlWires[#, {If[MemberQ[target, #[[1]]], subSubLabel, "1"], If[MemberQ[target, #[[2]]], subSubLabel, "1"]}] & /@ Partition[Sort[vpos], 2, 1]
						},
					_ :> {
						backgroundStyle = Replace[subLabel, gateBackgroundStyle];
						boundaryStyle = Replace[subLabel, gateBoundaryStyle];
						Which[
							Length[target] == 1,
							{
								drawGate[{target, hpos}, subLabel, opts],
								drawControlWires[#, {
									If[MemberQ[target, #[[1]]], subLabel, "1"],
									If[MemberQ[target, #[[2]]], subLabel, "1"]
								}] & /@ Partition[Sort[vpos], 2, 1]
							},
							MatchQ[subLabel, "SWAP"],
							{
								Map[drawGate[{{#}, hpos}, "*", opts] &, target],
								drawControlWires[#, {
									If[MemberQ[target, #[[1]]], subLabel, "1"],
									If[MemberQ[target, #[[2]]], subLabel, "1"]
								}] & /@ Partition[Sort[vpos], 2, 1]
							},
							True,
							{
								Map[drawGate[{{#}, hpos}, Subscript[subLabel, #], opts] &, target],
								drawControlWires[#, {
									If[MemberQ[target, #[[1]]], Subscript[subLabel, #[[1]]], "1"],
									If[MemberQ[target, #[[2]]], Subscript[subLabel, #[[2]]], "1"]
								}] & /@ Partition[Sort[vpos], 2, 1]
							}
						]
					}
				}],
				drawGate[{{#}, hpos}, "1",
					"GateBoundaryStyle" -> {"1" -> boundaryStyle},
					"GateBackgroundStyle" -> {"1" -> backgroundStyle},
					opts
				] & /@ control1,
				drawGate[{{#}, hpos}, "0",
					"GateBoundaryStyle" -> {"0" -> boundaryStyle},
					"GateBackgroundStyle" -> {"0" -> backgroundStyle},
					opts
				] & /@ control0
			}
		],
		Subscript["R", subSubLabels_CircleTimes][angle_] :> With[{
			labels = Catenate @ Replace[List @@ subSubLabels, {Superscript[subSubLabel_, CircleTimes[n_Integer]] :> Table[subSubLabel, n], subSubLabel_ :> {subSubLabel}}, {1}]
		},
			With[{index = First /@ PositionIndex[vpos]}, {
				Map[drawGate[{{#1}, hpos}, Subscript["R", labels[[index[#1]]]][angle], opts] &, vpos],
				drawControlWires[#, {Subscript["R", labels[[index[#[[1]]]]]][angle], Subscript["R", labels[[index[#[[2]]]]]][angle]}] & /@ Partition[Sort[vpos], 2, 1]
			}] /; Length[labels] == Length[vpos]
		],
		Subscript["R", Superscript[subSubLabel_, CircleTimes[n_Integer]]][angle_] /; n == Length[vpos] :> {
			MapIndexed[drawGate[{{#1}, hpos}, Subscript["R", subSubLabel][angle], opts] &, vpos],
			drawControlWires[#, {Subscript["R", subSubLabel][angle], Subscript["R", subSubLabel][angle]}] & /@ Partition[Sort[vpos], 2, 1]
		},
		Subscript["R", subLabel : Except[_Subscript]][angle_] /; Length[vpos] > 1 :> {
			Map[drawGate[{{#}, hpos}, Subscript["R", Subscript[subLabel, #]][angle], opts] &, vpos],
			drawControlWires[#, {Subscript["R", Subscript[subLabel, #[[1]]]][angle], Subscript["R", Subscript[subLabel, #[[2]]]][angle]}] & /@ Partition[Sort[vpos], 2, 1]
		},
		"I" :> {
			wireStyle,
			MapThread[Line[{{center[[1]] - size / 2, - #1 vGapSize}, {center[[1]] + size / 2, - #2 vGapSize}}] &, {vposIn, vposOut}]
		},
		"Cup" :> {
			wireStyle,
			Circle[{center[[1]] + size / 2, center[[2]]}, {vGapSize / 2, (Max[vpos] - Min[vpos]) vGapSize / 2}, {Pi / 2, 3 Pi / 2}]
		},
		"Cap" :> {
			wireStyle,
			Circle[{center[[1]] - size / 2, center[[2]]}, {vGapSize / 2, (Max[vpos] - Min[vpos]) vGapSize / 2}, {- Pi / 2, Pi / 2}]
		},
		(type : "ZSpider" | "XSpider")[phase_] :> {
			wireStyle,
			FaceForm[Switch[type, "ZSpider", White, "XSpider", LightGray]],
			EdgeForm[wireStyle],
			Disk[center, size / 3],
			If[	phase === 0,
				Nothing,
				Text[Style[phase, labelStyleOpts], center]
			],
			Table[With[{p = {center[[1]] - size / 2, - i vGapSize}}, Line[{center + size / 3 Normalize[p - center], p}]], {i, vposIn}],
			Table[With[{p = {center[[1]] + size / 2, - i vGapSize}}, Line[{center + size / 3 Normalize[p - center], p}]], {i, vposOut}]
		},
		"Discard" :> {
			wireStyle,
			Table[{
				Line[{{center[[1]] - size /2, - i vGapSize}, {center[[1]], - i vGapSize}}],
				Thickness[Large],
				Table[
					Line[{{center[[1]] + (j - 1) size / 6, - i vGapSize + size / j / 2}, {center[[1]] + (j - 1) size / 6, - i vGapSize - size / j / 2}}],
					{j, 3}
				]
			},
				{i, vposIn}
			]
		},
		"1" :> {
			wireStyle,
			Line[{center - {size / 2, 0}, center - {size / 8, 0}}],
			Line[{center + {size / 8, 0}, center + {size / 2, 0}}],
			EdgeForm[Replace["1", gateBoundaryStyle]],
			FaceForm[Replace["1", gateBackgroundStyle]],
			Disk[center, size / 8]
		},
		"0" :> {
			wireStyle,
			Line[{center - {size / 2, 0}, center - {size / 8, 0}}],
			Line[{center + {size / 8, 0}, center + {size / 2, 0}}],
			EdgeForm[Replace["0", gateBoundaryStyle]],
			FaceForm[Transparent],
			Disk[center, size / 8]},
		"NOT" :> {
			wireStyle,
			Line[{center - {size / 2, 0}, center - {size / 5, 0}}],
			Line[{center + {size / 5, 0}, center + {size / 2, 0}}],
			EdgeForm[Replace["NOT", gateBoundaryStyle]],
			FaceForm[Replace["NOT", gateBackgroundStyle]],
			Disk[center, size / 5],
			Replace["NOT", gateBackgroundStyle], Opacity[1],
			Line[{center + size / 5 {-1, 0}, center + size / 5 {1, 0}}], Line[{center + size / 5 {0, -1}, center + size / 5 {0, 1}}]
		},
		"SWAP" :> With[{bottom = {center[[1]], -vpos[[1]] vGapSize}, top = {center[[1]], -vpos[[-1]] vGapSize}}, {
			wireStyle,
		    Line[{bottom, top}],
			$DefaultGray, Opacity[0.8], Thickness[Medium],
			MapThread[drawGate[{{#1}, {#2}}, "*", opts] &, {vpos, hpos}]
		}],
		"*" :> {
			wireStyle,
		    Line[{center - {size / 2, 0}, center + {size / 2, 0}}],
			$DefaultGray, Opacity[0.8], Thickness[Medium],
		    Line[{center + size / 5 {-1, -1} / Sqrt[2], center + size / 5 {1, 1} / Sqrt[2]}],
		    Line[{center + size / 5 {1, -1} / Sqrt[2], center + size / 5 {-1, 1} / Sqrt[2]}]
		},
		"RootSWAP" :> With[{bottom = {center[[1]], -vpos[[1]] vGapSize}, top = {center[[1]], -vpos[[-1]] vGapSize}}, {
			wireStyle,
		    Line[{bottom - {size / 2, 0}, bottom + {size / 2, 0}}],
		    Line[{top - {size / 2, 0}, top + {size / 2, 0}}],
		    Line[{bottom, center + {0, size / 5}}],
			Line[{center - {0, size / 5}, top}],
			$DefaultGray, Opacity[0.8], Thickness[Medium],
		    Line[{bottom + size / 5 {-1, -1} / Sqrt[2], bottom + size / 5 {1, 1} / Sqrt[2]}],
		    Line[{bottom + size / 5 {1, -1} / Sqrt[2], bottom + size / 5 {-1, 1} / Sqrt[2]}],
			Line[{top + size / 5 {-1, -1} / Sqrt[2], top + size / 5 {1, 1} / Sqrt[2]}],
		    Line[{top + size / 5 {1, -1} / Sqrt[2], top + size / 5 {-1, 1} / Sqrt[2]}],
			EdgeForm[Replace[subLabel, gateBoundaryStyle]],
			FaceForm[Replace[subLabel, gateBackgroundStyle]],
			Rectangle[center - size / 5, center + size / 5, Sequence @@ FilterRules[{opts}, Options[Rectangle]]],
			If[gateLabelsQ, Text[Style["1 / 2", labelStyleOpts], center], Nothing]
		}],
		"\[Pi]"[perm__] :> {
			wireStyle,
			MapThread[Line[{{center[[1]] - size / 2, - #1 vGapSize}, {center[[1]] + size / 2, - #2 vGapSize}}] &, {vposIn, vposOut[[{perm}]]}]
		},
		"PhaseShift"[n_] | n_Integer /; Length[vpos] == 1 :> {
			EdgeForm[Replace[label, gateBoundaryStyle]],
			FaceForm[Replace[label, gateBackgroundStyle]],
			Disk[center, size / 2],
			If[connectorsQ, {FaceForm[Directive[$DefaultGray, Opacity[1]]], Disk[#, size / 32] & /@ {{center[[1]] - size / 2, - vGapSize #}, {center[[1]] + size / 2, - vGapSize #}} & /@ vpos}, Nothing],
			If[gateLabelsQ, Rotate[Text[Style[Superscript[If[TrueQ[Negative[n]], -Pi, Pi], InputForm[2 ^ (1 - Abs[n])]], labelStyleOpts], center], rotateLabel], Nothing]
		},
		subLabels_CircleTimes :> With[{
			labels = Catenate @ Replace[List @@ subLabels, {Superscript[subSubLabel_, CircleTimes[n_Integer]] :> Table[subSubLabel, n], subSubLabel_ :> {subSubLabel}}, {1}]
		},
			{
				With[{boundaryStyles = Replace[labels, gateBoundaryStyle, {1}]},
					If[	Equal @@ boundaryStyles,
						boundaryStyle = First[boundaryStyles];
					]
				];
				With[{index = First /@ PositionIndex[vpos]}, {
					Map[drawGate[{{#1}, hpos}, labels[[index[#1]]], opts] &, vpos],
					drawControlWires[#, {labels[[index[#[[1]]]]], labels[[index[#[[1]]]]]}] & /@ Partition[Sort[vpos], 2, 1]
				}]
			} /; Length[labels] == Length[vpos]
		],
		Superscript[subLabel_, CircleTimes[n_Integer]] /; n == Length[vpos] :> {
			boundaryStyle = Replace[subLabel, gateBoundaryStyle];
			If[thickWireQ, boundaryStyle = Directive[boundaryStyle, Thickness[0.015]]];
			MapIndexed[drawGate[{{#1}, hpos}, subLabel, opts] &, vpos],
			drawControlWires[#, {subLabel, subLabel}] & /@ Partition[Sort[vpos], 2, 1]
		},
		subLabel : "GlobalPhase"[subSubLabel_] | (subSubLabel_ /; AnyTrue[hpos, LessThan[0]]) :> {
			EdgeForm[Replace[subLabel, gateBoundaryStyle]],
			FaceForm[Replace[subLabel, gateBackgroundStyle]],
			GeometricTransformation[Rectangle[Sequence @@ corners, Sequence @@ FilterRules[{opts}, Options[Rectangle]]], RotationTransform[Pi / 4, center]],
			If[gateLabelsQ, Rotate[Text[Style[subSubLabel, labelStyleOpts], center], rotateLabel], Nothing]
		},
		SuperDagger[subLabel_] | subLabel_ :> {
			EdgeForm[If[thickWireQ, Directive[Thickness[0.015], #] &, Identity] @ Replace[subLabel, gateBoundaryStyle]],
			FaceForm[Replace[subLabel, gateBackgroundStyle]],
			Rectangle[Sequence @@ corners, Sequence @@ FilterRules[{opts}, Options[Rectangle]]],
			If[connectorsQ, {FaceForm[Directive[$DefaultGray, Opacity[1]]], Disk[#, size / 32] & /@ Join[{center[[1]] + size / 2, - vGapSize #} & /@ vposOut, {center[[1]] - size / 2, - vGapSize #} & /@ vposIn]}, Nothing],
			If[gateLabelsQ, Rotate[Text[Style[Replace[label, OptionValue["GateLabels"]], labelStyleOpts], center], rotateLabel], Nothing]
		}
	}]
]

Options[drawMeasurement] = Join[{
	"Size" -> .75,
	"VerticalGapSize" -> 1,
	"HorizontalGapSize" -> 1,
	"Arrowhead" -> 0.005,
	"MeasurementBackgroundStyle" -> Replace["Measurement", $GateDefaultBackgroundStyle],
	"MeasurementBoundaryStyle" -> Replace["Measurement", $GateDefaultBoundaryStyle],
	"MeasurementWirePosition" -> Top,
	"ShowMeasurementWire" -> True,
	"ShowConnectors" -> False,
	"ShowExtraQudits" -> False,
	"WireStyle" -> Automatic,
	"ThickWire" -> False
},
	Options[Style],
	Options[Rectangle]
];
drawMeasurement[{vpos_, _, hpos_}, max_, opts : OptionsPattern[]] := Block[{
	size = OptionValue["Size"],
	vGapSize = OptionValue["VerticalGapSize"],
	hGapSize = OptionValue["HorizontalGapSize"],
	arrowhead = OptionValue["Arrowhead"],
	showMeasurementWireQ = OptionValue["ShowMeasurementWire"],
	extraQuditsQ = OptionValue["ShowExtraQudits"],
	connectorsQ = TrueQ[OptionValue["ShowConnectors"]],
	thickWireQ = TrueQ[OptionValue["ThickWire"]],
	wireStyle = Replace[OptionValue["WireStyle"], Automatic -> Directive[$DefaultGray, Opacity[.3]]],
	corners,
	center
},
	corners = positionCorners[{Select[vpos, Positive], Table[Max[hpos], 2]}, size, vGapSize, hGapSize];
	center = Mean[corners];
	{
		EdgeForm[If[thickWireQ, Directive[#, Thickness[0.015]] &, Identity] @ OptionValue["MeasurementBoundaryStyle"]],
		FaceForm[OptionValue["MeasurementBackgroundStyle"]],
		Rectangle[Sequence @@ corners, Sequence @@ FilterRules[{opts}, Options[Rectangle]]],

		Thickness[Small],
		Table[Line[{center + 0.25 size {Cos[a], Sin[a]} - {0, size / 4}, center + 0.35 size {Cos[a], Sin[a]} - {0, size / 4}}], {a, Pi Subdivide[.2, .8, 7]}],
		Thickness[Medium],
		With[{a = 0.35 Pi}, Line[{center - {0, size / 4}, center + 0.5 size {Cos[a], Sin[a]} - {0, size / 4}}]],
		If[connectorsQ, {FaceForm[Directive[$DefaultGray, Opacity[1]]], Disk[#, size / 32] & /@ {{center[[1]] - size / 2, - vGapSize #}, {center[[1]] + size / 2, - vGapSize #}} & /@ Select[vpos, Positive]}, Nothing],
		If[	showMeasurementWireQ || extraQuditsQ, {
			$DefaultGray,
			If[ OptionValue["MeasurementWirePosition"] === Top,
				{
					Line[{{center[[1]], corners[[2, 2]]}, {center[[1]], - vGapSize # - size / 4 - size / 32}}],
					If[	! showMeasurementWireQ,
						{
							wireStyle,
							Line[{{center[[1]], - vGapSize #}, {corners[[2, 1]], - vGapSize #}}]
						},
						Nothing
					]
				} & /@ If[extraQuditsQ, Select[vpos, NonPositive], {0}],
				Line[{{center[[1]], corners[[1, 2]]}, {center[[1]], - vGapSize max - vGapSize + size / 4 + size / 32}}]
			],
			EdgeForm[Directive[$DefaultGray, Opacity[0.3]]],
			FaceForm[Directive[$DefaultGray, Opacity[0.8]]],
			If[ OptionValue["MeasurementWirePosition"] === Top || extraQuditsQ,
				Map[
					y |-> Polygon[{center[[1]], - vGapSize y - size / 4 - size / 32} + size # / 4 & /@ {{- 1 / 2, 0}, {1 / 2, 0}, {0, 1}}],
					If[extraQuditsQ, Select[vpos, NonPositive], {0}]
				],
				Polygon[{center[[1]], - vGapSize max - vGapSize + size / 4 + size / 32} + size # / 4 & /@ {{- 1 / 2, 0}, {1 / 2, 0}, {0, -1}}]
			]
		},
			Nothing
		]
	}
]

Options[drawChannel] = Join[{
	"Size" -> .75,
	"VerticalGapSize" -> 1,
	"HorizontalGapSize" -> 1,
	"ChannelBackgroundStyle" -> Replace["Channel", $GateDefaultBackgroundStyle],
	"ChannelBoundaryStyle" -> Directive[Dotted, Replace["Channel", $GateDefaultBoundaryStyle]],
	"ShowConnectors" -> False,
	"ShowGateLabels" -> True,
	"GateLabels" -> {},
	"RotateGateLabel" -> Automatic,
	"ShowExtraQudits" -> False,
	"ThickWire" -> False
},
	Options[Style],
	Options[Rectangle]
];
drawChannel[{vpos_, _, hpos_}, label_, opts : OptionsPattern[]] := Block[{
	size = OptionValue["Size"],
	vGapSize = OptionValue["VerticalGapSize"],
	hGapSize = OptionValue["HorizontalGapSize"],
	connectorsQ = TrueQ[OptionValue["ShowConnectors"]],
	gateLabelsQ = TrueQ[OptionValue["ShowGateLabels"]],
	thickWireQ = TrueQ[OptionValue["ThickWire"]],
	rotateLabel,
	labelStyleOpts = {Background -> Transparent, FilterRules[{opts}, Options[Style]], $DefaultFontStyleOptions},
	extracQuditsQ = TrueQ[OptionValue["ShowExtraQudits"]],
	corners,
	center,
	y
},
	y = If[extracQuditsQ, vpos, Select[vpos, Positive]];
	rotateLabel = Replace[OptionValue["RotateGateLabel"], {True | Automatic -> If[Length[y] > 1, Pi / 2, 0], False | None -> 0}];
	corners = positionCorners[{y, hpos}, size, vGapSize, hGapSize];
	center = Mean[corners];
	{
		EdgeForm[If[thickWireQ, Directive[#, Thickness[0.015]] &, Identity] @ OptionValue["ChannelBoundaryStyle"]],
		FaceForm[OptionValue["ChannelBackgroundStyle"]],
		Rectangle[Sequence @@ corners, Sequence @@ FilterRules[{opts}, Options[Rectangle]]],
		If[connectorsQ, {FaceForm[Directive[$DefaultGray, Opacity[1]]], Disk[#, size / 32] & /@ Join[{center[[1]] + size / 2, - vGapSize #} & /@ y, {center[[1]] - size / 2, - vGapSize #} & /@ Select[y, Positive]]}, Nothing],
		If[gateLabelsQ, Rotate[Text[Style[Replace[label, OptionValue["GateLabels"]], labelStyleOpts], center], rotateLabel], Nothing]
	}
]

Options[drawWires] = {"Size" -> .75,
	"VerticalGapSize" -> 1, "HorizontalGapSize" -> 1,
	"ShortOuterWires" -> True, "ShowWireEndpoints" -> False,
	"WireStyle" -> Automatic
};
drawWires[wires_List, OptionsPattern[]] := With[{
	size = OptionValue["Size"],
	vGapSize = OptionValue["VerticalGapSize"],
	hGapSize = OptionValue["HorizontalGapSize"],
	height = Max[wires[[All, 2, 1]]],
	endpointsQ = TrueQ[OptionValue["ShowWireEndpoints"]]
},
	{
		Replace[OptionValue["WireStyle"], Automatic -> Directive[$DefaultGray, Opacity[.3]]],
		Map[ Apply[With[{
			points = If[TrueQ[OptionValue["ShortOuterWires"]],
					{{hGapSize #1[[1]] + size / 2, - vGapSize #1[[2]]}, {hGapSize #2[[1]] - size / 2, - vGapSize #2[[2]]}},
					Which[
						#1[[1]] == 0,
						{{hGapSize - size / 2, - vGapSize #1[[2]]}, {hGapSize #2[[1]] - size / 2, - vGapSize #2[[2]]}},
						#2[[1]] == height,
						{{hGapSize #1[[1]] + size / 2, - vGapSize #1[[2]]}, {hGapSize (#2[[1]] - 1) + size / 2, - vGapSize #2[[2]]}},
						True,
						{{hGapSize #1[[1]] + size / 2, - vGapSize #1[[2]]}, {hGapSize #2[[1]] - size / 2, - vGapSize #2[[2]]}}
					]
				]},
			{
				Line[points],
				If[endpointsQ, {Opacity[1], PointSize[Scaled[0.003]], Point /@ points} , Nothing]
	 		}
		 ] &],
			wires
		]
	}
]

Options[drawMeasurementWire] = Join[{
	"Size" -> .75,
	"VerticalGapSize" -> 1,
	"HorizontalGapSize" -> 1,
	"MeasurementWirePosition" -> Top,
	"MeasurementWireLabel" -> "c",
	"ShowGateLabels" -> True,
	"WireStyle" -> Automatic
}, Options[Style]];
drawMeasurementWire[x_, max_, opts : OptionsPattern[]] := With[{
	size = OptionValue["Size"],
	vGapSize = OptionValue["VerticalGapSize"],
	hGapSize = OptionValue["HorizontalGapSize"],
	label = OptionValue["MeasurementWireLabel"],
	gateLabelsQ = OptionValue["ShowGateLabels"]
}, With[{
	y = If[OptionValue["MeasurementWirePosition"] === Top, 0, - vGapSize max - vGapSize]
},
	{
		{
			Replace[OptionValue["WireStyle"], Automatic -> Directive[$DefaultGray, Opacity[.3]]],
			Line[{{size / 2, y - size / 32}, {hGapSize x - size / 2, y - size / 32}}],
			Line[{{size / 2, y + size / 32}, {hGapSize x - size / 2, y + size / 32}}]
		},
		If[	gateLabelsQ,
			Replace[label, {
				Placed[l_, p_] :> Text[Style[l, Background -> Transparent, FilterRules[{opts}, Options[Style]], $DefaultWireLabelStyle],
					{Replace[p, {Left -> 3 size / 8, Right -> hGapSize x - 3 size / 8}], y},
					Replace[p, {Left -> {1, 0}, Right -> {-1, 0}}]
				],
				l_ :> Text[Style[l, Background -> Transparent, FilterRules[{opts}, Options[Style]], $DefaultWireLabelStyle], {3 size / 8, y}, {1, 0}]
			}],
			Nothing
		]
	}
]]

Options[drawWireLabels] = Join[{"Size" -> .75, "VerticalGapSize" -> 1, "HorizontalGapSize" -> 1}, Options[Style]];
drawWireLabels[wireLabels_, min_, max_, height_, opts : OptionsPattern[]] := Block[{
	size = OptionValue["Size"],
	vGapSize = OptionValue["VerticalGapSize"],
	hGapSize = OptionValue["HorizontalGapSize"],
	labels
},
	labels = Replace[wireLabels, {
		rules : {_Rule...} | _Association :> ReplacePart[Range[min, max], Cases[Normal[rules], HoldPattern[i_Integer /; min <= i <= max -> _]]],
		l : Placed[Automatic, _] :> Table[l, max - min + 1],
		Automatic :> Range[min, max],
		None -> {}
	}];
    labels = MapIndexed[{label, index} |-> With[{i = vGapSize (min - 1 + First[index])},
        Map[
            Replace[{
                Placed[l_, p_] :> With[{
					text = Style[Replace[l, Automatic -> i], Background -> Transparent, FilterRules[{opts}, Options[Style]], $DefaultWireLabelStyle],
					ps = Developer`ToList[Replace[p, Automatic -> {Left, Center}]]
				},
                    Text[
                        text,
                        Total @ Replace[If[MemberQ[#, Left | Right], #, Append[#, Left]] & @
								If[MemberQ[#, Center], #, Append[#, Center]] & @ ps, {
							Center -> {0, -i},
                            Above -> {0, size / 8},
                            Below -> {0, - size / 8},
                            Left -> {3 size / 8, 0},
                            Right -> {hGapSize height - 3 size / 8, 0}
                        },
						{1}],
						If[MemberQ[ps, Right], {-1, 0}, {1, 0}]
					]
				],
                l_ :> Text[Style[l, Background -> Transparent, FilterRules[{opts}, Options[Style]], $DefaultWireLabelStyle], {3 size / 8, -i}, {1, 0}]
                }
            ],
            Developer`ToList[label]
        ]
    ],
        labels
    ];
    labels
]

Options[drawOutline] = Join[{"Size" -> .75, "VerticalGapSize" -> 1, "HorizontalGapSize" -> 1, "OutlineStyle" -> Directive[
	EdgeForm[Directive[Dashing[{Tiny, Tiny}], $DefaultGray, Opacity[0.8]]],
	FaceForm[RGBColor[0.898039, 0.898039, 0.898039, .3]]
	]},
	Options[Rectangle]
];
drawOutline[min_, max_, height_, opts : OptionsPattern[]] := With[{size = OptionValue["Size"], vGapSize = OptionValue["VerticalGapSize"], hGapSize = OptionValue["HorizontalGapSize"]},
	{OptionValue["OutlineStyle"], Rectangle[{hGapSize - 5 size / 8, vGapSize (- min + 1 / 2) }, {hGapSize (height - 1) + 5 size / 8, - vGapSize max - vGapSize / 2}, Sequence @@ FilterRules[{opts}, Options[Rectangle]]]}
]

Options[drawLabel] = Join[{"VerticalGapSize" -> 1, "HorizontalGapSize" -> 1}, Options[Style]];
drawLabel[label_, height_, pos_, opts : OptionsPattern[]] := With[{vGapSize = OptionValue["VerticalGapSize"], hGapSize = OptionValue["HorizontalGapSize"]},
	Text[Style[label, Background -> Transparent, FilterRules[{opts}, Options[Style]], FontFamily -> "Times"], {hGapSize height / 2, - vGapSize (pos - 1 / 2)}]
]

Options[drawBarrier] = {"Size" -> .75, "VerticalGapSize" -> 1, "HorizontalGapSize" -> 1, "BarrierStyle" -> Automatic, "ShowExtraQudits" -> False}
drawBarrier[{vposOut_, vposIn_, hpos_}, OptionsPattern[]] := Block[{
	vpos = Union[vposOut, vposIn],
	size = OptionValue["Size"], vGapSize = OptionValue["VerticalGapSize"], hGapSize = OptionValue["HorizontalGapSize"],
	extraQuditsQ = TrueQ[OptionValue["ShowExtraQudits"]],
	x, y
},
	x = hGapSize First[hpos];
	y = If[extraQuditsQ, vpos, Select[vpos, Positive]];
	{
		Replace[OptionValue["BarrierStyle"], Automatic -> Directive[$DefaultGray, Dashed, Opacity[.8], Thickness[Large]]],
		Line[Map[{x, #} &, List @@ Interval @@ (vGapSize {- # + 1 / 2, - # - 1 / 2} & /@ y), {2}]]
	}
]

Options[circuitDraw] := DeleteDuplicatesBy[First] @ Join[
	{
		"WireLabels" -> Automatic, "MeasurementWireLabel" -> "c", "ShowWires" -> True, "ShowLabel" -> False,
		"ShowMeasurementWire" -> True, "ShowEmptyWires" -> True,
		"ShowOutline" -> False,
		"SubcircuitLevel" -> 1,
		"GateOverlap" -> False,
		"MeasurementWirePosition" -> Top,
		"HorizontalGapSize" -> 1,
		"ShowGateLabels" -> True,
		"SubcircuitOptions" -> {},
		"ShowExtraQudits" -> False
	},
	Options[drawGate], Options[drawMeasurement], Options[drawChannel],
	Options[drawWires], Options[drawWireLabels],
	Options[drawOutline], Options[drawBarrier],
	Options[Style]
];
circuitDraw[circuit_QuantumCircuitOperator, opts : OptionsPattern[]] := Block[{
	numGates = circuit["GateCount"],
	order = Union @@ circuit["Order"],
	freeOrder = circuit["FreeOrder"],
	level = Max[OptionValue["SubcircuitLevel"]],
	hGapSize = OptionValue["HorizontalGapSize"],
	height,
	labels = #["Label"] & /@ circuit["Operators"],
	positions,
	gatePositions,
	wires,
	emptyWiresQ = TrueQ[OptionValue["ShowEmptyWires"]],
	showMeasurementWireQ,
	extraQuditsQ = TrueQ[OptionValue["ShowExtraQudits"]] || AnyTrue[Through[Select[circuit["Operators"], Not @* QuantumChannelQ]["Order"]], NonPositive, 3],
	labelCount = 0,
	labelCounter,
	gateLabelsQ = TrueQ[OptionValue["ShowGateLabels"]],
	min = Min[1, circuit["Min"]],
	max = circuit["Max"],
	outlineMin,
	scalarPos = 1
},
	showMeasurementWireQ = TrueQ[OptionValue["ShowMeasurementWire"]] && ! extraQuditsQ && circuit["Measurements"] > 0;
	labelCounter = ReplaceAll[None :> (labelCount++; Subscript["U", labelCount])];
	outlineMin = Which[showMeasurementWireQ, 1, extraQuditsQ, min, emptyWiresQ, 1, True, Min[circuit["Orders"], max]];
	positions = circuitPositions[circuit, level, MatchQ[OptionValue["GateOverlap"], Automatic | True], showMeasurementWireQ || extraQuditsQ];
	height = Max[0, positions] + 1;
	wires = circuitWires[circuit];
	If[ ! emptyWiresQ,
		wires = DeleteCases[wires, _[_, _, pos_ /; MemberQ[freeOrder, pos]]]
	];
	If[ ! extraQuditsQ,
		wires = DeleteCases[wires, _[_, _, pos_ /; pos < 1]]
	];
	gatePositions = MapThread[{#1[[1]], #1[[2]], #2[[Union @@ #1 - min + 1]]} &, {circuit["NormalOrders", True], positions[[All, 2]]}];
	wires = Replace[wires, _[left_, right_, pos_] :> {{If[left == 0, 0, positions[[left, 2, pos - min + 1]]], pos}, {If[right == -1, height, positions[[right, 1, pos - min + 1]] + 1], pos}}, {1}];
	{
		If[TrueQ[OptionValue["ShowOutline"]], drawOutline[outlineMin, max, height, FilterRules[{opts}, Options[drawOutline]]], Nothing],
		If[TrueQ[OptionValue["ShowWires"]], drawWires[wires, FilterRules[{opts}, Options[drawWires]]], Nothing],
		If[showMeasurementWireQ, drawMeasurementWire[height, max, FilterRules[{opts}, Options[drawMeasurementWire]]], Nothing],
		MapThread[
			Which[
				BarrierQ[#1],
				drawBarrier[Append[Table[circuitElementPosition[#1, min, max] + min - 1, 2], #3 + 1], "ShowExtraQudits" -> extraQuditsQ, FilterRules[{opts}, Options[drawBarrier]]],
				QuantumCircuitOperatorQ[#1],
				If[ level > 0,
					Translate[
						circuitDraw[#1,
							OptionValue["SubcircuitOptions"],
							"SubcircuitLevel" -> level - 1,
							"WireLabels" -> None, "ShowOutline" -> True, "ShowLabel" -> True,
							"ShowMeasurementWire" -> False, "ShowEmptyWires" -> False, "ShortOuterWires" -> False,
							"ShowExtraQudits" -> False,
							opts
						],
						{hGapSize Max[#3[[circuitElementPosition[#1, min, max]]]], 0}
					],
					drawGate[#2, labelCounter @ #1["Label"], FilterRules[{opts}, Options[drawGate]]]
				],
				QuantumMeasurementOperatorQ[#1] || QuantumMeasurementQ[#1],
				drawMeasurement[#2, max, "ShowMeasurementWire" -> showMeasurementWireQ, "ShowExtraQudits" -> extraQuditsQ, "ThickWire" -> #["MatrixQ"], FilterRules[{opts}, Options[drawMeasurement]]],
				QuantumChannelQ[#1],
				drawChannel[#2, labelCounter @ #1["Label"], "ShowExtraQudits" -> extraQuditsQ, "ThickWire" -> #["MatrixQ"], FilterRules[{opts}, Options[drawChannel]]],
				True,
				drawGate[If[#2 === {{}, {}, {}}, {{scalarPos}, {scalarPos++}, {- hGapSize / 2}}, #2], labelCounter @ #1["Label"], "ThickWire" -> #["MatrixQ"], FilterRules[{opts}, Options[drawGate]]]
			] &,
			{circuit["FullElements"], gatePositions, positions[[All, 1]]}
		],
		drawWireLabels[
			OptionValue["WireLabels"],
			outlineMin, max, height,
			FilterRules[{opts}, Options[drawWireLabels]]
		],
		If[TrueQ[OptionValue["ShowLabel"]], drawLabel[Replace[circuit["Label"], None -> ""], height, outlineMin, FilterRules[{opts}, Options[drawLabel]]], Nothing]
	}
]

circuitPositions[circuit_QuantumCircuitOperator, level_Integer : 1, defaultOverlapQ : True | False : False, showMeasurementWireQ : True | False : True] := With[{
	min = Min[1, circuit["Min"]],
	max = circuit["Max"],
	width = circuit["Width"]
},
	Rest @ FoldList[
		Block[{
			pos = circuitElementPosition[#2, min, max],
			gatePos = #1[[2]],
			ranges = #1[[3]],
			overlapQ = defaultOverlapQ || MatchQ[#2["Label"], "I" | "\[Pi]"[___] | "Cap" | "Cup"],
			shift,
			overlapShift
		},
			If[pos === {}, Return[#1, Block]];
			overlapShift = Function[x, If[! overlapQ, NestWhile[# + 1 &, 0, ContainsAny[Lookup[ranges, x + #, {}], pos] &], 0]];
			shift = Which[
				BarrierQ[#2],
				ReplacePart[ConstantArray[0, width], Thread[pos -> 1]],
				QuantumMeasurementOperatorQ[#2] && showMeasurementWireQ || QuantumChannelQ[#2],
				ReplacePart[ConstantArray[0, width], Thread[Range[Max[pos]] -> 1]],
				level > 0 && QuantumCircuitOperatorQ[#2],
				ReplacePart[ConstantArray[0, width], Thread[Range @@ MinMax[pos] -> Max[Replace[circuitPositions[#2, level - 1, overlapQ, False], {{___, {_, o_}} :> o, _ -> 0}]]]],
				True,
				ReplacePart[ConstantArray[0, width], Thread[pos -> 1]]
			];
			{
				gatePos = Which[
					BarrierQ[#2],
					SubsetMap[With[{x = Max[#]}, overlapShift[x] + ConstantArray[x, Length[pos]]] &, gatePos, List /@ pos],
					QuantumMeasurementOperatorQ[#2] && showMeasurementWireQ || QuantumChannelQ[#2],
					SubsetMap[ConstantArray[Max[#], Length[#]] &, gatePos, List /@ Range[Max[pos]]],
					True,
					SubsetMap[With[{x = Max[gatePos[[Span @@ MinMax[pos]]]]}, overlapShift[x] + ConstantArray[x, Length[pos]]] &, gatePos, List /@ pos]
				],
				gatePos + shift,
				Merge[{ranges, Max[gatePos[[pos]]] -> Range @@ MinMax[pos]}, Apply[Union]]
			}
		] &,
		{ConstantArray[0, width], ConstantArray[0, width], <|0 -> {}|>},
		circuit["FullElements"]
	][[All, ;; 2]]
]

circuitWires[circuit_QuantumCircuitOperator] := Block[{
	min = Min[1, circuit["Min"]],
	max = circuit["Max"],
	width = circuit["Width"],
	orders
},
	orders = circuit["NormalOrders", True];
	Catenate @ ReplacePart[{-1, _, 2} -> -1] @ FoldPairList[
		{prev, order} |-> Block[{next, skip, input, output},
			{next, skip} = prev;
			{output, input} = order - min + 1;
			next[[ Union[output, input] ]] = Max[next] + skip;
			{DirectedEdge[prev[[1, #]], next[[#]], # + min - 1] & /@ input, {next, If[order === {{}, {}}, skip + 1, 1]}}
		],
		{Table[0, width], 1},
		Append[{{}, Union[circuit["OutputOrder"], circuit["FreeOrder"]]}] @ orders
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
