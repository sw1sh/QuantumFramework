Package["Wolfram`QuantumFramework`"]

PackageScope["CircuitDraw"]



$DefaultGray = RGBColor[0.537254, 0.537254, 0.537254];

$GateDefaultBoundaryStyle = {
	None | Subscript["\[Psi]", _] | _Ket | _Bra -> Hue[0.62, 0.45, 0.87],
	"H" -> RGBColor[0.368417, 0.506779, 0.709798],
	"T" | "S" -> RGBColor[0.922526, 0.385626, 0.209179],
	"I" | "X" | "Y" | "Z" | "Pauli" | "NOT" | "0" | "1" -> RGBColor[0.880722, 0.611041, 0.142051],
	"P"[_] | (Superscript | Power)["P"[_], _] | "PhaseShift"[_] -> RGBColor[0.560181, 0.691569, 0.194885],
	Subscript["R", _][_] -> RGBColor[0.528488, 0.470624, 0.701351],
	"Measurement" | "Measurement"[_] -> RGBColor[0.7367, 0.358, 0.5030],
	"Channel" | "Channel"[_] -> Directive[Dotted, $DefaultGray],
	"ZSpider" | "XSpider" | "Spider" | "Measure" | "Encode" | "Copy" -> Directive[CapForm[None], $DefaultGray, Opacity[.3]],
	_ -> $DefaultGray
};

$GateDefaultBackgroundStyle = Join[
	{
		"ZSpider" -> White, "XSpider" ->LightGray, "Spider" | "Measure" | "Encode" | "Copy" -> Directive[Opacity[.1], $DefaultGray],
		"Channel" -> Directive[$DefaultGray, Opacity[.25]]
	},
	MapAt[Directive[#, Opacity[0.3]] &, Most[$GateDefaultBoundaryStyle], {All, 2}],
	{
		_ -> Directive[RGBColor[0.0313725, 0.027451, 0.027451], Opacity[.25]]
	}
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

defaultWireThickness[0] := Directive[AbsoluteThickness[0], Opacity[0]]
defaultWireThickness[dim_] := Directive[AbsoluteThickness[Log2[dim / 4 + 3 / 2]], Opacity[Min[0.3 Log2[dim], 1]]]

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
	"ShowGlobalPhase" -> True,
	"WireStyle" -> Automatic,
	"ThickWire" -> False,
	"DimensionWires" -> True,
	"IdentityGate" -> False
},
	Options[Style], Options[Rectangle]
];
drawGate[{vpos_, hpos_}, args___] := drawGate[{vpos, vpos, hpos}, args]
drawGate[{vposOut_, vposIn_, hpos_}, dims : {outDims : {___Rule}, inDims : {___Rule}}, label_, opts : OptionsPattern[]] := Block[{
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
	globalPhaseQ = TrueQ[OptionValue["ShowGlobalPhase"]],
	thickWireQ = TrueQ[OptionValue["ThickWire"]],
	identityQ = TrueQ[OptionValue["IdentityGate"]],
	backgroundStyle,
	boundaryStyle,
	drawControlWires,
	wireStyle = Replace[OptionValue["WireStyle"], Automatic -> Directive[CapForm[None], $DefaultGray, Opacity[.3]]],
	wireThickness = If[TrueQ[OptionValue["DimensionWires"]], defaultWireThickness, AbsoluteThickness[1] &],
	roundingRadius = OptionValue[RoundingRadius],
	gateFunction, gate
},
	vpos = Union[vposOut, vposIn];
	vposIndex = PositionIndex[Developer`ToList[vpos]];
	rotateLabel = Replace[OptionValue["RotateGateLabel"], {True | Automatic -> If[Length[vpos] > 1, - Pi / 2, 0], False | None -> 0}];
	corners = positionCorners[{vpos, hpos}, size, vGapSize, hGapSize];
	center = Mean[corners];
	backgroundStyle = Replace[label, gateBackgroundStyle];
	boundaryStyle = Replace[label, gateBoundaryStyle];
	drawControlWires = Function[{
		boundaryStyle,
		With[{shift = Which[vposIn === {}, 1 / 3, vposOut === {}, - 1 / 3, True, 0] size},
			Line[{
				{center[[1]] + shift, - vGapSize #1[[1]] - size Switch[#2[[1]], "NOT", 1 / 5, "SWAP", 0, "1" | "0", 1 / 8, _, 1 / 2]},
				{center[[1]] + shift, - vGapSize #1[[2]] + size Switch[#2[[2]], "NOT", 1 / 5, "SWAP", 0, "1" | "0", 1 / 8, _, 1 / 2]}
			}]
		]
	}];
	gateFunction = Function[Replace[FixedPoint[ReplaceAll[{Interpretation[_, l_] :> l, "Eigen"[l_] :> l}], #], {
		Subscript["C", subLabel_][control1_, control0_] :> Block[{
			target = DeleteCases[vpos, Alternatives @@ Join[control1, control0]],
			control = Join[control1, control0],
			index
		},
			index = First /@ PositionIndex[target];
			{
				Replace[subLabel, {
					subSubLabels : _CircleTimes | _Composition :> With[{
						labels = labelList[subSubLabels]
					},
						{
							With[{boundaryStyles = Replace[labels, gateBoundaryStyle, {1}]},
								If[	Equal @@ boundaryStyles,
									backgroundStyle = Replace[First[labels], gateBackgroundStyle];
									boundaryStyle = First[boundaryStyles];
								]
							];
							Map[drawGate[{{#}, hpos}, dims, labels[[index[#]]], "IdentityGate" -> True, opts] &, target],
							drawControlWires[#, {If[MemberQ[target, #[[1]]], labels[[index[#[[1]]]]], "1"], If[MemberQ[target, #[[2]]], labels[[index[#[[2]]]]], "1"]}] & /@ Partition[Sort[vpos], 2, 1]
						} /; Length[labels] == Length[target]
					],
					Superscript[subSubLabel_, CircleTimes[n_Integer]] /; n == Length[target] :>
						{
							backgroundStyle = Replace[subSubLabel, gateBackgroundStyle];
							boundaryStyle = Replace[subSubLabel, gateBoundaryStyle];
							MapIndexed[drawGate[{{#1}, hpos}, dims, subSubLabel, "IdentityGate" -> True, opts] &, target],
							drawControlWires[#, {If[MemberQ[target, #[[1]]], subSubLabel, "1"], If[MemberQ[target, #[[2]]], subSubLabel, "1"]}] & /@ Partition[Sort[vpos], 2, 1]
						},
					_ :> {
						backgroundStyle = Replace[subLabel, gateBackgroundStyle];
						boundaryStyle = Replace[subLabel, gateBoundaryStyle];
						Which[
							Length[target] == 1,
							{
								drawGate[{target, hpos}, dims, subLabel, "IdentityGate" -> True, opts],
								drawControlWires[#, {
									If[MemberQ[target, #[[1]]], subLabel, "1"],
									If[MemberQ[target, #[[2]]], subLabel, "1"]
								}] & /@ Partition[Sort[vpos], 2, 1]
							},
							MatchQ[subLabel, "SWAP"],
							{
								Map[drawGate[{{#}, hpos}, dims, "*", opts] &, target],
								drawControlWires[#, {
									If[MemberQ[target, #[[1]]], subLabel, "1"],
									If[MemberQ[target, #[[2]]], subLabel, "1"]
								}] & /@ Partition[Sort[vpos], 2, 1]
							},
							True,
							{
								Map[drawGate[{{#}, hpos}, dims, Subscript[subLabel, #], "IdentityGate" -> True, opts] &, target],
								drawControlWires[#, {
									If[MemberQ[target, #[[1]]], Subscript[subLabel, #[[1]]], "1"],
									If[MemberQ[target, #[[2]]], Subscript[subLabel, #[[2]]], "1"]
								}] & /@ Partition[Sort[vpos], 2, 1]
							}
						]
					}
				}],
				drawGate[{{#}, hpos}, dims, "1",
					"GateBoundaryStyle" -> {"1" -> boundaryStyle},
					"GateBackgroundStyle" -> {"1" -> backgroundStyle},
					opts
				] & /@ control1,
				drawGate[{{#}, hpos}, dims, "0",
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
				Map[drawGate[{{#1}, hpos}, dims, Subscript["R", labels[[index[#1]]]][angle], opts] &, vpos],
				drawControlWires[#, {Subscript["R", labels[[index[#[[1]]]]]][angle], Subscript["R", labels[[index[#[[2]]]]]][angle]}] & /@ Partition[Sort[vpos], 2, 1]
			}] /; Length[labels] == Length[vpos]
		],
		Subscript["R", Superscript[subSubLabel_, CircleTimes[n_Integer]]][angle_] /; n == Length[vpos] :> {
			MapIndexed[drawGate[{{#1}, hpos}, dims, Subscript["R", subSubLabel][angle], opts] &, vpos],
			drawControlWires[#, {Subscript["R", subSubLabel][angle], Subscript["R", subSubLabel][angle]}] & /@ Partition[Sort[vpos], 2, 1]
		},
		Subscript["R", subLabel : Except[_Subscript]][angle_] /; Length[vpos] > 1 :> {
			Map[drawGate[{{#}, hpos}, dims, Subscript["R", Subscript[subLabel, #]][angle], opts] &, vpos],
			drawControlWires[#, {Subscript["R", Subscript[subLabel, #[[1]]]][angle], Subscript["R", Subscript[subLabel, #[[2]]]][angle]}] & /@ Partition[Sort[vpos], 2, 1]
		},
		"I" /; Length[vposOut] === Length[vposIn] && ! identityQ :> {
			wireStyle,
			MapThread[{wireThickness[Replace[#1, inDims]], Line[{{center[[1]] - size / 2, - #1 vGapSize}, {center[[1]] + size / 2, - #2 vGapSize}}]} &, {vposIn, vposOut}]
		},
		"Cup" | ("I" /; Length[vposOut] == 2 && vposIn === {}) :> {
			wireStyle,
			wireThickness[Replace[First[vposOut], outDims]],
			Circle[{center[[1]] + size / 2, center[[2]]}, {vGapSize / 2, (Max[vpos] - Min[vpos]) vGapSize / 2}, {Pi / 2, 3 Pi / 2}]
		},
		"Cap" | ("I" /; Length[vposIn] == 2 && vposOut === {}) :> {
			wireStyle,
			wireThickness[Replace[First[vposIn], inDims]],
			Circle[{center[[1]] - size / 2, center[[2]]}, {vGapSize / 2, (Max[vpos] - Min[vpos]) vGapSize / 2}, {- Pi / 2, Pi / 2}]
		},
		"Uncurry" :> {
			wireStyle,
			Map[{wireThickness[Replace[#, outDims]], Line[{{center[[1]] - size / 2, - vposIn[[1]] vGapSize}, {center[[1]] + size / 2, - # vGapSize}}]} &, vposOut]
		},
		"Curry" :> {
			wireStyle,
			Map[{wireThickness[Replace[#, inDims]], Line[{{center[[1]] - size / 2, - # vGapSize}, {center[[1]] + size / 2, - vposOut[[1]] vGapSize}}]} &, vposIn]
		},

		(type : "ZSpider" | "XSpider" | "Spider")[phase_] | (type : "Measure" | "Encode" | "Copy") :> Block[{center = If[AnyTrue[- Complement[Range @@ MinMax[vpos], vpos] vGapSize, # == center[[2]] &], center + {0, size / 2}, center]}, {
			FaceForm[Replace[type, gateBackgroundStyle]],
			EdgeForm[Directive[wireThickness[Max[Values[inDims], Values[outDims]]], Replace[type, gateBoundaryStyle]]],
			Disk[center, size / 5],
			If[	{phase} === {} || phase === 0,
				Nothing,
				Text[Style[phase, labelStyleOpts], center]
			],
			wireStyle,
			Map[With[{p = {center[[1]] - size / 2, - # vGapSize}}, {wireThickness[Replace[#, inDims]], Line[{center + size / 5 Normalize[p - center], p}]}] &, vposIn],
			Map[With[{p = {center[[1]] + size / 2, - # vGapSize}}, {wireThickness[Replace[#, outDims]], Line[{center + size / 5 Normalize[p - center], p}]}] &, vposOut]
		}],
		"WSpider" :> Block[{center = If[AnyTrue[- (Range @@ MinMax[vpos]) vGapSize, # ==center[[2]] &], center + {0, size / 2}, center]}, {
			wireStyle,
			Map[With[{p = {center[[1]] - size / 2, - # vGapSize}}, {wireThickness[Replace[#, inDims]], Line[{center, p}]}] &, vposIn],
			Map[With[{p = {center[[1]] + size / 2, - # vGapSize}}, {wireThickness[Replace[#, outDims]], Line[{center, p}]}] &, vposOut],
			FaceForm[Directive[Opacity[1], Black]],
			EdgeForm[Directive[wireThickness[Max[Values[inDims], Values[outDims]]], wireStyle]],
			If[	Length[vposIn] < Length[vposOut],
				Triangle[{center + size / 4 {1/2, 1}, center + size / 4 {1/2, -1}, center + size / 4 {-1, 0}}],
				Triangle[{center - size / 4 {1/2, 1}, center - size / 4 {1/2, -1}, center - size / 4 {-1, 0}}]
			]
		}],
		"Discard" | "Trace" :> {
			wireStyle,
			Table[{
				wireThickness[Replace[i, inDims]],
				Line[{{center[[1]] - size /2, - i vGapSize}, {center[[1]], - i vGapSize}}],
				Thickness[Large],
				Table[
					Line[{{center[[1]] + ((j - 1) / 6 - 1 / 3) size, - i vGapSize + size / j / 2}, {center[[1]] + ((j - 1) / 6 - 1 / 3) size, - i vGapSize - size / j / 2}}],
					{j, 3}
				]
			},
				{i, vposIn}
			]
		},
		"1" /; vposIn === vposOut :> {
			wireStyle,
			wireThickness[Replace[First[vposIn], inDims]],
			Line[{center - {size / 2, 0}, center - {size / 8, 0}}],
			Line[{center + {size / 8, 0}, center + {size / 2, 0}}],
			EdgeForm[Replace["1", gateBoundaryStyle]],
			FaceForm[Replace["1", gateBackgroundStyle]],
			Disk[center, size / 8]
		},
		"0" /; vposIn === vposOut :> {
			wireStyle,
			wireThickness[Replace[First[vposIn], inDims]],
			Line[{center - {size / 2, 0}, center - {size / 8, 0}}],
			Line[{center + {size / 8, 0}, center + {size / 2, 0}}],
			EdgeForm[Replace["0", gateBoundaryStyle]],
			FaceForm[Transparent],
			Disk[center, size / 8]},
		"NOT" :> {
			wireStyle,
			wireThickness[Replace[First[vposIn], inDims]],
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
			MapThread[drawGate[{{#1}, {#2}}, dims, "*", opts] &, {vpos, hpos}]
		}],
		"*" :> {
			wireStyle,
			wireThickness[Replace[First[vposIn], inDims]],
		    Line[{center - {size / 2, 0}, center + {size / 2, 0}}],
			$DefaultGray, Opacity[0.8], Thickness[Medium],
		    Line[{center + size / 5 {-1, -1} / Sqrt[2], center + size / 5 {1, 1} / Sqrt[2]}],
		    Line[{center + size / 5 {1, -1} / Sqrt[2], center + size / 5 {-1, 1} / Sqrt[2]}]
		},
		"RootSWAP" :> With[{bottom = {center[[1]], -vpos[[1]] vGapSize}, top = {center[[1]], -vpos[[-1]] vGapSize}}, {
			wireStyle,
			wireThickness[Replace[First[vposIn], inDims]],
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
			MapThread[{wireThickness[Replace[#1, inDims]], Line[{{center[[1]] - size / 2, - #1 vGapSize}, {center[[1]] + size / 2, - #2 vGapSize}}]} &, {vposIn, vposOut[[{perm}]]}]
		},
		"Braid" :> {
			wireStyle,
			{
				wireThickness[Replace[vposIn[[1]], inDims]],
				Line[{{center[[1]] - size / 2, - vposIn[[1]] vGapSize}, {center[[1]] + size / 2, - vposOut[[2]] vGapSize}}],
				wireThickness[Replace[vposIn[[2]], inDims]],
				With[{start = {center[[1]] - size / 2, - vposIn[[2]] vGapSize}, finish = {center[[1]] + size / 2, - vposOut[[1]] vGapSize}}, {
					Line[{start, start + (finish - start) 0.45}],
					Line[{start + (finish - start) 0.55, finish}]
				}]
			}
		},
		SuperDagger["Braid"] :> {
			wireStyle,
			{
				wireThickness[Replace[vposIn[[1]], inDims]],
				Line[{{center[[1]] - size / 2, - vposIn[[2]] vGapSize}, {center[[1]] + size / 2, - vposOut[[1]] vGapSize}}],
				wireThickness[Replace[vposIn[[2]], inDims]],
				With[{start = {center[[1]] - size / 2, - vposIn[[1]] vGapSize}, finish = {center[[1]] + size / 2, - vposOut[[2]] vGapSize}}, {
					Line[{start, start + (finish - start) 0.45}],
					Line[{start + (finish - start) 0.55, finish}]
				}]
			}
		},
		l : "PhaseShift"[n_] /; Length[vpos] == 1 :> {
			EdgeForm[Replace[l, gateBoundaryStyle]],
			FaceForm[Replace[l, gateBackgroundStyle]],
			Disk[center, size / 2],
			If[connectorsQ, {FaceForm[Directive[$DefaultGray, Opacity[1]]], Disk[#, size / 32] & /@ {{center[[1]] - size / 2, - vGapSize #}, {center[[1]] + size / 2, - vGapSize #}} & /@ vpos}, Nothing],
			If[gateLabelsQ, Rotate[Text[Style[Row[{If[TrueQ[Negative[n]], "-", ""], InputForm[2 ^ (1 - Abs[n])]}], labelStyleOpts], center], rotateLabel], Nothing]
		},
		"Measurement"[subLabel_] :> {
			Map[drawGate[{{#1}, hpos}, dims, "Measurement", "ShowGateLabels" -> False, opts] &, vpos],
			{
				Thickness[Small],
				Table[Line[{# + 0.25 size {Cos[a], Sin[a]} - {0, size / 4}, # + 0.35 size {Cos[a], Sin[a]} - {0, size / 4}}], {a, Pi Subdivide[.2, .8, 7]}],
				Thickness[Medium],
				With[{a = 0.35 Pi}, Line[{# - {0, size / 4}, # + 0.5 size {Cos[a], Sin[a]} - {0, size / 4}}]]
			} & /@ ({Max[hpos] hGapSize, - vGapSize #} & /@ vpos),
			Replace[subLabel, {
				Automatic | None | False | "I" | Interpretation[_, Automatic | None | False | "I"] :> Nothing,
				Interpretation[Style[_, style___], _] | _ :> If[gateLabelsQ, Text[Style[subLabel, style, labelStyleOpts], {hGapSize hpos[[1]], - vGapSize # - 2 size / 5}] & /@ vpos, Nothing]
			}],
			Dashed, drawControlWires[#, {subLabel, subLabel}] & /@ Partition[Sort[vpos], 2, 1]
		},
		"Channel"[subLabel_] :> {
			Map[drawGate[{{#1}, hpos}, dims, subLabel,
					"GateBackgroundStyle" -> subLabel -> Replace["Channel", gateBackgroundStyle],
					"GateBoundaryStyle" -> subLabel -> Replace["Channel", gateBoundaryStyle],
					opts
				] &,
				vpos
			],
			Dashed, drawControlWires[#, {subLabel, subLabel}] & /@ Partition[Sort[vpos], 2, 1],
			If[gateLabelsQ, Text[Style[subLabel, labelStyleOpts], {hGapSize hpos[[1]], - vGapSize #}] & /@ vpos, Nothing]
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
					Map[drawGate[Which[vposIn === {}, {{#}, {}, hpos}, vposOut === {}, {{}, {#}, hpos}, True, {{#}, hpos}], dims, labels[[index[#]]], "IdentityGate" -> True, opts] &, vpos],
					Dashed, drawControlWires[#, {labels[[index[#[[1]]]]], labels[[index[#[[1]]]]]}] & /@ Partition[Sort[vpos], 2, 1]
				}]
			} /; Length[labels] == Length[vpos]
		],
		Superscript[subLabel_, CircleTimes[n_Integer]] /; n == Length[vpos] :> {
			boundaryStyle = Replace[subLabel, gateBoundaryStyle];
			If[thickWireQ, boundaryStyle = Directive[boundaryStyle, AbsoluteThickness[3]]];
			Map[drawGate[Which[vposIn === {}, {{#}, {}, hpos}, vposOut === {}, {{}, {#}, hpos}, True, {{#}, hpos}], dims, subLabel, "IdentityGate" -> True, opts] &, vpos],
			Dashed, drawControlWires[#, {subLabel, subLabel}] & /@ Partition[Sort[vpos], 2, 1]
		},
		subLabel : "GlobalPhase"[subSubLabel_] | (subSubLabel_ /; AnyTrue[hpos, LessThan[0]]) :> If[globalPhaseQ,
			{
				EdgeForm[Replace[subLabel, gateBoundaryStyle]],
				FaceForm[Replace[subLabel, gateBackgroundStyle]],
				GeometricTransformation[Rectangle[Sequence @@ corners, Sequence @@ FilterRules[{opts}, Options[Rectangle]]], RotationTransform[Pi / 4, center]],
				If[gateLabelsQ, Rotate[Text[Style[subSubLabel, labelStyleOpts], center], rotateLabel], Nothing]
			},
			{}
		],
		(head : (SuperDagger | SuperStar))[subLabel_] :> (gateFunction[subLabel] /. Text[l_, args___] :> Text[head[l], args]),
		subLabel_ :> {
			EdgeForm[If[thickWireQ, Directive[AbsoluteThickness[3], #] &, Identity] @ Replace[subLabel, gateBoundaryStyle]],
			FaceForm[Replace[If[Length[vposIn] == 0 || Length[vposOut] == 0, None, subLabel], gateBackgroundStyle]],
			Which[
				Length[vposIn] == 0,
				Translate[
					RegionDilation[Triangle[{{center[[1]] + size / 2, - vGapSize Min[vposOut] + size / 2 - roundingRadius}, {center[[1]] + size / 2, - vGapSize Max[vposOut] - size / 2 + roundingRadius}, center + {size / 2 - (size - 2 roundingRadius) Sqrt[3] / 2, 0}}], roundingRadius],
					{- roundingRadius, 0}
				],
				Length[vposOut] == 0,
				Translate[
					RegionDilation[Triangle[{{center[[1]] - size / 2, - vGapSize Max[vposIn] - size / 2 + roundingRadius}, {center[[1]] - size / 2, - vGapSize Min[vposIn] + size / 2 - roundingRadius}, center + {- size / 2 + (size - 2 roundingRadius) Sqrt[3] / 2, 0}}], roundingRadius],
					{roundingRadius, 0}
				],
				True,
				Rectangle[Sequence @@ corners, Sequence @@ FilterRules[{opts}, Options[Rectangle]]]
			],
			If[	connectorsQ, {FaceForm[Directive[$DefaultGray, Opacity[1]]], Disk[#, size / 32] & /@ Join[{center[[1]] + size / 2, - vGapSize #} & /@ vposOut, {center[[1]] - size / 2, - vGapSize #} & /@ vposIn]}, Nothing],
			
			If[	gateLabelsQ,
				Rotate[
					Text[
						Style[Replace[subLabel, OptionValue["GateLabels"]], labelStyleOpts],
						Which[Length[vposIn] == 0, center + {size / 2 - (size + roundingRadius) / 2 / Sqrt[3], 0}, Length[vposOut] == 0, center + {- size / 2 + (size + roundingRadius) / 2 / Sqrt[3], 0}, True, center]
					],
					rotateLabel
				],
				Nothing
			]
		}
	}]];
	gate = If[gateShapeFunction =!= None,
		gateShapeFunction[center, label, hGapSize hpos, - vGapSize vpos, gateFunction, gateBackgroundStyle, gateBoundaryStyle],
		gateFunction[label]
	];
	Tooltip[gate, label]
]

Options[drawMeasurement] = Join[{
	"Size" -> .75,
	"VerticalGapSize" -> 1,
	"HorizontalGapSize" -> 1,
	"Arrowhead" -> 0.005,
	"MeasurementWirePosition" -> Top,
	"Label" -> Automatic,
	"GateLabels" -> {},
	"ShowGateLabels" -> True,
	"ShowMeasurementWire" -> True,
	"ShowConnectors" -> False,
	"ShowExtraQudits" -> False,
	"ShowGauge" -> True,
	"WireStyle" -> Automatic,
	"DimensionWires" -> True
},
	Options[drawGate],
	Options[Style],
	Options[Rectangle]
];
drawMeasurement[{vpos_, _, hpos_}, eigenDims_, max_, opts : OptionsPattern[]] := Block[{
	size = OptionValue["Size"],
	vGapSize = OptionValue["VerticalGapSize"],
	hGapSize = OptionValue["HorizontalGapSize"],
	arrowhead = OptionValue["Arrowhead"],
	showMeasurementWireQ = OptionValue["ShowMeasurementWire"],
	extraQuditsQ = OptionValue["ShowExtraQudits"],
	gaugeQ = OptionValue["ShowGauge"],
	label = OptionValue["Label"],
	connectorsQ = TrueQ[OptionValue["ShowConnectors"]],
	gateLabelsQ = TrueQ[OptionValue["ShowGateLabels"]],
	labelStyleOpts = {Background -> Transparent, FilterRules[{opts}, Options[Style]], $DefaultFontStyleOptions},
	wireStyle = Replace[OptionValue["WireStyle"], Automatic -> Directive[CapForm[None], $DefaultGray, Opacity[.3]]],
	wireThickness = If[TrueQ[OptionValue["DimensionWires"]], defaultWireThickness, AbsoluteThickness[1] &],
	order = Select[vpos, Positive],
	height = Max[hpos],
	corners,
	center
},
	corners = positionCorners[{order, Table[height, 2]}, size, vGapSize, hGapSize];
	center = Mean[corners];
	{
		drawGate[{order, order, hpos}, {{}, {}}, Superscript[MapAt[If[gaugeQ, "Measurement", "Channel"], label, Replace[label, {_Interpretation -> 2, _ -> {{}}}]], CircleTimes[Length[order]]], FilterRules[{opts}, Options[drawGate]]],
		If[	connectorsQ, {FaceForm[Directive[$DefaultGray, Opacity[1]]], Disk[#, size / 32] & /@ {{center[[1]] - size / 2, - vGapSize #}, {center[[1]] + size / 2, - vGapSize #}} & /@ Select[vpos, Positive]}, Nothing],
		If[	showMeasurementWireQ || extraQuditsQ, {
			$DefaultGray,
			If[ OptionValue["MeasurementWirePosition"] === Top,
				MapThread[{
					Line[{{center[[1]], corners[[2, 2]]}, {center[[1]], - vGapSize # - size / 4 - size / 32}}],
					If[	! showMeasurementWireQ,
						{
							wireStyle,
							wireThickness[#2],
							Line[{{center[[1]], - vGapSize #}, {corners[[2, 1]], - vGapSize #}}]
						},
						Nothing
					]
				} &, If[extraQuditsQ, {#, PadRight[eigenDims, Length[#], 2]} & @ Select[vpos, NonPositive], {{0}, {2}}]],
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


Options[drawWires] = {"Size" -> .75,
	"VerticalGapSize" -> 1, "HorizontalGapSize" -> 1,
	"LongOuterWires" -> True, "ShowWireEndpoints" -> False,
	"WireStyle" -> Automatic,
	"DimensionWires" -> True,
	"ShowWireDimensions" -> False
};
drawWires[wires_List, height_, OptionsPattern[]] := Block[{
	size = OptionValue["Size"],
	vGapSize = OptionValue["VerticalGapSize"],
	hGapSize = OptionValue["HorizontalGapSize"],
	endpointsQ = TrueQ[OptionValue["ShowWireEndpoints"]],
	wireThickness = If[TrueQ[OptionValue["DimensionWires"]], defaultWireThickness, AbsoluteThickness[1] &],
	wireGroups = Catenate @* DeleteMissing /@ Thread @ Values @ Merge[KeyUnion[GroupBy[Take[#, 2] &] /@ wires], Identity],
	points
},
	points = {
		{#1, #2} -> {#3, If[	TrueQ[OptionValue["LongOuterWires"]],
			{{hGapSize #1[[1]] + size / 2, - vGapSize #1[[2]]}, {hGapSize (#1[[1]] + #2[[1]]) / 2, - vGapSize #2[[2]]}},
			Which[
				#1[[1]] == 0,
				{{hGapSize - size / 2, - vGapSize #1[[2]]}, {hGapSize #2[[1]] - size / 2, - vGapSize #2[[2]]}},
				#2[[1]] == height,
				{{hGapSize #1[[1]] + size / 2, - vGapSize #1[[2]]}, {hGapSize (#2[[1]] - 1) + size / 2, - vGapSize #2[[2]]}},
				True,
				{{hGapSize #1[[1]] + size / 2, - vGapSize #1[[2]]}, {hGapSize (#1[[1]] + #2[[1]]) / 2, - vGapSize #2[[2]]}}
			]
		]} & @@@ wires[[1]],
		{#1, #2} -> {#3, If[ TrueQ[OptionValue["LongOuterWires"]],
			Which[
				#2[[1]] == height || #1[[1]] == 0,
				{{hGapSize #1[[1]] + size / 2, - vGapSize #1[[2]]}, {hGapSize #2[[1]] - size / 2, - vGapSize #2[[2]]}},
				True,
				{{hGapSize (#1[[1]] + #2[[1]]) / 2, - vGapSize #1[[2]]}, {hGapSize #2[[1]] - size / 2, - vGapSize #2[[2]]}}
			],
			Which[
				#1[[1]] == 0,
				{{hGapSize - size / 2, - vGapSize #1[[2]]}, {hGapSize #2[[1]] - size / 2, - vGapSize #2[[2]]}},
				#2[[1]] == height,
				{{hGapSize #1[[1]] + size / 2, - vGapSize #1[[2]]}, {hGapSize (#2[[1]] - 1) + size / 2, - vGapSize #2[[2]]}},
				True,
				{{hGapSize (#1[[1]] + #2[[1]]) / 2, - vGapSize #1[[2]]}, {hGapSize #2[[1]] - size / 2, - vGapSize #2[[2]]}}
			]
		]} & @@@ wires[[2]]
	};
	points = Values @ Merge[
		KeyUnion[points],
		Which[MissingQ[#[[1]]], #[[2]], #[[1, 1]] == #[[2, 1]], {#[[1, 1]], {#[[1, 2, 1]], #[[2, 2, 2]]}}, True, Splice @ Thread[{#[[All, 1]], #[[All, 2]]}]] &
	];
	{
		Replace[OptionValue["WireStyle"], Automatic -> Directive[CapForm[None], $DefaultGray, Opacity[.3]]],
		If[	Subtract @@ #2 != {0, 0},
			{
				wireThickness[#1],
				If[TrueQ[OptionValue["ShowWireDimensions"]], Text[Style[#1, Opacity[1]], Mean[#2], {0, -1}], Nothing],
				Line[#2],
				If[endpointsQ, {Opacity[1], PointSize[Scaled[0.003]], Point /@ #2} , Nothing]
			}
		]
	} & @@@ points
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
		rules : {_Rule...} | _Association :> Replace[Range[min, max], Cases[Normal[rules], HoldPattern[i_Integer /; min <= i <= max -> _]], {1}],
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
	Text[Style[label, Background -> Transparent, FilterRules[{opts}, Options[Style]], FontFamily -> "Times"], {hGapSize height / 2, - vGapSize (pos - 3 / 5)}]
]

Options[drawBarrier] = Join[{"Size" -> .75, "VerticalGapSize" -> 1, "HorizontalGapSize" -> 1, "BarrierStyle" -> Automatic, "ShowExtraQudits" -> False, "Label" -> None}, Options[Style]]
drawBarrier[{vposOut_, vposIn_, hpos_}, opts : OptionsPattern[]] := Block[{
	vpos = Union[vposOut, vposIn],
	size = OptionValue["Size"], vGapSize = OptionValue["VerticalGapSize"], hGapSize = OptionValue["HorizontalGapSize"],
	extraQuditsQ = TrueQ[OptionValue["ShowExtraQudits"]],
	label = Replace[OptionValue["Label"], None -> ""], labelPos = Top,
	x, y
},
	Replace[label, Placed[l_, p_] :> (label = l; labelPos = p)];
	x = hGapSize Max[hpos];
	y = If[extraQuditsQ, vpos, Select[vpos, Positive]];
	{
		Replace[OptionValue["BarrierStyle"], Automatic -> Directive[$DefaultGray, Dashed, Opacity[.8], Thickness[Large]]],
		Line[Map[{x, #} &, List @@ Interval @@ (vGapSize {- # + 1 / 2, - # - 1 / 2} & /@ y), {2}]],
		Text[Style[label, FilterRules[{opts}, Options[Style]]], {x, Replace[labelPos, {Top -> - Min[y] vGapSize + size, Bottom -> - Max[y] vGapSize - size}]}]
	}
]

expandLevel[lvl_] := Replace[lvl, {True -> Infinity, False -> - Infinity, None -> 0}]

expandLevel[lvl1 : _Integer | Infinity | - Infinity, lvl2_] :=
	Switch[
		{lvl1, lvl2},
		{Infinity, _}, expandLevel[lvl2],
		{- Infinity, _}, - Infinity,
		{_, None}, lvl1,
		{None, _}, lvl2,
		{_, False}, - Infinity,
		{_, True}, Infinity,
		_, lvl1 + lvl2
	]

Options[circuitDraw] := DeleteDuplicatesBy[First] @ Join[
	{
		"WireLabels" -> Automatic, "MeasurementWireLabel" -> "c", "ShowWires" -> True, "ShowLabel" -> False,
		"ShowMeasurementWire" -> True, "ShowEmptyWires" -> True,
		"ShowOutline" -> False,
		"Expand" -> 1,
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
circuitDraw[circuit_QuantumCircuitOperator, Dynamic[qc_Symbol], position_List, opts : OptionsPattern[]] := Block[{
	numGates = circuit["GateCount"],
	order = Union @@ circuit["Order"],
	freeOrder = circuit["FreeOrder"],
	inputOrders,
	level = expandLevel[OptionValue["Expand"]],
	hGapSize = OptionValue["HorizontalGapSize"],
	height,
	positions,
	gatePositions,
	outWires, inWires, wires,
	emptyWiresQ = TrueQ[OptionValue["ShowEmptyWires"]],
	dimensionWiresQ = TrueQ[OptionValue["DimensionWires"]],
	showMeasurementWireQ,
	extraQuditsQ,
	labelCount = 0,
	labelCounter,
	gateLabelsQ = TrueQ[OptionValue["ShowGateLabels"]],
	min = Min[1, circuit["Min"]],
	max = circuit["Max"],
	outlineMin,
	scalarPos = 1
},
	inputOrders = Map[If[QuantumChannelQ[#] || QuantumMeasurementOperatorQ[#], #["InputOrder"], Union @@ #["Order"]] &, circuit["Flatten"]["Operators"]];
	extraQuditsQ = TrueQ[OptionValue["ShowExtraQudits"]] || AnyTrue[inputOrders, NonPositive, 2];
	showMeasurementWireQ = TrueQ[OptionValue["ShowMeasurementWire"]] && ! extraQuditsQ && circuit["Measurements"] > 0;
	labelCounter[label_, {out_, in_}] := If[MatchQ[label, ("Measurement" | "Channel")[_] | Subscript["C", ("Measurement" | "Channel")[_]][___]], label, label /. None :> (labelCount++; Subscript[If[out === {} || in === {}, "\[Psi]", "U"], labelCount])];
	outlineMin = Which[showMeasurementWireQ, 1, AnyTrue[order, NonPositive] && extraQuditsQ, min, emptyWiresQ, 1, True, Min[inputOrders, max]];
	positions = circuitPositions[circuit, level, MatchQ[OptionValue["GateOverlap"], Automatic | True], showMeasurementWireQ, extraQuditsQ];
	height = Max[0, positions] + 1;
	{outWires, inWires} = circuitWires[circuit];
	If[ ! emptyWiresQ,
		outWires = DeleteCases[outWires, _[_, _, {pos_, _} /; MemberQ[freeOrder, pos]]];
		inWires = DeleteCases[inWires, _[_, _, {pos_, _} /; MemberQ[freeOrder, pos]]];
	];
	If[ ! extraQuditsQ,
		outWires = DeleteCases[outWires, _[_, _, {pos_, _} /; pos < 1]];
		inWires = DeleteCases[inWires, _[_, _, {pos_, _} /; pos < 1]]
	];
	gatePositions = MapThread[{#1[[1]], #1[[2]], #2[[Union @@ #1 - min + 1]]} &, {circuit["NormalOrders", True], positions[[All, 2]]}];
	wires = Replace[{outWires, inWires}, _[left_, right_, {pos_, dim_}] :> {{If[left == 0, 0, positions[[left, 2, pos - min + 1]]], pos}, {If[right == -1, height, positions[[right, 1, pos - min + 1]] + 1], pos}, dim}, {2}];
	{
		If[TrueQ[OptionValue["ShowOutline"]], drawOutline[outlineMin, max, height, FilterRules[{opts}, Options[drawOutline]]], Nothing],
		If[TrueQ[OptionValue["ShowWires"]], drawWires[wires, height, FilterRules[{opts}, Options[drawWires]]], Nothing],
		If[showMeasurementWireQ, drawMeasurementWire[height, max, FilterRules[{opts}, Options[drawMeasurementWire]]], Nothing],
		MapThread[
			Which[
				BarrierQ[#1],
				drawBarrier[Append[Table[circuitElementPosition[#1, min, max] + min - 1, 2], #3 + 1], "ShowExtraQudits" -> extraQuditsQ, "Label" -> Replace[#1, {"Barrier"[_, label_, ___] :> label, _ -> None}], FilterRules[{opts}, Options[drawBarrier]]],
				QuantumCircuitOperatorQ[#1],
				EventHandler[
					If[ expandLevel[level, #1["Expand"]] > 0,
						Translate[
							circuitDraw[#1, Dynamic[qc], Append[position, #4],
								#1["DiagramOptions"],
								OptionValue["SubcircuitOptions"],
								"Expand" -> level - 1,
								"WireLabels" -> None, "ShowOutline" -> True, "ShowLabel" -> True,
								"ShowMeasurementWire" -> False, "ShowEmptyWires" -> False, "LongOuterWires" -> False,
								"ShowExtraQudits" -> False, "ShowGlobalPhase" -> False,
								opts
							],
							{hGapSize Max[#3[[circuitElementPosition[#1, min, max]]]], 0}
						],
						drawGate[#2, {Thread[#1["OutputOrder"] -> #1["OutputDimensions"]], Thread[#1["InputOrder"] -> #1["InputDimensions"]]}, labelCounter[#1["Label"], #1["Order"]], FilterRules[{opts}, Options[drawGate]]]
					],
					{"MouseClicked" :> (qc = qc["ToggleExpand", Append[position, #4]])},
					PassEventsDown -> True,
					PassEventsUp -> False
				],
				QuantumMeasurementOperatorQ[#1],
				drawMeasurement[#2, #1["Eigendimensions"], max, "ShowMeasurementWire" -> showMeasurementWireQ, "ShowExtraQudits" -> extraQuditsQ, "ThickWire" -> #["MatrixQ"], "Label" -> #1["Label"], FilterRules[{opts}, Options[drawMeasurement]]],
				QuantumChannelQ[#1],
				drawMeasurement[#2, #1["TraceDimensions"], max, "ShowMeasurementWire" -> False,
					"ShowExtraQudits" -> extraQuditsQ, "ThickWire" -> #["MatrixQ"], "Label" -> #1["Label"], "ShowGauge" -> False,
					FilterRules[{opts}, Options[drawMeasurement]]
				],
				True,
				drawGate[
					If[#2 === {{}, {}, {}}, {{scalarPos}, {scalarPos++}, {- hGapSize / 2}}, #2],
					{Thread[#1["OutputOrder"] -> #1["OutputDimensions"]], Thread[#1["InputOrder"] -> #1["InputDimensions"]]},
					labelCounter[#1["Label"], #1["Order"]],
					"ThickWire" -> #["MatrixQ"],
					FilterRules[{opts}, Options[drawGate]]
				]
			] &,
			{circuit["NormalOperators", True], gatePositions, positions[[All, 1]], Range[Length[gatePositions]]}
		],
		drawWireLabels[
			OptionValue["WireLabels"],
			outlineMin, max, height,
			FilterRules[{opts}, Options[drawWireLabels]]
		],
		If[TrueQ[OptionValue["ShowLabel"]], drawLabel[Replace[circuit["Label"], None -> ""], height, outlineMin, FilterRules[{opts}, Options[drawLabel]]], Nothing]
	}
]



circuitElementPosition["Barrier", from_, to_, ___] := Range[to - from + 1]
circuitElementPosition["Barrier"[order_ ? orderQ, ___], from_, to_, ___] := Select[order, Between[{from, to}]] - from + 1
circuitElementPosition["Barrier"[span : Span[_Integer, _Integer | All], ___], from_, to_, ___] := Range @@ (Replace[List @@ span, {x_Integer :> Clip[x, {from ,to}], All -> to}, {1}] - from + 1)
circuitElementPosition[order_List, from_, _] := Union[Flatten[order]] - from + 1
circuitElementPosition[op_, from_, to_] := circuitElementPosition[op["Order"], from, to]


circuitPositions[circuit_QuantumCircuitOperator, level_ : 1, defaultOverlapQ : True | False : False, showMeasurementWireQ : True | False : True, showExtraQuditsQ : True | False : True] := With[{
	min = Min[1, circuit["Min"]],
	max = circuit["Max"],
	width = circuit["Width"]
},
	Rest @ FoldList[
		Block[{
			op = #2,
			pos, fullPos,
			gatePos = #1[[2]],
			ranges = #1[[3]],
			shift,
			overlapQ,
			overlapShift
		},
			pos = circuitElementPosition[op, min, max];
			If[pos === {}, Return[#1, Block]];
			fullPos = Which[
				QuantumCircuitOperatorQ[op],
				With[{circuitPos = Select[pos, # + min - 1 > 0 &]}, If[circuitPos === {}, {}, Range @@ MinMax[circuitPos]]],
				QuantumChannelQ[op] && ! showExtraQuditsQ,
				Rest[pos],
				QuantumMeasurementOperatorQ[op] && ! showMeasurementWireQ && ! showExtraQuditsQ,
				pos,
				True,
				Range @@ MinMax[pos]
			];
			overlapQ = defaultOverlapQ || QuantumOperatorQ[op] && MatchQ[op["Label"], "I" | "Permutation" | "Cap" | "Cup"];
			overlapShift = Function[x, If[overlapQ, 0, NestWhile[# + 1 &, 0, ContainsAny[Lookup[ranges, x + #, {}], fullPos] &]]];
			shift = If[
				QuantumCircuitOperatorQ[op] && expandLevel[level, op["Expand"]] > 0,
				ReplacePart[ConstantArray[0, width], Thread[pos -> Max[Replace[circuitPositions[op, level - 1, defaultOverlapQ, False, False], {{___, {_, o_}} :> o, _ -> 0}]]]],
				ReplacePart[ConstantArray[0, width], Thread[pos -> 1]]
			];
			{
				gatePos = SubsetMap[With[{x = Max[gatePos[[pos]]]}, overlapShift[x] + ConstantArray[x, Length[pos]]] &, gatePos, List /@ pos],
				gatePos = gatePos + shift,
				Merge[{ranges, Max[gatePos[[pos]]] - 1 -> fullPos}, Apply[Union]]
			}
		] &,
		{ConstantArray[0, width], ConstantArray[0, width], <|0 -> {}|>},
		circuit["NormalOperators", True]
	][[All, ;; 2]]
]

circuitWires[circuit_QuantumCircuitOperator] := Block[{
	min = Min[1, circuit["Min"]],
	max = circuit["Max"],
	width = circuit["Width"],
	operators = circuit["NormalOperators", True],
	orderDims, inWires, outWires
},
	orderDims = If[BarrierQ[#], {{{}, {}}, {{}, {}}}, {#["Order"], {#["OutputDimensions"], #["InputDimensions"]}}] & /@ operators;
	inWires = Catenate @ ReplacePart[{-1, _, 2} -> -1] @ FoldPairList[
		{prev, orderDim} |-> Block[{next, skip, input, output},
			{next, skip} = prev;
			{output, input} = orderDim[[1]] - min + 1;
			next[[ Union[output, input] ]] = Max[next] + skip;
			{MapThread[DirectedEdge[prev[[1, #]], next[[#]], {# + min - 1, #2}] &, {input, orderDim[[2, 2]]}], {next, If[orderDim[[1]] === {{}, {}}, skip + 1, 1]}}
		],
		{Table[0, width], 1},
		With[{outOrder = Union[circuit["OutputOrder"], circuit["FreeOrder"]]},
			Append[{{{}, outOrder}, {{}, Replace[outOrder, Append[_ -> 2] @ Thread[circuit["OutputOrder"] -> circuit["OutputDimensions"]], {1}]}}] @ orderDims
		]
	];
	outWires = DeleteCases[DirectedEdge[__, {_, 1}]] @ Catenate @ FoldPairList[
		{prev, orderDim} |-> Block[{next, dims, skip, input, output, order},
			{next, dims, skip} = prev;
			{output, input} = orderDim[[1]] - min + 1;
			order = Union[output, input];
			next[[ order ]] = Max[next] + skip;
			{MapThread[DirectedEdge[prev[[1, #]], next[[#]], {# + min - 1, #2}] &, {input, dims[[input]]}], dims[[ output ]] = orderDim[[2, 1]]; {next, dims, If[orderDim[[1]] === {{}, {}}, skip + 1, 1]}}
		],
		{Table[0, width], Table[1, width], 1},
		orderDims
	];
	{outWires, inWires}
]

Options[CircuitDraw] := Join[{"Dynamic" -> False}, Options[circuitDraw], Options[Graphics]];
CircuitDraw[circuit_QuantumCircuitOperator, opts : OptionsPattern[]] := If[TrueQ[OptionValue["Dynamic"]],
	DynamicModule[{qc = circuit},
		Dynamic[
			Graphics[
				circuitDraw[
					qc, Dynamic[qc], {},
					FilterRules[{opts}, Options[circuitDraw]],
					"Expand" -> None,
					RoundingRadius -> 0.1
				],
				FilterRules[{opts}, Options[Graphics]]
			],
			TrackedSymbols :> {qc}
		]
	],
	Module[{qc = circuit},
		Graphics[
			circuitDraw[
				circuit, Dynamic[qc], {},
				FilterRules[{opts}, Options[circuitDraw]],
				RoundingRadius -> 0.1
			],
			FilterRules[{opts}, Options[Graphics]]
		]
	]
]

