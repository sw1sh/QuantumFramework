$PrivateImports = {"Wolfram`QuantumFramework`PackageScope`", "Wolfram`QuantumFramework`Draw`PackagePrivate`"}

$ContextPath = Join[$ContextPath, $PrivateImports]

Package["Wolfram`QuantumFramework`DiagramPlot`"]

PackageImport["Wolfram`QuantumFramework`"]

PackageExport[QuantumCircuitDiagram]
PackageExport[DiagramPlot]



QuantumCircuitDiagram[circuit_QuantumCircuitOperator] := Block[{
    elements = circuit["NormalOperators", True]
},
    <|
        "Elements" -> Map[elem |-> 
            <|
                "Type" -> Which[
                    BarrierQ[elem], "Barrier",
                    QuantumMeasurementOperatorQ[elem], "Measurement",
                    QuantumChannelQ[elem], "Channel",
                    QuantumOperatorQ[elem], "Operator",
                    QuantumCircuitOperatorQ[elem], "Diagram",
                    True, None
                ],
                "Label" -> If[BarrierQ[elem], elem, elem["Label"]],
                "Thickness" -> If[BarrierQ[elem], None, {elem["OutputDimensions"], elem["InputDimensions"]}],
                "Position" -> If[BarrierQ[elem], {{}, {}}, elem["Order"]],
                "Thick" -> If[QuantumOperatorQ[elem], elem["MatrixQ"], False],
                "Diagram" -> If[QuantumCircuitOperatorQ[elem], QuantumCircuitDiagram[elem], None],
                "Eigendimensions" -> If[QuantumMeasurementOperatorQ[elem], elem["Eigendimensions"], None],
                "TraceDimensions" -> If[QuantumChannelQ[elem], elem["TraceDimensions"], None]
            |>
            ,
            elements
        ],
        "Label" -> circuit["Label"],
        "Expand" -> True
    |>
]

FlattenDiagram[diagram_] := MapAt[
    Map[If[#Type == "Diagram", Splice[FlattenDiagram[#Diagram]["Elements"]], #] &],
    diagram,
    "Elements"
]


DiagramAllPositions[diagram_] := Through[diagram["Elements"]["Position"]]
DiagramPositions[diagram_] := Through[Select[diagram["Elements"], #["Type"] =!= "Barrier" &]["Position"]]
DiagramMin[diagram_] := Replace[Min[DiagramPositions[diagram]], Infinity -> 1]
DiagramMax[diagram_] := Replace[Max[DiagramPositions[diagram]], - Infinity -> 1]
DiagramWidth[diagram_] := Max[DiagramMax[diagram], 1] - Min[DiagramMin[diagram], 1] + 1
DiagramPosition[diagram_] := Replace[collectOrders[DiagramPositions[diagram]], {} -> {{1}, {1}}]
DiagramFreePosition[diagram_] := Complement[Range[Min[DiagramMin[diagram], 1], DiagramMax[diagram]], Union @@ Catenate[DiagramPositions[diagram]]]
DiagramThickness[diagram_] := MapIndexed[
    With[{position = #1, i = #2[[1]]},
        Map[
            q |-> (
                If[MissingQ[#], 2, ResourceFunction["LookupPart"][#["Thickness"][[i]], q /. (Thread[# -> Range[Length[#]]] & @ #["Position"][[i]]), 2]] & @
                    SelectFirst[diagram["Elements"], elem |-> MemberQ[elem["Position"][[i]], q]]
            ),
            position
        ]
    ] &,
    DiagramPosition[diagram]
]


Options[diagramPlot] := DeleteDuplicatesBy[First] @ Join[
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
diagramPlot[diagram_, Dynamic[d_Symbol], position_List, opts : OptionsPattern[]] := Block[{
	order = Union @@ DiagramPosition[diagram],
	freeOrder = DiagramFreePosition[diagram],
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
	min = Min[1, DiagramMin[diagram]],
	max = DiagramMax[diagram],
	outlineMin,
	scalarPos = 1
},
	inputOrders = Map[Switch[#["Type"], "Barrier", Nothing, "Channel" | "Measurement", #["Position"][[2]], _, Union @@ #["Position"]] &, FlattenDiagram[diagram]["Elements"]];
	extraQuditsQ = TrueQ[OptionValue["ShowExtraQudits"]] || AnyTrue[inputOrders, NonPositive, 2];
	showMeasurementWireQ = TrueQ[OptionValue["ShowMeasurementWire"]] && ! extraQuditsQ && Count[diagram, KeyValuePattern["Type" -> "Measurement"]] > 0;
	labelCounter[label_, {out_, in_}] := If[MatchQ[label, ("Measurement" | "Channel")[_] | Subscript["C", ("Measurement" | "Channel")[_]][___]], label, label /. None :> (labelCount++; Subscript[If[out === {} || in === {}, "\[Psi]", "U"], labelCount])];
	outlineMin = Which[showMeasurementWireQ, 1, AnyTrue[order, NonPositive] && extraQuditsQ, min, emptyWiresQ, 1, True, Min[inputOrders, max]];
	positions = diagramPositions[diagram, level, MatchQ[OptionValue["GateOverlap"], Automatic | True], showMeasurementWireQ, extraQuditsQ];
	height = Max[0, positions] + 1;
	{outWires, inWires} = diagramWires[diagram];
	If[ ! emptyWiresQ,
		outWires = DeleteCases[outWires, _[_, _, {pos_, _} /; MemberQ[freeOrder, pos]]];
		inWires = DeleteCases[inWires, _[_, _, {pos_, _} /; MemberQ[freeOrder, pos]]];
	];
	If[ ! extraQuditsQ,
		outWires = DeleteCases[outWires, _[_, _, {pos_, _} /; pos < 1]];
		inWires = DeleteCases[inWires, _[_, _, {pos_, _} /; pos < 1]]
	];
	gatePositions = MapThread[{#1[[1]], #1[[2]], #2[[Union @@ #1 - min + 1]]} &, {DiagramAllPositions[diagram], positions[[All, 2]]}];
	wires = Replace[{outWires, inWires}, _[left_, right_, {pos_, dim_}] :> {{If[left == 0, 0, positions[[left, 2, pos - min + 1]]], pos}, {If[right == -1, height, positions[[right, 1, pos - min + 1]] + 1], pos}, dim}, {2}];
	{
		If[TrueQ[OptionValue["ShowOutline"]], drawOutline[outlineMin, max, height, FilterRules[{opts}, Options[drawOutline]]], Nothing],
		If[TrueQ[OptionValue["ShowWires"]], drawWires[wires, height, FilterRules[{opts}, Options[drawWires]]], Nothing],
		If[showMeasurementWireQ, drawMeasurementWire[height, max, FilterRules[{opts}, Options[drawMeasurementWire]]], Nothing],
		MapThread[
			Switch[
                #1["Type"],
				"Barrier",
				drawBarrier[Append[Table[circuitElementPosition[#1, min, max] + min - 1, 2], #3 + 1], "ShowExtraQudits" -> extraQuditsQ, "Label" -> Replace[#1, {"Barrier"[_, label_, ___] :> label, _ -> None}], FilterRules[{opts}, Options[drawBarrier]]],
				"Diagram",
				EventHandler[
					If[ expandLevel[level, #1["Diagram"]["Expand"]] > 0,
						Translate[
							diagramPlot[#1["Diagram"], Dynamic[d], Append[position, #4],
								Lookup[#1, "DiagramOptions", {}],
								OptionValue["SubcircuitOptions"],
								"Expand" -> level - 1,
								"WireLabels" -> None, "ShowOutline" -> True, "ShowLabel" -> True,
								"ShowMeasurementWire" -> False, "ShowEmptyWires" -> False, "LongOuterWires" -> False,
								"ShowExtraQudits" -> False, "ShowGlobalPhase" -> False,
								opts
							],
							{hGapSize Max[#3[[circuitElementPosition[#1, min, max]]]], 0}
						],
						drawGate[#2, {Thread[#1["Position"][[1]] -> #1["Thickness"][[1]]], Thread[#1["Position"][[2]] -> #1["Thickness"][[2]]]}, labelCounter[#1["Label"], #1["Position"]], FilterRules[{opts}, Options[drawGate]]]
					],
					{},
					PassEventsDown -> True,
					PassEventsUp -> False
				],
				"Measurement",
				drawMeasurement[#2, #1["Eigendimensions"], max, "ShowMeasurementWire" -> showMeasurementWireQ, "ShowExtraQudits" -> extraQuditsQ, "ThickWire" -> #["Thick"], "Label" -> #1["Label"], FilterRules[{opts}, Options[drawMeasurement]]],
				"Channel",
				drawMeasurement[#2, #1["TraceDimensions"], max, "ShowMeasurementWire" -> False,
					"ShowExtraQudits" -> extraQuditsQ, "ThickWire" -> #["Thick"], "Label" -> #1["Label"], "ShowGauge" -> False,
					FilterRules[{opts}, Options[drawMeasurement]]
				],
				_,
				drawGate[
					If[#2 === {{}, {}, {}}, {{scalarPos}, {scalarPos++}, {- hGapSize / 2}}, #2],
					{Thread[#1["Position"][[1]] -> #1["Thickness"][[1]]], Thread[#1["Position"][[2]] -> #1["Thickness"][[2]]]},
					labelCounter[#1["Label"], #1["Position"]],
					"ThickWire" -> #1["Thick"],
					FilterRules[{opts}, Options[drawGate]]
				]
			] &,
			{diagram["Elements"], gatePositions, positions[[All, 1]], Range[Length[gatePositions]]}
		],
		drawWireLabels[
			OptionValue["WireLabels"],
			outlineMin, max, height,
			FilterRules[{opts}, Options[drawWireLabels]]
		],
		If[TrueQ[OptionValue["ShowLabel"]], drawLabel[Replace[diagram["Label"], None -> ""], height, outlineMin, FilterRules[{opts}, Options[drawLabel]]], Nothing]
	}
]

diagramPositions[diagram_, level_ : 1, defaultOverlapQ : True | False : False, showMeasurementWireQ : True | False : True, showExtraQuditsQ : True | False : True] := With[{
	min = Min[1, DiagramMin[diagram]],
	max = DiagramMax[diagram],
	width = DiagramWidth[diagram]
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
                op["Type"] === "Diagram",
				With[{circuitPos = Select[pos, # + min - 1 > 0 &]}, If[circuitPos === {}, {}, Range @@ MinMax[circuitPos]]],
				op["Type"] === "Channel" && ! showExtraQuditsQ,
				Rest[pos],
				op["Type"] === "Measurement" && ! showMeasurementWireQ && ! showExtraQuditsQ,
				pos,
				True,
				Range @@ MinMax[pos]
			];
			overlapQ = defaultOverlapQ || op["Type"] === "Operator" && MatchQ[op["Label"], "I" | "Permutation" | "Cap" | "Cup"];
			overlapShift = Function[x, If[overlapQ, 0, NestWhile[# + 1 &, 0, ContainsAny[Lookup[ranges, x + #, {}], fullPos] &]]];
			shift = If[
				op["Type"] === "Diagram" && expandLevel[level, op["Diagram"]["Expand"]] > 0,
				ReplacePart[ConstantArray[0, width], Thread[pos -> Max[Replace[diagramPositions[op["Diagram"], level - 1, defaultOverlapQ, False, False], {{___, {_, o_}} :> o, _ -> 0}]]]],
				ReplacePart[ConstantArray[0, width], Thread[pos -> 1]]
			];
			{
				gatePos = SubsetMap[With[{x = Max[gatePos[[pos]]]}, overlapShift[x] + ConstantArray[x, Length[pos]]] &, gatePos, List /@ pos],
				gatePos = gatePos + shift,
				Merge[{ranges, Max[gatePos[[pos]]] - 1 -> fullPos}, Apply[Union]]
			}
		] &,
		{ConstantArray[0, width], ConstantArray[0, width], <|0 -> {}|>},
		diagram["Elements"]
	][[All, ;; 2]]
]

diagramWires[diagram_] := Block[{
	min = Min[1, DiagramMin[diagram]],
	max = DiagramMax[diagram],
	width = DiagramWidth[diagram],
	orderDims, inWires, outWires
},
	If[operators === {}, Return[{{}, {}}]];
	orderDims = If[BarrierQ[#["Type"]], {{{}, {}}, {{}, {}}}, {#["Position"], #["Thickness"]}] & /@ diagram["Elements"];
	inWires = Catenate @ ReplacePart[{-1, _, 2} -> -1] @ FoldPairList[
		{prev, orderDim} |-> Block[{next, skip, input, output},
			{next, skip} = prev;
			{output, input} = orderDim[[1]] - min + 1;
			next[[ Union[output, input] ]] = Max[next] + skip;
			{MapThread[DirectedEdge[prev[[1, #]], next[[#]], {# + min - 1, #2}] &, {input, orderDim[[2, 2]]}], {next, If[orderDim[[1]] === {{}, {}}, skip + 1, 1]}}
		],
		{Table[0, width], 1},
		With[{outOrder = Union[DiagramPosition[diagram][[1]], DiagramFreePosition[diagram]]},
			Append[{{{}, outOrder}, {{}, Replace[outOrder, Append[_ -> 2] @ Thread[DiagramPosition[diagram][[1]] -> DiagramThickness[diagram][[1]]], {1}]}}] @ orderDims
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

Options[DiagramPlot] := Join[{"Dynamic" -> False}, Options[diagramPlot], Options[Graphics]];
DiagramPlot[diagram_, opts : OptionsPattern[]] := If[TrueQ[OptionValue["Dynamic"]],
	DynamicModule[{d = diagram},
		Dynamic[
			Graphics[
				diagramPlot[
					d, Dynamic[d], {},
					FilterRules[{opts}, Options[diagramPlot]],
					"Expand" -> None,
					RoundingRadius -> 0.1
				],
				FilterRules[{opts}, Options[Graphics]]
			],
			TrackedSymbols :> {d}
		]
	],
	Module[{d = diagram},
		Graphics[
			diagramPlot[
				diagram, Dynamic[d], {},
				FilterRules[{opts}, Options[diagramPlot]],
				RoundingRadius -> 0.1
			],
			FilterRules[{opts}, Options[Graphics]]
		]
	]
]

$ContextPath = DeleteElements[$ContextPath, $PrivateImports];

