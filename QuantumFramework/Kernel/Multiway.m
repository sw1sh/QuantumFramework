
Package["Wolfram`QuantumFramework`"]

PackageExport[QuantumCircuitMultiwayGraph]
PackageExport[QuantumCircuitMultiwayCausalGraph]
PackageExport[QuantumCircuitPathGraph]
PackageExport[QuantumCircuitTokenEventGraph]



operatorApply[op_ ? QuantumOperatorQ, states : {_ ? QuantumStateQ ...}] := Enclose @ With[{
	inputOrder = op["FullInputOrder"],
	outputOrder = op["FullOutputOrder"]
},
	Map[
		#[[1]] -> ReplacePart[states, Thread[outputOrder -> #[[2]]]] &,
		Which[
			inputOrder === outputOrder === {},
			{FullSimplify[op["Norm"]] -> {}},
			inputOrder === {},
			op["State"]["FullSimplify"]["DecomposeWithAmplitudes", op["OutputDimensions"]],
			True,
			op["State"][QuantumTensorProduct @@ states[[inputOrder]]]["FullSimplify"]["DecomposeWithAmplitudes", op["OutputDimensions"]]
		]
	]
]


Options[QuantumCircuitMultiwayGraph] = Join[{"Normalize" -> False}, Options[Graph]];
QuantumCircuitMultiwayGraph[circuit_, initStates : Except[OptionsPattern[]] : Automatic, opts : OptionsPattern[]] := Enclose @ Block[{
	index = 0, normalizeQ = TrueQ[OptionValue[QuantumCircuitMultiwayGraph, {opts}, "Normalize"]]
},
	ResourceFunction["FoldGraph"][
		List /* Replace[{{pos_, states_}, op_} :> Block[{weightedStates = Confirm @ operatorApply[op, states], norm},
			norm = Total[weightedStates[[All, 1]]];
			index++;
			MapIndexed[
				With[{newPos = Join[pos, #2]},
					Labeled[{newPos, If[normalizeQ, #1[[2]], With[{factor = #1[[1]] ^ (1 / Length[op["FullOutputOrder"]])}, MapAt[factor * # &, #1[[2]], List /@ op["FullOutputOrder"]]]]}, <|
						"Input" -> op["FullInputOrder"],
						"Output" -> op["FullOutputOrder"],
						"Step" -> Length[newPos],
						"TreePosition" -> newPos,
						"Index" -> index,
						"Probability" -> #1[[1]] / norm,
						"Operator" -> op
					|>]
				] &,
				weightedStates
			]
		]],
		{{{}, Replace[initStates, Automatic :>
			(QuantumState["Register", #] & /@ Replace[
				Union[circuit["OutputOrder"], circuit["FreeOrder"]],
				Append[_ -> 2] @ Thread[circuit["InputOrder"] -> circuit["InputDimensions"]],
				{1}
			])
		]
		}},
		circuit["Flatten"]["NormalOperators"],
		FilterRules[{opts}, Options[Graph]],
		GraphLayout -> {"LayeredDigraphEmbedding", "Orientation" -> Left}
	]
]

GraphSources[graph_] := Pick[VertexList @ graph, VertexInDegree @ graph, 0]
GraphSinks[graph_] := Pick[VertexList @ graph, VertexOutDegree @ graph, 0]

lineGraph[g_, opts : OptionsPattern[Graph]] := With[{
    edges = DeleteDuplicates @ Catenate[(v |-> DirectedEdge @@@
        Tuples[{
            Cases[EdgeList[g], DirectedEdge[_, Verbatim[v], ___]],
            Cases[EdgeList[g], DirectedEdge[Verbatim[v], __]]}]) /@ VertexList[g]
    ]
},
    Graph[
        EdgeList[g], edges,
        opts
    ]
]

Options[QuantumCircuitMultiwayCausalGraph] = Join[{"IncludeInitialEvent" -> False, "IncludeBranchialEdges" -> True}, Options[Graph]];

QuantumCircuitMultiwayCausalGraph[qc_QuantumCircuitOperator, opts___] :=
	Enclose @ QuantumCircuitMultiwayCausalGraph[ConfirmBy[QuantumCircuitMultiwayGraph[qc], GraphQ], opts]

QuantumCircuitMultiwayCausalGraph[sg_ ? GraphQ, opts : OptionsPattern[]] := Enclose @ Block[{events = EdgeList[sg], cg, branchialEdges},
	cg = If[Length[events] > 0, TransitiveReductionGraph @ EdgeDelete[
			TransitiveClosureGraph[lineGraph[sg]],
			DirectedEdge[DirectedEdge[_, _, tag1_], DirectedEdge[_, _, tag2_]] /;
				! MatchQ[tag2["TreePosition"], Append[tag1["TreePosition"], ___]] || ! IntersectingQ[tag1["Output"], tag2["Input"]]
		],
		Graph[{}, {}]
	];
	If[ TrueQ[OptionValue["IncludeInitialEvent"]], cg = EdgeAdd[cg,
		With[{inits = GraphSources[SimpleGraph[sg]], initEvents = GraphSources[cg]}, Catenate @ Outer[DirectedEdge[
			DirectedEdge[{{}, {}}, #1, <|
				"Input" -> {},
				"Output" -> Range[Length[#1[[2]]]],
				"Step" -> 0,
				"TreePosition" -> {},
				"Index" -> 0,
				"Probability" -> 1,
				"Operator" -> QuantumOperator["I", Range[Length[#1[[2]]]]]
			|>], #2] &, inits, initEvents, 1]]]];
	If[ TrueQ[OptionValue["IncludeBranchialEdges"]],
		branchialEdges = Catenate @ Values[
			UndirectedEdge @@@ SubsetCases[#,
				{_[_, _, tag1_], _[_, _, tag2_]} /; IntersectingQ[tag1["Input"], tag2["Input"]] || tag1["Input"] == tag2["Input"],
				Overlaps -> True
			] & /@
				GroupBy[
					VertexList[cg],
					With[{tp = #[[3]]["TreePosition"]}, tp[[;; Max[0, Length[tp] - 1]]]] &
				]
		];
		cg = EdgeAdd[cg, branchialEdges];
	];
	Graph[cg,
		FilterRules[{opts}, Options[Graph]],
		VertexShapeFunction -> Function[Inset[Tooltip[Framed[
			Style[#2[[3]]["Operator"]["Label"], Black],
			Background -> Directive[Opacity[0.2], Hue[0.14, 0.34, 1]],
			FrameMargins -> {{2, 2}, {0, 0}},
			FrameStyle -> Directive[Opacity[0.3], Hue[0.09, 1, 0.91]]
		], KeyDrop[#2[[3]], "Operator"]], #1, #3]],
		VertexStyle -> _DirectedEdge -> ResourceFunction["WolframPhysicsProjectStyleData"]["CausalGraph", "VertexStyle"],
		EdgeStyle -> {
			_ -> ResourceFunction["WolframPhysicsProjectStyleData"]["StatesGraph", "EdgeStyle"],
			_UndirectedEdge -> Directive[Thick, Dashed, ResourceFunction["WolframPhysicsProjectStyleData"]["BranchialGraph", "EdgeStyle"]],
			DirectedEdge[_DirectedEdge, _DirectedEdge] -> ResourceFunction["WolframPhysicsProjectStyleData"]["CausalGraph", "EdgeStyle"]
		},
		GraphLayout -> "LayeredDigraphEmbedding",
		PerformanceGoal -> "Quality", FormatType -> StandardForm, BaseStyle -> Bold
	]
]


UntaggedGraph[g_] := Graph[VertexList[g], EdgeList[g][[All, ;; 2]], Options[g] /. e_DirectedEdge :> e[[;; 2]]]

Options[QuantumCircuitPathGraph] = Join[{"Tagged" -> False}, Options[Graph]]

QuantumCircuitPathGraph[qc_ ? QuantumCircuitOperatorQ, opts : OptionsPattern[]] := Block[{
	ops = qc["NormalOperators"], g, weights,
	taggedQ = TrueQ[OptionValue["Tagged"]]
},
	If[	qc["InputOrder"] =!= {},
		PrependTo[ops, QuantumOperator[QuantumState[QuantumState[{1}, qc["InputDimensions"]], qc["Input"]["Dual"]], {qc["InputOrder"], {}}]]
	];
	g = VertexReplace[
		ResourceFunction["FoldGraph"][{bot, top} |->
			MapThread[
				With[{amplitude = Chop @ FullSimplify[#2]}, If[amplitude == 0, Nothing, Labeled[{bot[[1]] + 1, #1}, {amplitude, top["Order"]}]]] &,
				With[{newOp = top[bot[[2]]]["Sort"]},
					{QuantumOperator[#, {newOp["OutputOrder"], {}}] & /@ newOp["OutputBasis"]["BasisStates"], newOp["StateVector"]}
				]
			],
			{{1, QuantumOperator[{{1}}, {{}, {}}]}},
			ops
		],
		{i_, op_} :> {i, First @ Keys[op["Amplitude"]], op["OutputOrder"]}
	];
	weights = EdgeTags[g][[All, 1]];
	g = Graph[g,
		FilterRules[{opts}, Options[Graph]],
        VertexShapeFunction -> Function[Inset[Framed[Style[#2[[2]], Black], Background -> LightBlue], #1, #3]],
		VertexLabels -> {_, formula_, _} :> Placed[formula, Tooltip],
		VertexWeight -> Thread[VertexList[g] -> 1],
        EdgeWeight -> weights,
		EdgeStyle -> If[AllTrue[weights, RealValuedNumericQ],
			Thread[EdgeList[g] -> ({Arrowheads[0.0075], Thickness[0.001 Abs[#]], Replace[Sign[#], {-1 -> Blue, 1 -> Red}]} & /@ weights)],
			Automatic
		],
		VertexCoordinates -> RotationTransform[- Pi / 2] @ GraphEmbedding[g, {"MultipartiteEmbedding", "VertexPartition" -> Length /@ GatherBy[VertexList[g], First]}],
        PerformanceGoal -> "Quality"
	];

	If[! taggedQ, g = UntaggedGraph[g]];
	g
]

QuantumCircuitTokenEventGraph[qc_ ? QuantumCircuitOperatorQ, opts___] := With[{
    events = Replace[
        EdgeList[QuantumCircuitPathGraph[qc, "Tagged" -> True]],
        DirectedEdge[{_, q1_, o1_}, {_, q2_, o2_}, {p_, {out_, in_}}] :> DirectedEdge[
            Replace[Thread @ {Extract[FirstCase[q1, q_QuditName :> q["Name"], None, All], FirstPosition[o1, #] & /@ in], in}, {} -> {$QuditIdentity}],
            Thread @ {Extract[FirstCase[q2, q_QuditName :> q["Name"], None, All], FirstPosition[o2, #] & /@ out], out},
            p
        ],
        {1}
    ]
},

    SimpleGraph[
        Catenate[(event |-> Join[DirectedEdge[#, event] & /@ event[[1]], DirectedEdge[event, #] & /@ event[[2]]]) /@ events],
        opts,
        VertexStyle -> Thread[events -> Hue[0.11, 1, 0.97]],
        VertexShapeFunction -> {
            _DirectedEdge -> Function[Inset[Framed[Style[#2[[3]], Black], FrameStyle -> Directive[Thick, Hue[0.11, 1, 0.97]], Background -> If[RealValuedNumericQ[#2[[3]]], If[#2[[3]] < 0, LightBlue, LightPink], White]], #1, #3]],
            Except[_DirectedEdge] -> Function[Inset[Framed[Style[Replace[#2, {l_, o_} :> Subscript[l, o]], Black], Background -> White], #1, #3]]
        },
        EdgeStyle -> Directive[Opacity[.5], Arrowheads[0.005]],
        PerformanceGoal -> "Quality"
    ]
]

