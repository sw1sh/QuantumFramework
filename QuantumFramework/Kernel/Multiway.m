
Package["Wolfram`QuantumFramework`"]

PackageExport[QuantumCircuitMultiwayGraph]
PackageExport[QuantumCircuitMultiwayCausalGraph]
PackageExport[QuantumCircuitPathGraph]
PackageExport[QuantumCircuitTokenEventGraph]



operatorApply[op_, states : {_ ? QuantumStateQ ...}, basisQ_ : False] := Enclose @ With[{
	outputOrder = Replace[op["FullOutputOrder"], op["OutputOrderQuditMapping"], 1],
	inputOrder = Replace[op["FullInputOrder"], op["InputOrderQuditMapping"], 1],
	decompose = If[TrueQ[basisQ], {"BasisDecompose"}, {"DecomposeWithAmplitudes", op["OutputDimensions"]}]
},
	Map[
		#[[1]] -> ReplacePart[states, Thread[outputOrder -> #[[2]]]] &,
		Which[
			inputOrder === outputOrder === {},
			{Simplify[op["Norm"]] -> {}},
			inputOrder === {},
			op["State"]["Simplify"] @@ decompose,
			True,
			op["State"][QuantumTensorProduct @@ states[[inputOrder]]]["Simplify"] @@ decompose
		]
	]
]


Options[QuantumCircuitMultiwayGraph] = Join[{"Normalize" -> False, "BasisDecompose" -> False}, Options[Graph]];
QuantumCircuitMultiwayGraph[PatternSequence[circuit_, initStates_ : Automatic], opts : OptionsPattern[]] := Enclose @ Block[{
	index = 0
},
	ResourceFunction["FoldGraph"][
		List /* Replace[{{pos_, states_}, op_} :> Block[{weightedStates = Confirm @ operatorApply[op, states, TrueQ[OptionValue["BasisDecompose"]]], norm},
			norm = Total[weightedStates[[All, 1]]];
			index++;
			MapIndexed[
				With[{newPos = Join[pos, #2]},
					Labeled[
						{newPos, If[TrueQ[OptionValue["Normalize"]],
							#1[[2]]
							,
							With[{factor = #1[[1]] ^ (1 / Length[op["FullOutputOrder"]])},
								MapAt[factor * # &, #1[[2]], List /@ Replace[op["FullOutputOrder"], op["OutputOrderQuditMapping"], 1]]
							]
						]},
						<|
							"Input" -> op["FullInputOrder"],
							"Output" -> op["FullOutputOrder"],
							"Step" -> Length[newPos],
							"TreePosition" -> newPos,
							"Index" -> index,
							"Probability" -> #1[[1]] / norm,
							"Operator" -> op
						|>
					]
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
		circuit["NormalOperators"],
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

Options[QuantumCircuitPathGraph] = Join[{"Tagged" -> False, "ThicknessFactor" -> 5}, Options[Graph]]

QuantumCircuitPathGraph[qc_ ? QuantumCircuitOperatorQ, opts : OptionsPattern[]] := Block[{
	ops = qc["NormalOperators"], g, weights,
	taggedQ = TrueQ[OptionValue["Tagged"]],
	thickness = OptionValue["ThicknessFactor"]
},
	If[	qc["InputOrder"] =!= {},
		PrependTo[ops, QuantumOperator[QuantumState[QuantumState[{1}, qc["InputDimensions"]], qc["Input"]["Dual"]], {qc["InputOrder"], {}}]]
	];
	g = VertexReplace[
		ResourceFunction["FoldGraph"][{bot, top} |->
			MapThread[
				With[{amplitude = Chop @ Simplify[#2]}, If[N[amplitude] == 0., Nothing, Labeled[{MapAt[# + 1 &, Join[AssociationThread[top["OutputOrder"] -> 0], bot[[1]]], {Key[#]} & /@ top["OutputOrder"]], #1}, {amplitude, top["Order"]}]]] &,
				With[{newOp = top[bot[[2]]]["Sort"]},
					{QuantumOperator[#, {newOp["OutputOrder"], {}}] & /@ newOp["OutputBasis"]["BasisStates"], newOp["StateVector"]}
				]
			],
			{{<||>, QuantumOperator[{{1}}, {{}, {}}]}},
			ops
		],
		{i_, op_} :> {{i, op["OutputOrder"]}, First @ Keys[op["Amplitude"]]}
	];
	weights = EdgeTags[g][[All, 1]];
	g = Graph[g,
		FilterRules[{opts}, Options[Graph]],
        VertexShapeFunction -> Function[Inset[Framed[Style[#2[[2]], Black], Background -> LightBlue], #1, #3]],
		VertexLabels -> {_, formula_} :> Placed[formula, Tooltip],
		VertexWeight -> Thread[VertexList[g] -> 1],
        EdgeWeight -> weights,
		EdgeStyle -> If[AllTrue[weights, RealValuedNumericQ],
			Thread[EdgeList[g] -> ({Arrowheads[0.0075], Thickness[thickness 0.001 Abs[#]], If[N[Sign[#]] == -1., Blue, Red]} & /@ weights)],
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
        DirectedEdge[{{i_, o1_}, q1_}, {{j_, o2_}, q2_}, {p_, {out_, in_}}] :> DirectedEdge[
            {Lookup[i, #[[2]]], #[[2]]} -> #[[1]] & /@ Replace[Thread @ {Extract[FirstCase[q1, q_QuditName :> q["Name"], None, All], FirstPosition[o1, #] & /@ in], in}, {} -> {{$QuditIdentity, {}}}],
           	{Lookup[j, #[[2]]], #[[2]]} -> #[[1]] & /@ Thread @ {Extract[FirstCase[q2, q_QuditName :> q["Name"], None, All], FirstPosition[o2, #] & /@ out], out},
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
            Except[_DirectedEdge] -> Function[Inset[Framed[Style[Replace[#2, {({{}, {}} -> l_) :> l, ({t_, o_} -> l_) :> Subsuperscript[l, o, t]}], Black], Background -> White], #1, #3]]
        },
        EdgeStyle -> Directive[Opacity[.5], Arrowheads[0.005]],
        PerformanceGoal -> "Quality"
    ]
]

