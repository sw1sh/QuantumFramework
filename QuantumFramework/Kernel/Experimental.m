Package["Wolfram`QuantumFramework`"]

PackageScope["DecomposedQuantumStateProbabilities"]
PackageScope["QuantumBeamSearch"]
PackageScope["QuantumDiagramProcess"]

PackageExport["QuantumCircuitMultiwayGraph"]
PackageExport["QuantumCircuitMultiwayCausalGraph"]
PackageExport["QuantumMPS"]
PackageExport["QuantumMPO"]



DecomposedQuantumStateProbabilities[states : {{__QuantumState}..}] :=
	Normalize[Abs[Total[Flatten[Outer[Times, Sequence @@ #, 1], Length[#]] & /@ Map[#["Computational"]["StateVector"] &, states, {2}]]] ^ 2, Total]

DecomposedQuantumStateProbabilities[states : {(_ -> {__QuantumState})..}] :=
	Normalize[Abs[Total[states[[All, 1]] (Flatten[Outer[Times, Sequence @@ #, 1], Length[#]] & /@ Map[#["Computational"]["StateVector"] &, states[[All, 2]], {2}])]] ^ 2, Total]

BeamBranch[prob_ -> states_, op_] := With[{
	decompose = op["State"][
		QuantumTensorProduct[states[[op["InputOrder"]]]]
	]["DecomposeWithProbabilities"]
},
	prob #1 -> ReplacePart[states, Thread[op["InputOrder"] -> #2]] & @@@ decompose
]


Options[QuantumBeamSearch] = {"Width" -> 8, "Deterministic" -> False, "Shots" -> 1};

QuantumBeamSearch[states_List, ops_List, OptionsPattern[]] := Module[{
	width = OptionValue["Width"],
	random = !TrueQ[OptionValue["Deterministic"]],
	shots = OptionValue["Shots"]
},
	Normalize[#, Total] & @ Total @ Table[
		DecomposedQuantumStateProbabilities @ Fold[
			{beam, op} |-> SubsetMap[Normalize[#, Total] &, {All, 1}] @
				With[{candidates = Catenate[BeamBranch[#, op] & /@ beam]},
					If[ random,
						RandomSample[candidates[[All, 1]] -> candidates, UpTo[width]],
						TakeLargestBy[candidates, First, UpTo[width]]
					]
				],
			{1 -> states},
			N @ ops
		],
		shots
	]
]


operatorApply[op_ ? QuantumOperatorQ, states : {_ ? QuantumStateQ ...}] := Enclose @ With[{
	inputOrder = op["FullInputOrder"],
	outputOrder = op["FullOutputOrder"]
},
	(* ConfirmAssert[1 <= Min[inputOrder] <= Max[inputOrder] <= Length[states]];
	ConfirmAssert[1 <= Min[outputOrder] <= Max[outputOrder] <= Length[states]]; *)
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



DiagramProcess := DiagramProcess = ResourceFunction["https://www.wolframcloud.com/obj/murzin.nikolay/DeployedResources/Function/DiagramProcess"]

QuantumDiagramProcess[qco_QuantumCircuitOperator] := With[{
    ops = qco["Operators"], net = qco["TensorNetwork", "PrependInitial" -> False], n = qco["GateCount"]
},
    With[{
        map = GroupBy[EdgeTags[net], #[[2]] &, #[[1, 1]] &],
        freeIndices = TensorNetworkFreeIndices[net]
    },
        DiagramProcess[
            Subsuperscript[
                With[{mat = ops[[#]]["Computational"]["Tensor"]}, Labeled[Part[mat, ##] &, ops[[#]]["Label"]]],
                Sequence @@ Reverse @ Replace[
                    MapAt[ReplaceAll[map], TakeDrop[HoldForm /@ AnnotationValue[{net, #}, "Index"], ops[[#]]["OutputQudits"]], 2],
                    With[{
                        outs = Alternatives @@ Cases[freeIndices, _Superscript],
                        ins = Alternatives @@ Cases[freeIndices, _Subscript]
                    },
                        {in : HoldForm[ins] :> Overscript[in, ˘], out : HoldForm[outs] :> Overscript[out, \[DownBreve]]}
                    ],
                    {2}
                ]
            ] & /@ Range[n]
        ]
    ]
]


Options[QuantumMPS] = {"Ordered" -> False, "Sides" -> True}

QuantumMPS[qs_ ? QuantumStateQ, m : _Integer | Infinity : Infinity, OptionsPattern[]] := Block[{
	decompose = If[VectorQ[#[[All, 1]], NumericQ], TakeLargestBy[#, First, UpTo[m]], #] & @ qs["DecomposeWithAmplitudes", qs["Dimensions"]],
	dimensions = qs["Dimensions"],
	proba, n, rowVector, colVector, matrices, result
},
	n = Length[decompose];
	proba = Keys[decompose];

	matrices = If[n > 1,
		colVector = QuantumOperator[{Table[{1}, n]}, {{0}, {}}, QuantumBasis[{n}, {}, "Label" -> TraditionalForm[Bra[{" "}]]]];
		rowVector = QuantumOperator[{proba}, {{}, {0}}, QuantumBasis[{}, {n}, "Label" -> TraditionalForm[Ket[{" "}]]]];
		matrices = MapIndexed[
			QuantumOperator[
				Transpose[
					ReplacePart[ConstantArray[Table[0, #1[[2]]], {n, n}], Thread[{#, #} & /@ Range[n] -> #1[[1]], List, 2]],
					2 <-> 3
				],
				If[#2[[1]] <= qs["OutputQudits"], {Prepend[#2, 0], {0}}, {{0}, Append[#2, 0]}],
				QuantumBasis[{n, #1[[2]]}, {n}]
			] &,
			Thread[{Transpose @ Map[#["Computational"]["StateVector"] &, Values[decompose], {2}], dimensions}]
		],

		colVector = Nothing;
		rowVector = QuantumOperator[{proba}, {{}, {}}, QuantumBasis[{}, {}, "Label" -> TraditionalForm[First[proba]]]];
		matrices = MapIndexed[
			QuantumOperator[{{#1[[1]]}}, {#2, {}}, QuantumBasis[{#1[[2]]}, {}]] &,
			Thread[{Transpose @ Map[#["Computational"]["StateVector"] &, Values[decompose], {2}], dimensions}]
		]
	];
	result = If[TrueQ[OptionValue["Sides"]],
		QuantumCircuitOperator[{colVector, Splice @ matrices, rowVector}],
		QuantumCircuitOperator[MapAt[rowVector, -1] @ MapAt[If[colVector === Nothing, #, #[colVector]] &, 1] @ matrices]
	];
	If[	TrueQ[OptionValue["Ordered"]],
		result = Reverse[MapIndexed[{"I", #1} -> #2 -> qs["OutputQudits"] + #2 &, qs["InputDimensions"]]] /* result
	];
	QuantumCircuitOperator[result, "MPS"]
]

QuantumMPS[qo_ ? QuantumOperatorQ, m : _Integer | Infinity : Infinity, opts : OptionsPattern[]] :=
	With[{range = Range[Length[qo["InputOrder"]]]},
		{{"Permutation", qo["InputDimensions"], range} -> qo["InputOrder"] -> range + Length[qo["OutputOrder"]]}
	] /*
	QuantumMPS[qo["State"], m, "Ordered" -> False, opts] /*
	With[{range = Range[Length[qo["OutputOrder"]]]},
		{{"Permutation", qo["OutputDimensions"], range} -> range -> qo["OutputOrder"]}
	]


Options[QuantumMPO] = {"Ordered" -> True}

QuantumMPO[qo_QuantumOperator, m : _Integer | Infinity : Infinity, OptionsPattern[]] := Block[{
	mps = QuantumMPS[qo["ReverseInput"]["State"], m, "Sides" -> False],
	top, bot, split,
	result
},
	{top, bot} = TakeDrop[mps["Operators"], qo["OutputQudits"]];
	split = Length[top] - Length[bot];
	bot = #["Transpose", {{0, -1}, {-1, 0}}] & /@ Reverse[bot];
	result = QuantumCircuitOperator[
		If[ split > 0,
			Join[top[[;; split]], MapThread[Construct, {top[[split + 1 ;;]], bot}]],
			Join[bot[[;; - split]], MapThread[Construct, {top, bot[[- split + 1 ;;]]}]]
		] //
		MapAt[QuantumOperator[{"Cap", #["OutputDimensions"][[1]]}, {-1, 0}][#] &, -1] //
		MapAt[
			If[
				ContainsAll[#["OutputOrder"], {-1 ,0}],
				QuantumOperator[{"Uncurry", Replace[{-1, 0}, #["OutputOrderDimensions"], {1}]}, {-1, 0} -> {0}][#],
				QuantumOperator[#["State"], {#["OutputOrder"] /. -1 -> 0, #["InputOrder"]}]
			] &,
			{;; -2}
		] //
		MapAt[
			If[
				ContainsAll[#["InputOrder"], {-1 ,0}],
				#[QuantumOperator[{"Curry", Replace[{-1, 0}, #["InputOrderDimensions"], {1}]}, {0} -> {-1, 0}]],
				QuantumOperator[#["State"], {#["OutputOrder"], #["InputOrder"] /. -1 -> 0}]
			] &,
		 	{2 ;;}
		] //
		Map[QuantumOperator[
			#["State"],
			{#["OutputOrder"], Replace[#["InputOrder"], i_ /; i > qo["OutputQudits"] :> qo["Qudits"] - i + Max[split, 0] + 1, {1}]},
			"Label" -> None] &
		]
	];
	If[	TrueQ[OptionValue["Ordered"]],
		result =
			{{"Permutation", qo["InputDimensions"]} -> qo["InputOrder"] -> Reverse[Range[Length[qo["InputOrder"]]] + Max[split, 0]]} /*
			result /*
			{{"Permutation", qo["OutputDimensions"]} -> Range[Length[qo["OutputOrder"]]] -> qo["OutputOrder"]}
	];
	QuantumCircuitOperator[result, "MPO"]
]

