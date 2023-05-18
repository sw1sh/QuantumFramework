Package["Wolfram`QuantumFramework`"]

PackageScope["DecomposedQuantumStateProbabilities"]
PackageScope["QuantumBeamSearch"]
PackageScope["QuantumDiagramProcess"]

PackageExport["QuantumCircuitMultiwayGraph"]



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


operatorApply[op_ ? QuantumOperatorQ, states : {_ ? QuantumStateQ ..}] := Enclose @ With[{
	inputOrder = op["FullInputOrder"],
	outputOrder = op["FullOutputOrder"]
},
	ConfirmAssert[1 <= Min[inputOrder] <= Max[inputOrder] <= Length[states]];
	ConfirmAssert[1 <= Min[outputOrder] <= Max[outputOrder] <= Length[states]];
	Map[
		ReplacePart[states, Thread[outputOrder -> #]] &,
		op["State"][QuantumTensorProduct @@ states[[inputOrder]]]["Decompose"]
	]
]

QuantumCircuitMultiwayGraph[circuit_, initStates : Except[OptionsPattern[]] : Automatic, opts : OptionsPattern[]] := Enclose @ Block[{
	index = 0
},
	VertexReplace[
		ResourceFunction["FoldGraph"][
			List /* Replace[{{pos_, states_}, op_} :> (
				index++;
				MapIndexed[
					With[{newPos = Join[pos, #2]},
						Labeled[{newPos, #1}, <|
							"Destroyed" -> op["FullInputOrder"],
							"Created" -> op["FullOutputOrder"],
							"Step" -> Length[newPos],
							"TreePosition" -> newPos,
							"Index" -> index,
							"Operator" -> op
						|>]
					] &,
					Confirm @ operatorApply[op, states]
				]
			)],
			{{{}, Replace[initStates, Automatic -> Table[QuantumState["0"], circuit["Width"]]]}},
			#["Sort"] & /@ circuit["Flatten"]["Operators"],
			opts,
			GraphLayout -> {"LayeredDigraphEmbedding", "Orientation" -> Left}
		],
		{_, states_} :> states
	]
]



DiagramProcess := DiagramProcess = ResourceFunction["https://www.wolframcloud.com/obj/murzin.nikolay/DeployedResources/Function/DiagramProcess"]

QuantumDiagramProcess[qco_QuantumCircuitOperator] := With[{
    ops = qco["Operators"], net = qco["TensorNetwork", "PrependInitial" -> False], n = qco["Gates"]
},
    With[{
        map = GroupBy[EdgeTags[net], #[[2]] &, #[[1, 1]] &],
        freeIndices = TensorNetworkFreeIndices[net]
    },
        DiagramProcess[
            Subsuperscript[
                With[{mat = ops[[#]]["Computational"]["Tensor"]}, Labeled[Part[mat, ##] &, ops[[#]]["Label"]]],
                Sequence @@ Reverse @ Replace[
                    MapAt[ReplaceAll[map], TakeDrop[HoldForm /@ AnnotationValue[{net, # - 1}, "Index"], ops[[#]]["OutputQudits"]], 2],
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

