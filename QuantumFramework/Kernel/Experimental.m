Package["Wolfram`QuantumFramework`"]

PackageScope["DecomposedQuantumStateProbabilities"]
PackageScope["QuantumBeamSearch"]

PackageExport["CircuitMultiwayGraph"]



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
		op[QuantumTensorProduct @@ states[[inputOrder]]]["Decompose"]
	]
]

CircuitMultiwayGraph[circuit_, initStates : Except[OptionsPattern[]] : Automatic, opts : OptionsPattern[]] := Enclose @ Block[{
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
							"Index" -> index
						|>]
					] &,
					Confirm @ operatorApply[op, states]
				]
			)],
			{{{}, Replace[initStates, Automatic -> Table[QuantumState["0"], circuit["Arity"]]]}},
			#["Sort"] & /@ circuit["Operators"],
			opts,
			GraphLayout -> {"LayeredDigraphEmbedding", "Orientation" -> Left}
		],
		{_, states_} :> states
	]
]

