Package["Wolfram`QuantumFramework`Experimental`"]

PackageImport["Wolfram`QuantumFramework`"]


PackageExport[DecomposedQuantumStateProbabilities]
PackageExport[QuantumBeamSearch]
PackageExport[QuantumDiagramProcess]

PackageExport[QuantumMPS]
PackageExport[QuantumMPO]
PackageExport[QuantumMPSApply]

PackageExport[FeynmanBacktracking]



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

QuantumMPS[qs_QuantumState, m : _Integer | Infinity : Infinity, OptionsPattern[]] := Block[{
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

QuantumMPS[qo_QuantumOperator, m : _Integer | Infinity : Infinity, opts : OptionsPattern[]] :=
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
				QuantumOperator[{"Curry", Replace[{-1, 0}, #["OutputOrderDimensions"], {1}]}, {-1, 0} -> {0}][#],
				QuantumOperator[#["State"], {#["OutputOrder"] /. -1 -> 0, #["InputOrder"]}]
			] &,
			{;; -2}
		] //
		MapAt[
			If[
				ContainsAll[#["InputOrder"], {-1 ,0}],
				#[QuantumOperator[{"Uncurry", Replace[{-1, 0}, #["InputOrderDimensions"], {1}]}, {0} -> {-1, 0}]],
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

QuantumMPSApply[mps_QuantumCircuitOperator, op_] /; op["InputOrder"] == op["OutputOrder"] && op["OutputQudits"] == 1 := Block[{
	ops = mps["Operators"],
	pos, us
},
	pos = FirstPosition[ops, o_ /; IntersectingQ[o["OutputOrder"], op["InputOrder"]], Missing[], {1}, Heads -> False];
	If[MissingQ[pos], mps /* op, QuantumCircuitOperator[ReplacePart[ops, Thread[pos -> op @ Extract[ops, pos]]], mps["Label"] /* op["Label"]]]	
]

QuantumMPSApply[mps_QuantumCircuitOperator, op_] /; op["InputOrder"] == op["OutputOrder"] && op["OutputQudits"] == 2 := Block[{
	ops = mps["Operators"],
	pos, us,
	newMPS, newOps
},
	pos = Position[ops, o_ /; IntersectingQ[o["OutputOrder"], op["InputOrder"]], {1}, Heads -> False];
	us = Extract[ops, pos];
	(* swap bond indices, maybe unnecessary as operator looks symmetric (at least for this special case) *)
	newMPS = QuantumMPS[QuantumOperator[QuantumCircuitOperator[Append[us, op]]]["State"]["Permute", Cycles[{{1, 4}}]]];
	newOps = {
		QuantumOperator[newMPS[[;; newMPS["Gates"] / 2]]],
		QuantumOperator[newMPS[[newMPS["Gates"] / 2 + 1 ;;]]]
	};
	newOps = {
		QuantumOperator[
			newOps[[1]]["State"]["Permute", FindPermutation[{1, 3, 2}]]["SplitDual", 1],
			{0} -> {0, First[op["OutputOrder"]]}
		],
		QuantumOperator[
			newOps[[2]]["State"]["Permute", FindPermutation[{3, 1, 2}]]["SplitDual", 2],
			{0} -> {0, Last[op["OutputOrder"]]}
		]
	};
	QuantumCircuitOperator[ReplacePart[ops, Thread[pos -> newOps]], mps["Label"] /* op["Label"]]	
]


$FeynmanBacktrackingCache = <||>;

iFeynmanBacktracking[ops : {___QuantumOperator}, state : {___Integer ? Positive} | Automatic, width_Integer, implicitInputOrder_List : {}] := Enclose @ Block[{
	key, amplitude, op, outputPos, inputPos, padState, inputs
},
	key = {Length[ops], state};
	If[ key[[1]] == 0,
		amplitude = Boole @ MatchQ[Delete[state, List /@ implicitInputOrder], {1 ...}];
		(*$FeynmanBacktrackingCache[key] = {amplitude, 1};*)
		Return[amplitude]
	];
	amplitude = Lookup[$FeynmanBacktrackingCache, Key[key]];
	If[! MissingQ[amplitude], $FeynmanBacktrackingCache[[Key[key], 2]]++; Return[amplitude[[1]]]];

	op = ops[[-1]];
	{outputPos, inputPos} = op["Order"];
	padState = PadRight[Replace[state, Automatic :> ConstantArray[1, Length[width]]], width, 1];
	inputs = Extract[op["Tensor"], padState[[outputPos]]];
	amplitude = If[ ArrayQ[inputs],
		inputs = SparseArray[inputs];
		Total @ MapThread[#2 iFeynmanBacktracking[Most[ops], ReplacePart[padState, Thread[inputPos -> #1]], width, implicitInputOrder] &, {inputs["ExplicitPositions"], inputs["ExplicitValues"]}]
		,
		(* no input - operator is a state *)
		If[inputs == 0, 0, inputs iFeynmanBacktracking[Most[ops], padState, width, Join[implicitInputOrder, outputPos]]]
	];
	$FeynmanBacktrackingCache[key] = {amplitude, 1};
	amplitude
]

FeynmanBacktracking[circuit_QuantumCircuitOperator, state : {___Integer ? Positive} : Automatic] := (
	$FeynmanBacktrackingCache = <||>;
	iFeynmanBacktracking[circuit["Flatten"]["NormalOperators"], state, circuit["Width"]]
)

