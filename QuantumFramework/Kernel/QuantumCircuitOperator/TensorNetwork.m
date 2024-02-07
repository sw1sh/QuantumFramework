Package["Wolfram`QuantumFramework`"]

PackageImport["Cotengra`"]

PackageExport["TensorNetworkIndexGraph"]
PackageExport["FromTensorNetwork"]
PackageExport["TensorNetworkQ"]
PackageExport["ContractTensorNetwork"]
PackageExport["TensorNetworkIndices"]
PackageExport["TensorNetworkTensors"]
PackageExport["TensorNetworkFreeIndices"]
PackageExport["TensorNetworkAdd"]
PackageExport["RemoveTensorNetworkCycles"]
PackageExport["TensorNetworkContractionPath"]
PackageExport["TensorNetworkContractPath"]
PackageExport["TensorNetworkNetGraph"]
PackageExport["TensorNetworkIndexReplace"]

PackageScope["TensorNetworkIndexDimensions"]
PackageScope["InitializeTensorNetwork"]
PackageScope["TensorNetworkApply"]
PackageScope["TensorNetworkCompile"]
PackageScope["QuantumTensorNetwork"]
PackageScope["QuantumCircuitHypergraph"]



TensorNetworkQ::msg1 = "Not all vertices are annotated with tensors."
TensorNetworkQ::msg2 = "Not all indices are duplicate free lists of Subscripts and Superscripts."
TensorNetworkQ::msg3 = "Tensor ranks and indices are not compatible."
TensorNetworkQ::msg4 = "Not all edges are tagged with indices."

TensorNetworkQ[net_Graph, verbose : _ ? BooleanQ : False] := Module[{
    tensors, indices
},
    {tensors, indices} = AssociationThread[VertexList[net] -> #] & /@
        (AnnotationValue[{net, Developer`FromPackedArray[VertexList[net]]}, #] & /@ {"Tensor", "Index"});
    (* (
        AllTrue[tensors, TensorQ] ||
        (If[verbose, Message[TensorNetworkQ::msg1]]; False)
    ) && *)
    (
        AllTrue[indices, MatchQ[#, {(Superscript | Subscript)[_, _] ...}] && DuplicateFreeQ[#] &] ||
        (If[verbose, Message[TensorNetworkQ::msg2]]; False)
     ) &&
    With[{ranks = TensorRank /@ Values[tensors]},
        (And @@ Thread[ranks == Length /@ Values[indices]] && Total[ranks] == CountDistinct[Catenate[Values[indices]]]) ||
        Total[ranks] == 0 ||
        (If[verbose, Message[TensorNetworkQ::msg3]]; False)
    ] (* &&
    (
        AllTrue[
            EdgeList[net],
            MatchQ[
                _[from_, to_, {i_, j_}] /;
                MemberQ[indices[from], i] && MemberQ[indices[to], j]
            ]
        ] ||
        (If[verbose, Message[TensorNetworkQ::msg4]]; False)
    ) *)
]

TensorNetworkQ[verbose : _ ? BooleanQ : False][net_Graph] := TensorNetworkQ[net, verbose]


NeighborhoodEdges[g_, vs_List] := Catenate[EdgeList[g, _[#, __] | _[_, #, ___]] & /@ vs]
NeighborhoodEdges[g_, v_] := NeighborhoodEdges[g, {v}]

(* temporary due to EdgeContract bug *)
edgeContract[g_, edge_] := With[{edges = NeighborhoodEdges[g, edge[[1]]]},
	EdgeAdd[EdgeDelete[g, edges], Replace[DeleteCases[edges, edge], {
		head_[edge[[1]], edge[[1]], rest___] :> head[edge[[2]], edge[[2]], rest],
		head_[edge[[1]], rest___] :> head[edge[[2]], rest],
		head_[v_, edge[[1]], rest___] :> head[v, edge[[2]], rest]
	}, {1}]]
]

edgeContractWithIndex[g_, edge : _[from_, to_, {i_, j_}]] := Annotate[
	{edgeContract[g, edge], to},
	"Index" -> If[
		from === to,
		DeleteCases[AnnotationValue[{g, to}, "Index"], i | j],
		DeleteCases[Join[AnnotationValue[{g, from}, "Index"], AnnotationValue[{g, to}, "Index"]], i | j]
	]
]

ContractEdge[g_, edge : _[from_, to_, {i_, j_}]] := Enclose @ Module[{
	tensors = ConfirmMatch[AnnotationValue[{g, {from, to}}, "Tensor"], {__ ? TensorQ}],
	indices = Confirm[AnnotationValue[{g, {from, to}}, "Index"]],
	rank
},
	rank = TensorRank[tensors[[1]]];
	Annotate[
		{edgeContractWithIndex[g, edge], to},
		"Tensor" ->TensorContract[
			TensorProduct[tensors[[1]], tensors[[2]]],
			{Confirm[FirstPosition[indices[[1]], i]][[1]], rank + Confirm[FirstPosition[indices[[2]], j]][[1]]}
		]
	]
]

ContractEdge[g_, edge : _[v_, v_, {i_, j_}]] := Enclose @ Module[{
	tensor = ConfirmBy[AnnotationValue[{g, v}, "Tensor"], TensorQ],
	index = Confirm[AnnotationValue[{g, v}, "Index"]]
},
	Annotate[{edgeContractWithIndex[g, edge], v}, "Tensor" -> TensorContract[tensor, Catenate @ Position[index, i | j]]]
]


NaiveContractTensorNetwork[net_Graph] := Enclose @ Module[{g, edges},
	{g, {edges}} = Reap @ NestWhile[Confirm @ ContractEdge[#, Sow @ First[EdgeList[#]]] &, net, EdgeCount[#] > 0 &];
	Transpose[
        AnnotationValue[{g, edges[[-1, 2]]}, "Tensor"],
        Ordering @ OrderingBy[AnnotationValue[{g, edges[[-1, 2]]}, "Index"], Replace[{Subscript[_, x_] :> {x}, Superscript[_, x_] :> x}]]
    ]
]

FastContractTensorNetwork[net_Graph] := Enclose[
    Block[{indices, outIndices, tensors, scalarPositions, scalars},
        indices = TensorNetworkIndices[net] /. Rule @@@ EdgeTags[net];
        tensors =  TensorNetworkTensors[net];
        outIndices = TensorNetworkFreeIndices[net];
        If[MemberQ[tensors, {}], Return[ArrayReshape[{}, Append[Table[1, Length[outIndices] - 1], 0]]]];
        scalarPositions = Position[indices, {}, {1}, Heads -> False];
        scalars = Extract[tensors, scalarPositions];
        indices = Delete[indices, scalarPositions];
        tensors = Delete[tensors, scalarPositions];
        Times @@ scalars * Confirm[EinsteinSummation[indices -> outIndices, tensors]]
    ],
    (ReleaseHold[#["HeldMessageCall"]]; #) &
]

Options[ContractTensorNetwork] = {Method -> Automatic}

ContractTensorNetwork[net_Graph ? (TensorNetworkQ[True]), OptionsPattern[]] := Switch[
    OptionValue[Method],
    "Naive",
    NaiveContractTensorNetwork[net],
    _,
    FastContractTensorNetwork[net]
]


TensorNetworkIndices[net_Graph ? TensorNetworkQ] := AnnotationValue[{net, Developer`FromPackedArray[VertexList[net]]}, "Index"]

TensorNetworkTensors[net_Graph ? TensorNetworkQ] := AnnotationValue[{net, Developer`FromPackedArray[VertexList[net]]}, "Tensor"]


TensorNetworkFreeIndices[net_Graph ? TensorNetworkQ] :=
    SortBy[Replace[{Superscript[_, x_] :> {0, x}, Subscript[_, x_] :> {1, x}}]] @ DeleteCases[Join @@ TensorNetworkIndices[net], Alternatives @@ Union @@ EdgeTags[net]]


TensorNetworkIndexDimensions[net_Graph ? TensorNetworkQ] :=
    Catenate @ MapThread[Thread[#2 -> Dimensions[#1]] &, {TensorNetworkTensors[net], TensorNetworkIndices[net]}]


TensorNetworkIndexReplace[net_ ? TensorNetworkQ, rules_] :=
    Graph[net, AnnotationRules -> MapThread[#1 -> {"Index" -> #2} &, {VertexList[net], Replace[TensorNetworkIndices[net], rules, {2}]}]]


InitializeTensorNetwork[net_Graph ? TensorNetworkQ, tensor_ ? TensorQ, index_List : Automatic] := Annotate[
    {
        EdgeAdd[
            VertexDelete[net, _ ? NonPositive],
            MapIndexed[
                Replace[#1, DirectedEdge[_, i_, {_, to_}] :> DirectedEdge[0, i, {Superscript[0, #2[[1]]], to}]] &,
                EdgeList[net, DirectedEdge[_ ? NonPositive, __]]
            ]
        ],
        0
    },
    {
        "Tensor" -> tensor,
        "Index" -> Replace[index, Automatic :> (Superscript[0, #] & /@ Range[TensorRank[tensor]])],
        VertexLabels -> "Initial"
    }
]

TensorNetworkAdd[net_Graph ? TensorNetworkQ, tensor_ ? TensorQ, index : _List | Automatic : Automatic] := TensorNetworkAdd[net, Labeled[tensor, None], index]

TensorNetworkAdd[net_Graph ? TensorNetworkQ, Labeled[tensor_ ? TensorQ, label_ : None], autoIndex : _List | Automatic : Automatic] := Enclose @ With[{
    newVertex = Max[VertexList[net]] + 1,
    toIndex = Replace[autoIndex, Automatic :> Take[SortBy[TensorNetworkFreeIndices[net], Replace[{Superscript[_, x_] :> {1, x}, Subscript[_, x_] :> {0, x}}]], UpTo[TensorRank[tensor]]]]
},
{
    index = Join[
        Replace[toIndex, {Superscript[_, q_] :> Subscript[newVertex, q], Subscript[_, q_] :> Superscript[newVertex, q]}, {1}],
        Subscript[newVertex, #] & /@ Range[TensorRank[tensor] - Length[toIndex]]
    ]
},
    ConfirmAssert[TensorRank[tensor] == Length[index]];
    Annotate[
        {
            EdgeAdd[
                net,
                MapThread[
                    If[MatchQ[#1, _Superscript], DirectedEdge[newVertex, First[#2], {#1, #2}], DirectedEdge[First[#2], newVertex, {#2, #1}]] &,
                    {Take[index, UpTo[Length[toIndex]]], toIndex}
                ]
            ],
            newVertex
        },
        {
            "Tensor" -> tensor,
            "Index" -> index,
            VertexLabels -> label
        }
    ]
]

VertexCompleteGraph[vs_List] := With[{n = Length[vs]}, AdjacencyGraph[vs, SparseArray[Band[{1, 1}] -> 0, {n, n}, 1]]]

TensorNetworkIndexGraph[net_Graph ? (TensorNetworkQ[True]), opts : OptionsPattern[Graph]] := GraphUnion[
    DirectedEdge @@@ EdgeTags[net],
    Sequence @@ (VertexCompleteGraph /@ TensorNetworkIndices[net]),
    opts,
    VertexLabels -> Automatic
]

Options[QuantumTensorNetwork] = Join[{"PrependInitial" -> True, "Computational" -> True}, Options[Graph]]

QuantumTensorNetwork[qco_QuantumCircuitOperator, opts : OptionsPattern[]] := Enclose @ Block[{
    circuit = qco["Sort"], width, min, ops, orders, arity, vertices, edges, tensors
},
	ConfirmAssert[AllTrue[circuit["Operators"], #["Order"] === #["FullOrder"] &]];
    width = circuit["Width"];
    min = circuit["Min"];
    ops = circuit["NormalOperators"];
    If[TrueQ[OptionValue["Computational"]], ops = Through[ops["Computational"]]];
    arity = circuit["Arity"];
    MapThread[
        PrependTo[ops, QuantumOperator[QuantumState[{1}, #2, "Label" -> "0"], {#1}]] &,
        {circuit["InputOrder"], PadLeft[circuit["InputDimensions"], arity, 2]}
    ];
	orders = #["Order"] & /@ ops;
    vertices = Range[Length[ops]] - arity;
	edges = Catenate @ FoldPairList[
		{nprev, order} |-> Block[{output, input, n, prev, next, indices},
            {n, prev} = nprev;
            n += 1;
            next = prev;
			{output, input} = order;
            next[[ output - min + 1 ]] = n;
			next[[ Complement[input, output] - min + 1 ]] = None[n];
            indices = {Superscript[prev[[# - min + 1]], #], Subscript[next[[# - min + 1]], #]} & /@ input;
			{
                Replace[
                    Thread[DirectedEdge[prev[[ input - min + 1 ]], next[[ input - min + 1]], indices]],
                    {
                        DirectedEdge[None[_], ___] :> Nothing,
                        DirectedEdge[from_, None[to_], tag_] :> DirectedEdge[from, to, tag /. None[i_] :> i]
                    },
                    {1}
                ],
                {n, next}
            }
		],
		{1 - arity, Table[1 - arity, width]},
		Rest[orders]
	];
	tensors = If[#["MatrixQ"], #["Double"], #]["Tensor"] & /@ ops;
	ConfirmBy[
        If[TrueQ[OptionValue["PrependInitial"]] && circuit["Arity"] > 0, Identity, VertexDelete[#, _ ? NonPositive] &] @ Graph[
            vertices,
            edges,
            FilterRules[{opts}, Options[Graph]],
            AnnotationRules ->
                MapThread[#1 -> {
                        "Tensor" -> #2,
                        "Index" -> Join[OperatorApplied[Superscript, 2][#1] /@ Sort[#3[[1]]], OperatorApplied[Subscript, 2][#1] /@ Sort @ #3[[2]]]
                    } &,
                    {vertices, tensors, orders}
                ],
            VertexLabels ->
                Thread[vertices -> (Replace[#["Label"], {
                    label : Subscript["C", cop_][__] :> Interpretation[Row[{"C", cop}], label],
                    label : Subscript["R", rops__][angle_] :> Interpretation[Subscript["R", rops][angle], label]
                }] & /@ ops)],
            GraphLayout -> {"LayeredDigraphEmbedding", "Orientation" -> Left}
        ],
        TensorNetworkQ
    ]
]

QuantumCircuitHypergraph[qc_ ? QuantumCircuitOperatorQ, opts : OptionsPattern[]] := Enclose @ Block[{
    net = QuantumTensorNetwork[qc["Flatten"], FilterRules[{opts}, Except[Options[Graph], Options[QuantumTensorNetwork]]]], vs, indices, labels, edges
},
	Confirm[Needs["WolframInstitute`Hypergraph`" -> "H`"], "Hypergraph paclet is not installed."];
	vs = Developer`FromPackedArray @ VertexList[net];
	indices = TensorNetworkIndices[net];
	labels = AnnotationValue[{net, vs}, VertexLabels];
	edges = Replace[indices, Rule @@@ Reverse /@ EdgeTags[net], {2}];
	H`Hypergraph[Union @@ edges, edges, FilterRules[{opts}, Options[H`Hypergraph]], EdgeLabels -> Thread[edges -> labels]]
]


TensorNetworkApply[qco_QuantumCircuitOperator, qs_QuantumState] := Block[{
    circuit = qco["Sort"], res
},
    If[ qs["Qudits"] > 0,
        circuit = {qs -> circuit["InputOrder"]} /* circuit
    ];
    res = TensorNetworkCompile[circuit];
    Which[
        QuantumMeasurementOperatorQ[res],
        If[ContainsAll[res["OutputOrder"], res["TargetOrder"]], QuantumMeasurement[res], res],
        QuantumOperatorQ[res],
        res["State"],
        True,
        res
    ]
]

Options[TensorNetworkCompile] = Options[QuantumTensorNetwork]

TensorNetworkCompile[qco_QuantumCircuitOperator, opts : OptionsPattern[]] := Enclose @ Block[{
    circuit = qco["Normal"], width, net, phaseSpaceQ, bendQ, order, res,
    traceOrder, eigenOrder, basis
},
    width = circuit["Width"];
    basis = Confirm @ circuit["TensorNetworkBasis"];
    phaseSpaceQ = basis["Picture"] === "PhaseSpace";
    traceOrder = circuit["TraceOrder"];
    eigenOrder = circuit["Eigenorder"];
    order = Sort /@ circuit["Order"];
    bendQ = (AnyTrue[circuit["Operators"], #["MatrixQ"] &] || circuit["TraceQudits"] > 0) && ! phaseSpaceQ;
    If[ bendQ,
        (* TODO: handle this case somehow *)
        (* ConfirmAssert[AllTrue[Join[DeleteElements[order[[1]], Join[traceOrder, eigenOrder]], order[[2]]], Positive]]; *)
        circuit = circuit["Bend"];
        If[ circuit["TraceQudits"] > 0,
            circuit = circuit /* MapThread[{"Cap", #2} -> {#1, #1 + width} &, {circuit["TraceOrder"], circuit["TraceDimensions"]}];
        ]
    ];
    net = ConfirmBy[QuantumTensorNetwork[circuit, opts, "PrependInitial" -> False, "Computational" -> ! phaseSpaceQ], TensorNetworkQ];
    res = Confirm @ ContractTensorNetwork[net];
    res = With[{basis = Confirm @ circuit["TensorNetworkBasis"]},
        QuantumState[
            SparseArrayFlatten[res],
            QuantumBasis[QuditBasis[basis["OutputDimensions"]], QuditBasis[basis["InputDimensions"]]]
        ]
    ];
    If[ traceOrder =!= {},
        If[ bendQ,
            basis = QuantumPartialTrace[basis, traceOrder - qco["Min"] + 1],

            res = QuantumPartialTrace[res, traceOrder - qco["Min"] + 1]
        ];
        order = {DeleteElements[order[[1]], traceOrder], order[[2]]};
    ];
    If[ bendQ,
        If[ eigenOrder =!= {},
            order = {Join[Take[Select[order[[1]], NonPositive], - Length[eigenOrder]], Select[order[[1]], Positive]], order[[2]]};
        ];
        res = res["Unbend"]
    ];
    res = If[phaseSpaceQ || ! TrueQ[OptionValue["Computational"]], QuantumState[res["State"], basis], QuantumState[res, basis]];
    res = Which[
        eigenOrder =!= {},
        QuantumMeasurementOperator[QuantumOperator[res, order], qco["Target"]],
        True,
        QuantumOperator[res, order]
    ];
    res
]


Options[FromTensorNetwork] = {Method -> "Random"}

FromTensorNetwork[net_ /; DirectedGraphQ[net] && AcyclicGraphQ[net], OptionsPattern[]] := Enclose @ Block[{
	vs = Developer`FromPackedArray[TopologicalSort[net]],
	labels,
	inputs, outputs, inputOrder, outputOrder,
    orders, output,
	index, outOrders, inOrders
},
	orders = <||>;
	output = {};
	Do[
		inputs = VertexInComponent[net, gate, {1}];
		outputs = VertexOutComponent[net, gate, {1}];
		inputOrder = Lookup[First /@ PositionIndex[output], inputs, Nothing];
		inputOrder = Take[Join[inputOrder, Length[output] + Range[Length[inputs] - Length[inputOrder]]], UpTo[Length[inputs]]];
		If[ Length[outputs] <= Length[inputs],
			outputOrder = Take[inputOrder, Length[outputs]],
			outputOrder = Take[Join[inputOrder, Lookup[PositionIndex[output], None, {}], Length[output] + Range[Length[outputs]]], Length[outputs]]
		];
		output = PadRight[output, Max[Length[output], outputOrder], None];
		output[[Complement[inputOrder, outputOrder]]] = None;
		Scan[(output[[#]] = gate) &, outputOrder];
		orders[gate] = {outputOrder, inputOrder};
		,
		{gate, vs}
	];
	index = AssociationThread[vs, AnnotationValue[{net, vs}, "Index"]];
	outOrders = KeyValueMap[{v, i} |->
		Replace[i, {
			$Failed :> orders[v][[1]],
			indices_List :> Cases[indices, Superscript[_, o_] :> o]
		}],
		index
	];
	inOrders = KeyValueMap[{v, i} |->
		Replace[i, {
			$Failed :> orders[v][[2]],
			indices_List :> Cases[indices, Subscript[_, o_] :> o]
		}],
		index
	];
	labels = KeyValueMap[Replace[#2, {$Failed | Automatic :> #1, Interpretation[_, label_] :> label}] &, AssociationThread[vs, AnnotationValue[{net, vs}, VertexLabels]]];
	QuantumCircuitOperator @ MapIndexed[
		Block[{order = {outOrders[[#2[[1]]]], inOrders[[#2[[1]]]]}, label = labels[[#2[[1]]]], tensor},
            tensor = Replace[#1, {
                    $Failed :> With[{qubits = Total[Length /@ order]}, Switch[
                        OptionValue[Method],
                        "Zero", QuantumState[{"Register", qubits}],
                        _,  QuantumState[{"RandomPure", qubits}]
                    ]],
                    t_ :> ConfirmBy[QuantumState[Flatten[{t}]], QuantumStateQ]}
                ]["SplitDual", Length[order[[1]]]];
			Replace[label, {Subscript["Measurement", target___] :> (QuantumMeasurementOperator[#, {target}] &), _ -> Identity}] @  QuantumOperator[
				tensor,
				order,
				"Label" -> label
			]
		] &,
		AnnotationValue[{net, vs}, "Tensor"]
	]
]

FromTensorNetwork[net_ ? DirectedGraphQ, opts : OptionsPattern[]] :=
    Enclose @ FromTensorNetwork[Confirm @ RemoveTensorNetworkCycles[net], opts]

FromTensorNetwork[net_ ? GraphQ, opts : OptionsPattern[]] := FromTensorNetwork[DirectedGraph[net, "Acyclic"], opts]


RemoveTensorNetworkCycles[inputNet_ ? DirectedGraphQ, opts : OptionsPattern[Graph]] := Enclose @ Block[{
    net = IndexGraph[inputNet], cycles, id, q, r, edge, tag, cup, cap, cupIndex, capIndex, dim
},
	id = Max[VertexList[net], 0] + 1;
	{q, r} = MinMax[EdgeTags[net][[All, All, 2]]];
    q = Min[q, 1];
    r = Max[r, 1];


	While[
        Length[cycles = FindCycle[net, Infinity, 1]] > 0,

        q--;
		edge = cycles[[1, -1]];
        tag = If[Length[edge] == 3 && MatchQ[edge[[3]], {Superscript[_Integer, _Integer], Subscript[_Integer, _Integer]}],
            edge[[3]],
            None
        ];
		cup = id++;
		cap = id++;
		net = EdgeDelete[net, edge];
		net = VertexAdd[net, {cap, cup}];
        If[ tag =!= None,

            cupIndex = {Superscript[cup, q], Superscript[cup, tag[[2, 2]]]};
            capIndex = {Subscript[cap, q], Subscript[cap, tag[[1, 2]]]};
            net = EdgeAdd[net, {
                DirectedEdge[cup, cap, {cupIndex[[1]], capIndex[[1]]}],
                DirectedEdge[cup, edge[[2]], {cupIndex[[2]], tag[[2]]}],
                DirectedEdge[edge[[1]], cap, {tag[[1]], capIndex[[2]]}]
            }];
            dim = Enclose[
                ConfirmBy[
                    Dimensions[Confirm[AnnotationValue[{net, edge[[1]]}, "Tensor"]]][[ Confirm @ Lookup[PositionIndex[AnnotationValue[{net, edge[[1]]}, "Index"]], tag[[1]], $Failed, First] ]],
                    IntegerQ
                ],
                2 &
            ];
            net = Annotate[{net, cup}, "Index" -> cupIndex];
		    net = Annotate[{net, cap}, "Index" -> capIndex];

            ,

            net = EdgeAdd[net, {
                DirectedEdge[cup, cap],
                DirectedEdge[cup, edge[[2]]],
                DirectedEdge[edge[[1]], cap]
            }];
            dim = Enclose[First[Dimensions[Confirm[AnnotationValue[{net, edge[[1]]}, "Tensor"]]], 1], 2 &]
        ];

		net = Annotate[{net, cup}, {"Tensor" -> QuantumOperator["Cup"[dim]]["Tensor"], VertexLabels -> "Cup"}];
		net = Annotate[{net, cap}, {"Tensor" -> QuantumOperator["Cap"[dim]]["Tensor"], VertexLabels -> "Cap"}];
	];
	Graph[net, opts]
]



Options[TensorNetworkContractionPath] = {"ReturnParameters" -> False, "Optimal" -> False}

TensorNetworkContractionPath[net_ ? TensorNetworkQ, OptionsPattern[]] := Enclose @ Block[{
	tensors, indices, pairs, freeIndices, normalIndices, input, output, dimensions
},
	tensors = TensorNetworkTensors[net];
    indices = TensorNetworkIndices[net];
	pairs = EdgeTags[net];
	freeIndices = TensorNetworkFreeIndices[net];
	dimensions = AssociationThread[Catenate[indices], Catenate[Dimensions /@ tensors]];
	ConfirmAssert[AllTrue[Partition[Lookup[dimensions, Catenate[pairs]], 2], Apply[Equal]]];
	pairs = Rule @@@ pairs;
	dimensions = KeyMap[Replace[pairs], dimensions];
	indices = Replace[indices, pairs, {2}];
	normalIndices = Thread[# -> ToString /@ Range[Length[#]]] & [Union @@ indices];
	input = Replace[indices, normalIndices, {2}];
	output = Replace[freeIndices, normalIndices, {1}];
	dimensions = KeyMap[Replace[normalIndices], dimensions];
	If[TrueQ[OptionValue["ReturnParameters"]], Return[{input, output, dimensions}]];
	If[ TrueQ[OptionValue["Optimal"]],
        OptimalPath[input, output, dimensions, "size"],
        GreedyPath[input, output, dimensions]
    ]
]


einsum[{i_, j_} -> k_, a_, b_] := Block[{c = Complement[Join[i, j], k], adim = Dimensions[a], bdim = Dimensions[b], al, br, ac, bc, ad, bd},
	ac = Catenate @ Lookup[PositionIndex[i], c];
	bc = Catenate @ Lookup[PositionIndex[j], c];
	al = Complement[Range[Length[i]], ac];
	br = Complement[Range[Length[j]], bc];
	ad = Times @@ adim[[ac]];
	bd = Times @@ bdim[[bc]];
	Transpose[
		ArrayReshape[
			ArrayReshape[Transpose[a, FindPermutation[Join[al, ac]]], {Times @@ adim / ad, ad}] . ArrayReshape[Transpose[b, FindPermutation[Join[bc, br]]], {bd, Times @@ bdim / bd}],
			Join[adim[[al]], bdim[[br]]]
		],
		FindPermutation[Join[i[[al]], j[[br]]], k]
	]
]

TensorNetworkContractPath[net_ ? TensorNetworkQ, path_] := Enclose @ Block[{tensors, indices, freeIndices},
    tensors = TensorNetworkTensors[net];
    indices = TensorNetworkIndices[net];
    freeIndices = TensorNetworkFreeIndices[net];
    indices = Replace[indices, Rule @@@ EdgeTags[net], {2}];
    Do[
		Replace[p, {
			{i_} :> (
				{tensors, indices} = Append[Delete[#, {i}], #[[i]]] & /@ {tensors, indices}
			),
			{i_, j_} :> Block[{out, tensor},
				out = SymmetricDifference @@ Extract[indices, {{i}, {j}}];
                tensor = EinsteinSummation[indices[[{i, j}]] -> out, {tensors[[i]], tensors[[j]]}];
				(* tensor = einsum[indices[[{i, j}]] -> out, tensors[[i]], tensors[[j]]]; *)
				tensors = Append[Delete[tensors, {{i}, {j}}], tensor];
				indices = Append[Delete[indices, {{i}, {j}}], out]
			]
		}],
		{p, path}
	];
	ConfirmAssert[Length[tensors] == Length[indices] == 1];
	ConfirmAssert[ContainsAll[indices[[1]], freeIndices]];
	Transpose[tensors[[1]], FindPermutation[indices[[1]], freeIndices]]
]


TensorNetworkNetGraph[net_ ? TensorNetworkQ] := TensorNetworkNetGraph[net, TensorNetworkContractionPath[net]]

TensorNetworkNetGraph[net_ ? TensorNetworkQ, path_] := Enclose @ Block[{tensors, indices, freeIndices, g, tensorQueue, addEinsumLayer, n},
    tensors = NumericArray[N[#]] & /@ TensorNetworkTensors[net];
    indices = TensorNetworkIndices[net];
    freeIndices = TensorNetworkFreeIndices[net];
    indices = Replace[indices, Rule @@@ EdgeTags[net], {2}];
    n = Length[tensors];
    g = NetGraph[NetArrayLayer["Array" -> #] & /@ tensors, # -> NetPort["T" <> ToString[#]] & /@ Range[n]];
    tensorQueue = "T" <> ToString[#] & /@ Range[n];
    addEinsumLayer[{i_, j_} -> k_, a_, b_] :=
		With[{
			c = Complement[Join[i, j], k],
			adim = Flatten[{NetExtract[g, a]}],
			bdim = Flatten[{NetExtract[g, b]}]
	    },
		With[{
			ac = Catenate @ Lookup[PositionIndex[i], c],
			bc = Catenate @ Lookup[PositionIndex[j], c]
		},
		With[{
			al = Complement[Range[Length[i]], ac],
			br = Complement[Range[Length[j]], bc],
			ad = Times @@ adim[[ac]],
			bd = Times @@ bdim[[bc]]
		},
		With[{
			reshapea = {Times @@ adim / ad, ad},
			reshapeb = {bd, Times @@ bdim / bd},
			perma = PermutationList @ FindPermutation[Join[al, ac]],
			permb = PermutationList @ FindPermutation[Join[bc, br]],
			reshape = Join[adim[[al]], bdim[[br]]],
			perm = PermutationList @ FindPermutation[Join[i[[al]], j[[br]]], k]
		},
		n++;
		g = Confirm @ NetGraph[
			{
				g,
				Confirm @ NetGraph[<|
                    (*ToString[n] -> FunctionLayer[Apply[
                        Transpose[
                            ArrayReshape[
                                ArrayReshape[Transpose[#1, perma], reshapea] . ArrayReshape[Transpose[#2, permb], reshapeb],
                                reshape
                            ],
                            perm
                        ] &
                    ]]*)
					"a" -> NetChain[{If[perma === {}, Nothing, TransposeLayer[perma]], ReshapeLayer[reshapea]}],
					"b" -> NetChain[{If[permb === {}, Nothing, TransposeLayer[permb]], ReshapeLayer[reshapeb]}],
					"dot" -> DotLayer[],
					"post" -> NetChain[{ReshapeLayer[reshape], If[perm === {}, Nothing, TransposeLayer[perm]]}]
				|>, {NetPort[a] -> "a", NetPort[b] -> "b", {"a", "b"} -> "dot" -> "post" -> NetPort["T" <> ToString[n]]}]
			},
			{NetPort[{1, a}] -> NetPort[{2, a}], NetPort[{1, b}] -> NetPort[{2, b}]}
		];
	]]]];
    Do[
		Replace[p, {
			{i_} :> (
				{tensorQueue, indices} = Append[Delete[#, {i}], #[[i]]] & /@ {tensorQueue, indices}
			),
			{i_, j_} :> With[{out = SymmetricDifference @@ Extract[indices, {{i}, {j}}]},
				addEinsumLayer[indices[[{i, j}]] -> out, tensorQueue[[i]], tensorQueue[[j]]];
				tensorQueue = Append[Delete[tensorQueue, {{i}, {j}}], "T" <> ToString[n]];
				indices = Append[Delete[indices, {{i}, {j}}], out];
			]
		}],
		{p, path}
	];
	ConfirmAssert[Length[tensorQueue] == Length[indices] == 1];
	ConfirmAssert[ContainsAll[indices[[1]], freeIndices]];
	With[{perm = PermutationList @ FindPermutation[indices[[1]], freeIndices]},
		NetFlatten @ If[perm === {}, g, NetAppend[g, TransposeLayer[perm]]]
	]
]

