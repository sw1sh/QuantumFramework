Package["Wolfram`QuantumFramework`"]

PackageExport["TensorNetworkIndexGraph"]
PackageExport["FromTensorNetwork"]
PackageExport["TensorNetworkQ"]
PackageExport["ContractTensorNetwork"]
PackageExport["TensorNetworkFreeIndices"]

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
    Block[{indices, tensors, scalarPositions, scalars},
        indices = AnnotationValue[{net, Developer`FromPackedArray[VertexList[net]]}, "Index"] /. Rule @@@ EdgeTags[net];
        tensors =  AnnotationValue[{net, Developer`FromPackedArray[VertexList[net]]}, "Tensor"];
        scalarPositions = Position[indices, {}, {1}, Heads -> False];
        scalars = Extract[tensors, scalarPositions];
        indices = Delete[indices, scalarPositions];
        tensors = Delete[tensors, scalarPositions];
        Times @@ scalars * ConfirmQuiet[ResourceFunction["EinsteinSummation"][indices -> TensorNetworkFreeIndices[net], tensors]]
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


TensorNetworkFreeIndices[net_Graph ? TensorNetworkQ] :=
    SortBy[Last] @ DeleteCases[Join @@ AnnotationValue[{net, Developer`FromPackedArray[VertexList[net]]}, "Index"], Alternatives @@ Union @@ EdgeTags[net]]

InitializeTensorNetwork[net_Graph ? TensorNetworkQ, tensor_ ? TensorQ, index_List : Automatic] := Annotate[
    {net, 0},
    {
        "Tensor" -> tensor,
        "Index" -> Replace[index, Automatic :> With[{
                oldIndex = AnnotationValue[{net, 0}, "Index"]
            },
                Join[oldIndex, Take[Subscript @@@ oldIndex, UpTo[Max[0, TensorRank[tensor] - Length[oldIndex]]]]]
            ]
        ]
    }
]

VertexCompleteGraph[vs_List] := With[{n = Length[vs]}, AdjacencyGraph[vs, SparseArray[Band[{1, 1}] -> 0, {n, n}, 1]]]

TensorNetworkIndexGraph[net_Graph ? (TensorNetworkQ[True]), opts : OptionsPattern[Graph]] := GraphUnion[
    DirectedEdge @@@ EdgeTags[net],
    Sequence @@ (VertexCompleteGraph /@ AnnotationValue[{net, Developer`FromPackedArray[VertexList[net]]}, "Index"]),
    opts,
    VertexLabels -> Automatic
]

Options[QuantumTensorNetwork] = Join[{"PrependInitial" -> True}, Options[Graph]]

QuantumTensorNetwork[qc_QuantumCircuitOperator, opts : OptionsPattern[]] := Enclose @ Block[{
    width, min, ops, orders, vertices, edges, tensors
},
	ConfirmAssert[AllTrue[qc["Operators"], #["Order"] === #["FullOrder"] &]];
    width = qc["Width"];
    min = qc["Min"];
    ops = Through[qc["NormalOperators"]["Computational"]];
    PrependTo[ops, QuantumOperator[QuantumState[{1}, PadLeft[qc["InputDimensions"], qc["Arity"], 2], "Label" -> "Initial"], qc["InputOrder"]]];
	orders = #["Order"] & /@ ops;
    vertices = Range[Length[ops]] - 1;
	edges = Catenate @ FoldPairList[
		{nprev, order} |-> Block[{output, input, n, prev, next, indices},
            {n, prev} = nprev;
            n += 1;
            next = prev;
			{output, input} = order;
			next[[ Union[output, input] - min + 1 ]] = n;
            indices = {Superscript[prev[[# - min + 1]], #], Subscript[next[[# - min + 1]], #]} & /@ input;
			{Thread[DirectedEdge[prev[[ input - min + 1 ]], next[[ input - min + 1]], indices]], {n, next}}
		],
		{0, Table[0, width]},
		Rest @ orders
	];
	tensors = #["Tensor"] & /@ ops;
	ConfirmBy[
        If[TrueQ[OptionValue["PrependInitial"]] && qc["Arity"] > 0, Identity, VertexDelete[#, 0] &] @ Graph[
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
            GraphLayout -> "SpringElectricalEmbedding"
        ],
        TensorNetworkQ
    ]
]

QuantumCircuitHypergraph[qc_ ? QuantumCircuitOperatorQ, opts : OptionsPattern[]] := Enclose @ Block[{
    net = qc["TensorNetwork"], vs, indices, labels, edges
},
	Confirm @ Needs["WolframInstitute`Hypergraph`"];
	vs = Developer`FromPackedArray @ VertexList[net];
	indices = AnnotationValue[{net, vs}, "Index"];
	labels = AnnotationValue[{net, vs}, VertexLabels];
	edges = Replace[indices, Rule @@@ EdgeTags[net], {2}];
	WolframInstitute`Hypergraph`Hypergraph[edges, opts, EdgeLabels -> Thread[edges -> labels]]
]


TensorNetworkApply[qco_QuantumCircuitOperator, qs_QuantumState] := Block[{
    circuit, bendQ, res
},
    circuit = If[ qs["Qudits"] > 0,
        QuantumCircuitOperator[Prepend[qs] @ qco["Operators"]],
        qco
    ];
    res = TensorNetworkCompile[circuit];
    Which[
        QuantumMeasurementOperatorQ[res],
        QuantumMeasurement[res],
        QuantumOperatorQ[res],
        res["State"],
        True,
        res
    ]
]


TensorNetworkCompile[qco_QuantumCircuitOperator, OptionsPattern[]] := Enclose @ Block[{
    circuit = qco, net, bendQ, transpose, order, res,
    traceOrder, eigenOrder
},
    traceOrder = circuit["TraceOrder"];
    eigenOrder = circuit["Eigenorder"];
    order = Sort /@ circuit["Order"];
    bendQ = AnyTrue[circuit["Operators"], #["MatrixQ"] &];
    If[ bendQ,
        (* TODO: handle this case somehow *)
        ConfirmAssert[AllTrue[Join[DeleteElements[order[[1]], Join[traceOrder, eigenOrder]], order[[2]]], Positive]];
        circuit = circuit["Bend"];
        If[ circuit["TraceQudits"] > 0,
            circuit = circuit /* ({"Cap", #[[1, 2]]} -> #[[All, 1]] & /@ Partition[Thread[{circuit["TraceOrder"], circuit["TraceDimensions"]}], 2])
        ]
    ];
    net = ConfirmBy[circuit["TensorNetwork", "PrependInitial" -> False], TensorNetworkQ];
    order = {DeleteElements[order[[1]], traceOrder], order[[2]]};
    transpose = Ordering @ OrderingBy[TensorNetworkFreeIndices[net], Replace[{Superscript[_, x_] :> {0, x}, Subscript[_, x_] :> {1, x}}]];
    res = Confirm @ ContractTensorNetwork[net];
    If[ transpose =!= {},
        res = Transpose[res, transpose]
    ];
    res = QuantumState[
        SparseArrayFlatten[res],
        circuit["Basis"]
    ];
    If[ traceOrder =!= {} && ! bendQ,
        res = QuantumPartialTrace[res, traceOrder - circuit["Min"] + 1]
    ];
    If[ bendQ,
        If[ eigenOrder =!= {},
            res = res["PermuteOutput", InversePermutation @ FindPermutation[
                Catenate @ MapIndexed[If[MemberQ[eigenOrder, #], {#, Max[order[[1]]] + #2[[1]]}, {#}] &, order[[1]]]
            ]];
        ];
        res = res["Unbend"]
    ];

    res = If[ eigenOrder =!= {},
        QuantumMeasurementOperator[QuantumOperator[res, order], qco["Target"]],
        QuantumOperator[res, order]
    ];
    res
]


FromTensorNetwork[net_ /; DirectedGraphQ[net] && AcyclicGraphQ[net]] := Block[{
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
		With[{order = {outOrders[[#2[[1]]]], inOrders[[#2[[1]]]]}, label = labels[[#2[[1]]]]},
			QuantumOperator[
				Replace[#1, {$Failed :> QuantumState[{"Register", Total[Length /@ order]}], tensor_ :> QuantumState[Flatten[tensor]]}]["Split", Length[order[[1]]]],
				order,
				"Label" -> label
			]
		] &,
		AnnotationValue[{net, vs}, "Tensor"]
	]
]

