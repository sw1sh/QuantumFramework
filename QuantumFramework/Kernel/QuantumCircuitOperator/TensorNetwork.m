Package["Wolfram`QuantumFramework`"]

PackageExport["TensorNetworkQ"]
PackageExport["ContractTensorNetwork"]
PackageExport["TensorNetworkFreeIndices"]
PackageExport["InitializeTensorNetwork"]
PackageExport["TensorNetworkIndexGraph"]

PackageScope["QuantumTensorNetwork"]



TensorNetworkQ[net_Graph] := Module[{
    tensors, indices
},
    {tensors, indices} = AnnotationValue[{net, Developer`FromPackedArray[VertexList[net]]}, #] & /@ {"Tensor", "Index"};
    AllTrue[tensors, TensorQ] &&
    AllTrue[indices, MatchQ[#, {(Superscript | Subscript)[_, _] ...}] && DuplicateFreeQ[#] &] &&
    With[{ranks = TensorRank /@ tensors},
        And @@ Thread[ranks == Length /@ indices] && Total[ranks] == CountDistinct[Catenate[indices]]
    ] &&
    AllTrue[
        EdgeList[net],
        MatchQ[
            _[from_, to_, {i_, j_}] /;
            MemberQ[indices[[VertexIndex[net, from]]], i] && MemberQ[indices[[VertexIndex[net, to]]], j]
        ]
    ]
]

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

FastContractTensorNetwork[net_Graph] := ResourceFunction["EinsteinSummation"][
    (AnnotationValue[{net, Developer`FromPackedArray[VertexList[net]]}, "Index"] /. Rule @@@ EdgeTags[net]) -> TensorNetworkFreeIndices[net],
    AnnotationValue[{net, Developer`FromPackedArray[VertexList[net]]}, "Tensor"]
]

Options[ContractTensorNetwork] = {Method -> Automatic}

ContractTensorNetwork[net_Graph ? TensorNetworkQ, OptionsPattern[]] := Switch[
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
        "Index" -> Replace[index, Automatic -> With[{
                oldIndex = AnnotationValue[{net, 0}, "Index"]
            },
                Join[oldIndex, Take[Subscript @@@ oldIndex, UpTo[Max[0, TensorRank[tensor] - Length[oldIndex]]]]]
            ]
        ]
    }
]

VertexCompleteGraph[vs_List] := With[{n = Length[vs]}, AdjacencyGraph[vs, SparseArray[Band[{1, 1}] -> 0, {n, n}, 1]]]

TensorNetworkIndexGraph[net_Graph ? TensorNetworkQ, opts : OptionsPattern[Graph]] := GraphUnion[
    DirectedEdge @@@ EdgeTags[net],
    Sequence @@ (VertexCompleteGraph /@ AnnotationValue[{net, Developer`FromPackedArray[VertexList[net]]}, "Index"]),
    opts,
    VertexLabels -> Automatic
]

Options[QuantumTensorNetwork] = Options[Graph]

QuantumTensorNetwork[qc_QuantumCircuitOperator, opts : OptionsPattern[]] := Enclose @ Module[{width, targets, ops, size, orders, vertices, edges, tensors},
	ConfirmAssert[AllTrue[qc["Operators"], #["Order"] === #["FullOrder"] &]];
    targets = qc["Targets"];
    width = qc["Width"];
    ops = Prepend[QuantumOperator[QuantumState[{1}, qc["InputDimensions"], "Label" -> "Initial"], qc["InputOrder"]]] @
        Module[{m = - qc["Targets"] + 1},
            Map[
                If[ !QuantumMeasurementOperatorQ[#],
                    #,
                    With[{povm = #["POVM"]},
                        QuantumOperator[
                            povm["QuantumOperator"],
                            {Join[Reverse @ Table[m++, povm["Eigenqudits"]], Drop[povm["OutputOrder"], povm["Eigenqudits"]]], povm["InputOrder"]},
                            "Label" -> "Measurement"
                        ]
                    ]
                ]["Computational"] &,
                qc["Operators"]
            ]
        ];
	size = Length[ops];
	orders = #["Order"] & /@ ops;
    vertices = Range[Length[ops]] - 1;
	edges = Catenate @ FoldPairList[
		{prev, order} |-> Module[{output, input, n = Max[prev] + 1, next = prev, indices},
			{output, input} = order;
			next[[ input + targets ]] = n;
            indices = {Superscript[prev[[# + targets]], #], Subscript[next[[# + targets]], #]} & /@ input;
			{Thread[DirectedEdge[prev[[ input + targets ]], next[[ input + targets ]], indices]], next}
		],
		Table[0, width + targets],
		Rest @ orders
	];
	tensors = #["Tensor"] & /@ ops;
	ConfirmBy[
        Graph[
            vertices,
            edges,
            opts,
            AnnotationRules ->
                MapThread[#1 -> {
                        "Tensor" -> #2,
                        "Index" -> Join[OperatorApplied[Superscript, 2][#1] /@ Sort[#3[[1]]], OperatorApplied[Subscript, 2][#1] /@ Sort @ #3[[2]]]
                    } &,
                    {vertices, tensors, orders}
                ],
            VertexLabels ->
                Thread[vertices -> (Replace[#["Label"], "Controlled"[label_, ___] :> Row[{"C", label}]] & /@ ops)],
            GraphLayout -> "SpringElectricalEmbedding"
        ],
        TensorNetworkQ
    ]
]

