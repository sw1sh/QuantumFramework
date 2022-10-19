Package["Wolfram`QuantumFramework`"]

PackageExport["TensorNetworkQ"]
PackageExport["ContractTensorNetwork"]
PackageExport["TensorNetworkFreeIndices"]
PackageExport["InitializeTensorNetwork"]
PackageExport["TensorNetworkIndexGraph"]
PackageExport["TensorNetworkApply"]
PackageExport["TensorNetworkCompile"]

PackageScope["QuantumTensorNetwork"]



TensorNetworkQ::msg1 = "Not all vertices are annotated with tensors."
TensorNetworkQ::msg2 = "Not all indices are duplicate free lists of Subscripts and Superscripts."
TensorNetworkQ::msg3 = "Tensor ranks and indices are not compatible."
TensorNetworkQ::msg4 = "Not all edges are tagged with indices."

TensorNetworkQ[net_Graph, verbose : _ ? BooleanQ : False] := Module[{
    tensors, indices
},
    {tensors, indices} = AssociationThread[VertexList[net] -> #] & /@
        (AnnotationValue[{net, Developer`FromPackedArray[VertexList[net]]}, #] & /@ {"Tensor", "Index"});
    (
        AllTrue[tensors, TensorQ] ||
        (If[verbose, Message[TensorNetworkQ::msg1]]; False)
    ) &&
    (
        AllTrue[indices, MatchQ[#, {(Superscript | Subscript)[_, _] ...}] && DuplicateFreeQ[#] &] ||
        (If[verbose, Message[TensorNetworkQ::msg2]]; False)
     ) &&
    With[{ranks = TensorRank /@ Values[tensors]},
        (And @@ Thread[ranks == Length /@ Values[indices]] && Total[ranks] == CountDistinct[Catenate[Values[indices]]]) ||
        (If[verbose, Message[TensorNetworkQ::msg3]]; False)
    ] &&
    (
        AllTrue[
            EdgeList[net],
            MatchQ[
                _[from_, to_, {i_, j_}] /;
                MemberQ[indices[from], i] && MemberQ[indices[to], j]
            ]
        ] ||
        (If[verbose, Message[TensorNetworkQ::msg4]]; False)
    )
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

FastContractTensorNetwork[net_Graph] := ResourceFunction["EinsteinSummation"][
    (AnnotationValue[{net, Developer`FromPackedArray[VertexList[net]]}, "Index"] /. Rule @@@ EdgeTags[net]) -> TensorNetworkFreeIndices[net],
    AnnotationValue[{net, Developer`FromPackedArray[VertexList[net]]}, "Tensor"]
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
        "Index" -> Replace[index, Automatic -> With[{
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

Options[QuantumTensorNetwork] = Options[Graph]

QuantumTensorNetwork[qc_QuantumCircuitOperator, opts : OptionsPattern[]] := Enclose @ Module[{width, targets, ops, size, orders, vertices, edges, tensors},
	ConfirmAssert[AllTrue[qc["Operators"], #["Order"] === #["FullOrder"] &]];
    targets = qc["Targets"];
    width = qc["Width"];
    ops = Prepend[QuantumOperator[QuantumState[{1}, PadLeft[qc["InputDimensions"], qc["Width"], 2], "Label" -> "Initial"], Range @ qc["Width"]]] @
        Module[{m = - qc["Eigenqudits"] - qc["TraceQudits"] + 1},
            Map[
                Which[
                    QuantumMeasurementOperatorQ[#],
                    With[{povm = #["POVM"]},
                        QuantumOperator[
                            povm["QuantumOperator"],
                            {Join[Reverse @ Table[m++, povm["Eigenqudits"]], Drop[povm["OutputOrder"], povm["Eigenqudits"]]], povm["InputOrder"]},
                            "Label" -> "Measurement"
                        ]
                    ],
                    QuantumChannelQ[#],
                    With[{op = #["QuantumOperator"]},
                        QuantumOperator[
                            op,
                            {Join[Reverse @ Table[m++, #["TraceQudits"]], Drop[op["OutputOrder"], #["TraceQudits"]]], op["InputOrder"]}
                        ]
                    ],
                    True,
                    #
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
                Thread[vertices -> (Replace[#["Label"], {
                    Subscript["C", label_][__] :> Row[{"C", label}],
                    Subscript["R", rops__][angle_] :> Subscript["R", rops][angle]
                }] & /@ ops)],
            GraphLayout -> "SpringElectricalEmbedding"
        ],
        TensorNetworkQ
    ]
]


TensorNetworkApply[qco_QuantumCircuitOperator, qs_QuantumState] := Block[{
    state = If[
        qs["PureStateQ"],
        QuantumState[SparseArrayFlatten[#], TensorDimensions[#], "Label" -> qs["Label"] /* qco["Label"]] & @
            ContractTensorNetwork @ InitializeTensorNetwork[
                qco["TensorNetwork"],
                qs["Computational"]["Tensor"],
                Join[Superscript[0, #] & /@ qco["InputOrder"], Subscript[0, #] & /@ (Range[qs["InputQudits"]])]
            ],
        QuantumState[
            quantumCircuitApply[QuantumTensorProduct[qco, qco["Conjugate"]], qs["Bend"]]["State"]["PermuteOutput",
                With[{a = qco["Eigenqudits"] + qco["TraceQudits"], b = qs["Qudits"]},
                    FindPermutation[
                        Join[Array[0, a], Array[1, a], Array[2, b], Array[3, b]],
                        Join[Array[0, a], Array[2, b], Array[1, a], Array[3, b]]
                    ]
                ]]["Unbend"],
            "Label" -> qs["Label"] /* qco["Label"]
        ]
    ]
},
    If[ qco["Channels"] > 0,
        state = QuantumPartialTrace[state,
            First @ Fold[
                {
                    Join[#1[[1]], If[QuantumChannelQ[#2], #1[[2]] + Range[#2["TraceQudits"]], {}]],
                        #1[[2]] + Which[QuantumChannelQ[#2], #2["TraceQudits"], QuantumMeasurementOperatorQ[#2], #2["Eigenqudits"], True, 0]
                } &,
                {{}, 0},
                qco["Operators"]
            ]
        ]
    ];
    If[ qco["Measurements"] > 0,
        QuantumMeasurement[
            QuantumMeasurementOperator[
                QuantumOperator[
                    state,
                    {
                        Join[Range[- qco["Eigenqudits"] + 1, 0], DeleteCases[qco["OutputOrder"], _ ? NonPositive]],
                        qco["InputOrder"]
                    }
                ],
                qco["Target"]
            ],
            Fold[
                ReverseApplied[Construct],
                Prepend[
                    Select[qco["Operators"] , QuantumMeasurementOperatorQ],
                    If[Length[#] > 0, QuantumOperator["I", #], Nothing] & @ Complement[Range[qco["Arity"]], qco["Target"]]
                ]
            ]["POVM"]["Sort"]["OutputBasis"]
        ],
        state
    ]
]


TensorNetworkCompile[qco_QuantumCircuitOperator] :=
    Which[
        qco["Eigenqudits"] > 0,
        QuantumMeasurementOperator[QuantumOperator[#, {Join[Range[- qco["Eigenqudits"] + 1, 0], qco["OutputOrder"]], qco["InputOrder"]}], qco["Target"]] &,
        qco["TraceQudits"] > 0,
        QuantumChannel[QuantumOperator[#, qco["Order"]]] &,
        True,
        QuantumOperator[#, qco["Order"]] &
    ] @ QuantumState[SparseArrayFlatten[#], QuantumBasis @@ Reverse @ TakeDrop[TensorDimensions[#], - qco["Arity"]], "Label" -> qco["Label"]] & @
    With[{tn = VertexDelete[qco["TensorNetwork"], 0]},
        Transpose[ContractTensorNetwork[tn], Ordering @ OrderingBy[TensorNetworkFreeIndices[tn], Replace[{Superscript[_, x_] :> {0, x}, Subscript[_, x_] :> {1, x}}]]]
    ]

