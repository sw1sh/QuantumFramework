Package["Wolfram`QuantumFramework`"]

PackageExport["ContractTensorNetwork"]

PackageScope["QuantumTensorNetwork"]



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

edgeContractWithIndex[g_, edge : DirectedEdge[from_, to_, {i_, j_}]] := Annotate[
	{edgeContract[g, edge], to},
	"Index" -> If[
		from === to,
		DeleteCases[AnnotationValue[{g, to}, "Index"], i | j],
		DeleteCases[Join[AnnotationValue[{g, from}, "Index"], AnnotationValue[{g, to}, "Index"]], i | j]
	]
]

ContractEdge[g_, edge : DirectedEdge[from_, to_, {i_, j_}]] := Enclose @ Module[{
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

ContractEdge[g_, edge : DirectedEdge[v_, v_, {i_, j_}]] := Enclose @ Module[{
	tensor = ConfirmBy[AnnotationValue[{g, v}, "Tensor"], TensorQ],
	index = Confirm[AnnotationValue[{g, v}, "Index"]]
},
	Annotate[{edgeContractWithIndex[g, edge], v}, "Tensor" -> TensorContract[tensor, Catenate @ Position[index, i | j]]]
]


ContractTensorNetwork[net_Graph] := Enclose @ Module[{g, edges},
	{g, {edges}} = Reap @ NestWhile[Confirm @ ContractEdge[#, Sow @ First[EdgeList[#]]] &, net, EdgeCount[#] > 0 &];
	Transpose[
        AnnotationValue[{g, edges[[-1, 2]]}, "Tensor"],
        InversePermutation @ FindPermutation @ AnnotationValue[{g, edges[[-1, 2]]}, "Index"][[All, 1]]
    ]
]


Options[QuantumTensorNetwork] = Options[Graph]

QuantumTensorNetwork[qc_QuantumCircuitOperator, opts : OptionsPattern[]] := Enclose @ Module[{width, ops, size, orders, vertices, edges, tensors},
	ConfirmAssert[AllTrue[qc["Operators"], #["Order"] === #["FullOrder"] &]];
    width = qc["Width"] + qc["Targets"];
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
	orders = #["Order"] + qc["Targets"] & /@ ops;
    vertices = Range[Length[ops]];
	edges = Catenate @ FoldPairList[
		{prev, order} |-> Module[{output, input, n = Max[prev] + 1, next = prev, indices},
			{output, input} = order;
			next[[ input ]] = n;
            indices = {prev[[#]][#], next[[#]][-#]} & /@ input;
			{Thread[DirectedEdge[prev[[ input ]], next[[ input ]], indices]], next}
		],
		Table[1, width],
		Rest @ orders
	];
	tensors = #["Tensor"] & /@ ops;
	Graph[
        vertices,
		edges,
        opts,
		AnnotationRules ->
            MapThread[#1 -> {"Tensor" -> #2, "Index" -> #1 /@ Join[Sort @ #3[[1]], - Sort @ #3[[2]]]} &, {vertices, tensors, orders}],
		VertexLabels ->
            Thread[vertices -> (Replace[#["Label"], "Controlled"[label_, ___] :> Row[{"C", label}]] & /@ ops)],
        GraphLayout -> "SpringElectricalEmbedding"
	]
]

