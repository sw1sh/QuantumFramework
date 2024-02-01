Package["Wolfram`QuantumFramework`"]

PackageScope[ZXTensorNetwork]



Options[toZX] = {"Simplify" -> False, "Fuse" -> False}

toZX[op_, OptionsPattern[]] := With[{qasm = QuantumCircuitOperator[op]["Qiskit"]["QASM"]},
ExternalEvaluate[$PythonSession, StringTemplate["
import pyzx
from pyzx import draw, Circuit
from pyzx.utils import VertexType, EdgeType

circuit = Circuit.from_qasm('''`1`''')
graph = circuit.to_graph()

if `2`:
	pyzx.simplify.full_reduce(graph)
if `3`:
	pyzx.spider_simp(graph)

def vertex_type(t):
	match t:
		case VertexType.BOUNDARY:
			return 'B'
		case VertexType.Z:
			return 'Z'
		case VertexType.X:
			return 'X'
		case _:
			return 'Unknown'
def edge_type(t):
	match t:
		case EdgeType.HADAMARD:
			return 'H'
		case EdgeType.SIMPLE:
			return 'S'
		case _:
			return 'Unknown'
edges = graph.edges()
vertices = {v: vertex_type(t) for v, t in graph.types().items()}
edge_types = {e: edge_type(graph.edge_type(e)) for e in graph.edges()}
phases = graph.phases()
qubits = graph.qubits()

(edges, vertices, edge_types, phases, qubits)

"][qasm, TrueQ[OptionValue["Simplify"]], TrueQ[OptionValue["Fuse"]]]]
]


Options[ZXTensorNetwork] = Join[Options[toZX], Options[Graph]]

ZXTensorNetwork[qc_, opts : OptionsPattern[]] := Enclose @ Block[{edges, types, edgeTypes, phases, qubits, taggedEdges, annotations},
	{edges, types, edgeTypes, phases, qubits} = Confirm @ toZX[qc, FilterRules[{opts}, Options[toZX]]];
	taggedEdges = Flatten @ Block[{h = 0},
		FoldPairList[
			With[{
				out = <|#1[[1]], #2[[1]] -> Lookup[#1[[1]], #2[[1]], 0] + 1|>, in = <|#1[[2]], #2[[2]] -> Lookup[#1[[2]], #2[[2]], 0] + 1|>
			},
				{
					Switch[Lookup[edgeTypes, Key[#2]],
						"S", DirectedEdge[#2[[1]], #2[[2]], {Superscript[#2[[1]], Lookup[out, #2[[1]]]], Subscript[#2[[2]], Lookup[in, #2[[2]]]]}],
						"H",{
							DirectedEdge[#2[[1]], \[FormalCapitalH][++h], {Superscript[#2[[1]], Lookup[out, #2[[1]]]], Subscript[\[FormalCapitalH][h], Lookup[in, #2[[2]]]]}],
							DirectedEdge[\[FormalCapitalH][h], #2[[2]], {Superscript[\[FormalCapitalH][h], Lookup[out, #2[[1]]]], Subscript[#2[[2]], Lookup[in, #2[[2]]]]}]
						},
						_, {}
					],
					{out, in}
				}
			]&,
			ConstantArray[Association[qubits], 2],
			edges
		]
	];
	types = Join[types, AssociationThread[Cases[VertexList[taggedEdges], \[FormalCapitalH][_]] -> "H"]];
	annotations = Map[id |-> With[{
		inputs = Cases[taggedEdges, DirectedEdge[_, id, {_, in_}] :> in],
		outputs = Cases[taggedEdges, DirectedEdge[id, _, {out_, _}] :> out],
		label = Replace[Lookup[types, id], {"Z" -> "ZSpider"[Pi Lookup[phases, id]], "X" -> "XSpider"[Pi Lookup[phases, id]], "B" -> "I"}]
	},
		id -> {
			"Index" -> If[Lookup[types, id] == "B", Join[# /. Subscript -> Superscript, # /. Superscript -> Subscript] &, Identity] @ Join[outputs, inputs],
			"Tensor" -> QuantumOperator[label, If[Lookup[types, id] == "B", {#, #} & @ Range[Length[outputs] + Length[inputs]], {Range @ Length @ outputs, Range @ Length @ inputs}]]["TensorRepresentation"],
			VertexLabels -> label
		}
	],
		Keys[types]
	];
	Graph[
		taggedEdges, AnnotationRules -> annotations,
		FilterRules[{opts}, Options[Graph]],
		GraphLayout -> {"LayeredDigraphEmbedding", "Orientation" -> Left},
		VertexStyle -> v_ :> Replace[Lookup[types, v], {"Z" -> Green, "X" -> Red, "H" -> Yellow, "B" -> Black}],
		VertexShapeFunction -> v_ :> Replace[Lookup[types, v], {"H" -> "Square", _ -> Automatic}],
		VertexLabels -> v_ :> Replace[Lookup[types, v], {"Z" | "X" :> Placed[Pi Lookup[phases, v], Center], _ -> ""}],
		VertexSize -> v_ :> Replace[Lookup[types, v], {"H" | "B" -> .15, _ -> .3}],
		(*VertexCoordinates -> If[TrueQ[OptionValue["Simplify"]], Automatic, (# -> {Automatic, Replace[Lookup[qubits, #, Automatic], x_ ? NumericQ :> - x]} & /@ Keys[types])],*)
		PerformanceGoal -> "Quality"
	]
]

