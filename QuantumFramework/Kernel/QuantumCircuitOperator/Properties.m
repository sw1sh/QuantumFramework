Package["Wolfram`QuantumFramework`"]



$QuantumCircuitOperatorProperties = {
    "Association", "Operators", "Diagram", "OperatorCount", "Orders", "CircuitOperator", "QiskitCircuit", "Label",
    "Depth", "Arity", "Width", "TensorNetwork", "Topology"
};


QuantumCircuitOperator["Properties"] := Sort @ $QuantumCircuitOperatorProperties

QuantumCircuitOperatorProp[qco_, "Properties"] := Enclose @ Union @ Join[
    QuantumCircuitOperator["Properties"],
    ConfirmBy[Last[qco["Operators"]], QuantumFrameworkOperatorQ]["Properties"]
]


qds_QuantumCircuitOperator["ValidQ"] := QuantumCircuitOperatorQ[qds]


QuantumCircuitOperator::undefprop = "property `` is undefined for this circuit";

$QuantumCircuitPreventCache = {"Association", "Elements", "Diagram", "Icon", "Qiskit", "QiskitCircuit", "QuantumOperator", "Double", "Bend", "DiscardExtraQudits"}

(qds_QuantumCircuitOperator[prop_ ? propQ, args___]) /; QuantumCircuitOperatorQ[qds] := With[{
    result = QuantumCircuitOperatorProp[qds, prop, args]
},
    If[ TrueQ[$QuantumFrameworkPropCache] && ! MemberQ[$QuantumCircuitPreventCache, propName[prop]],
        QuantumCircuitOperatorProp[qds, prop, args] = result,
        result
    ] /; !MatchQ[Unevaluated @ result, _QuantumCircuitOperatorProp] || Message[QuantumCircuitOperator::undefprop, prop]
]

QuantumCircuitOperatorProp[QuantumCircuitOperator[data_Association], key_String] /; KeyExistsQ[data, key] := data[key]

QuantumCircuitOperatorProp[QuantumCircuitOperator[data_Association], "Association"] := data

QuantumCircuitOperatorProp[qco_, "FullElements"] := Replace[qco["Elements"], {} :> {QuantumOperator["I"]}]

QuantumCircuitOperatorProp[qco_, "Operators"] := Replace[DeleteCases[qco["Elements"], _ ? BarrierQ], {} :> {QuantumOperator["I"]}]

QuantumCircuitOperatorProp[qco_, "NormalOperators", elementsQ : True | False : False] :=
    First[collectCircuitOperator[#, qco, elementsQ]][If[elementsQ, "FullElements", "Operators"]] & @ MapThread[
        Which[
            BarrierQ[#],
            #,
            QuantumMeasurementOperatorQ[#] || QuantumMeasurementQ[#],
            QuantumMeasurementOperator[
                QuantumOperator[
                    #["Sort"]["POVM"]["State"],
                    #2
                ],
                #1["Target"]
            ],
            QuantumChannelQ[#],
            QuantumChannel[#, #2],
            QuantumOperatorQ[#],
            #["Reorder", #2, False],
            QuantumStateQ[#],
            QuantumOperator[#, #2],
            True,
            #
        ] &,
        {
            #[If[elementsQ, "FullElements", "Operators"]],
            #["NormalOrders", elementsQ]
        } & @ qco["Flatten"]["Sort"]
    ]

QuantumCircuitOperatorProp[qco_, "NormalElements"] := qco["NormalOperators", True]


collectCircuitOperator[flatOps_, elements_List, elementsQ_] := Block[{x, xs = elements, ops = flatOps, newElements = {}, op},
    While[xs =!= {} && ops =!= {},
        {{x}, xs} = TakeDrop[xs, 1];
        If[ QuantumCircuitOperatorQ[x],
            {op, ops} = collectCircuitOperator[ops, x, elementsQ];
            AppendTo[newElements, op]
            ,
            AppendTo[newElements, First[ops]];
            ops = Rest[ops]
        ]
    ];
    {newElements, ops}
]

collectCircuitOperator[flatOps_, qc_QuantumCircuitOperator, elementsQ_ : False] := MapAt[QuantumCircuitOperator[#, qc["Label"]] &, collectCircuitOperator[flatOps, qc[If[elementsQ, "FullElements", "Operators"]], elementsQ], {1}]


collectOrder[orders_, ops_, elementsQ_] := FoldPairList[With[{count = If[QuantumCircuitOperatorQ[#2], #2[If[TrueQ[elementsQ], "ElementCount", "OperatorCount"]], 1]}, TakeDrop[#1, count]] &, orders, ops]

collectOrders[orders_] := Fold[{Union[#2[[1]], Complement[#1[[1]], #2[[2]]]], Union[#1[[2]], Complement[#2[[2]], #1[[1]]]]} &, orders]

collectOrders[orders_, ops_, elementsQ_] := collectOrders /@ collectOrder[orders, ops, elementsQ]

QuantumCircuitOperatorProp[qco_, "NormalOrders", elementsQ_ : False] := Block[{occupied = {}, getNext, orders},
    getNext[q_] := With[{next = If[MemberQ[occupied, q], Last[Complement[Range[Min[occupied, 0] - 1, 0], occupied]], q]}, AppendTo[occupied, next]; next];
    orders = Map[
        Which[
            BarrierQ[#],
            {{}, {}},
            QuantumMeasurementOperatorQ[#],
            {Join[Sort @ Map[getNext, #["Eigenorder"]], Select[#["OutputOrder"], Positive]], #["InputOrder"]},
            QuantumChannelQ[#],
            {Join[Sort @ Map[getNext, #["TraceOrder"]], Select[#["OutputOrder"], Positive]], #["InputOrder"]},
            True,
            occupied = Complement[Union[occupied, Select[#["OutputOrder"], NonPositive]], Complement[#["InputOrder"], #["OutputOrder"]]];
            #["Order"]
        ] &,
        qco["Flatten"][If[TrueQ[elementsQ], "FullElements", "Operators"]]
    ];
    collectOrders[orders, qco[If[TrueQ[elementsQ], "FullElements", "Operators"]], elementsQ]
]

QuantumCircuitOperatorProp[qco_, "Diagram", opts : OptionsPattern[Options[CircuitDraw]]] :=
    CircuitDraw[qco, opts, ImageSize -> Medium]

QuantumCircuitOperatorProp[qco_, "Icon", opts : OptionsPattern[Options[CircuitDraw]]] :=
    CircuitDraw[
        qco, opts,
        "ShowGateLabels" -> False, "ShowMeasurementWire" -> False, "WireLabels" -> None,
        "ShowEmptyWires" -> False, "SubcircuitOptions" -> {"ShowLabel" -> False},
        ImageSize -> Tiny
    ]


Options[quantumCircuitCompile] := Join[{Method -> Automatic}, Options[TensorNetworkCompile]]

quantumCircuitCompile[qco_QuantumCircuitOperator, opts : OptionsPattern[]] :=
    Switch[
        OptionValue[Method],
        "Schrodinger" | "Schroedinger" | "Schrödinger",
        Fold[ReverseApplied[Construct], qco["Flatten"]["Operators"]],
        Automatic | "TensorNetwork",
        TensorNetworkCompile[qco, FilterRules[{opts}, Options[TensorNetworkCompile]]],
        "QuEST",
        QuESTCompile[qco],
        "Qiskit",
        qco["Qiskit"]["QuantumOperator"],
        _,
        $Failed
    ]

QuantumCircuitOperatorProp[qco_, "QuantumOperator" | "CircuitOperator" | "Compile", opts : OptionsPattern[quantumCircuitCompile]] := quantumCircuitCompile[qco["Flatten"], opts]

QuantumCircuitOperatorProp[qco_, "Gates" | "GateCount" | "OperatorCount", lvl_ : Infinity] := Length @ qco["Flatten", lvl]["Operators"]

QuantumCircuitOperatorProp[qco_, "ElementCount", lvl_ : Infinity] := Length @ qco["Flatten", lvl]["Elements"]

QuantumCircuitOperatorProp[qco_, "InputOrders"] := qco["NormalOrders"][[All, 2]]

QuantumCircuitOperatorProp[qco_, "OutputOrders"] := qco["NormalOrders"][[All, 1]]

QuantumCircuitOperatorProp[qco_, "Orders"] := Through[qco["Operators"]["Order"]]

QuantumCircuitOperatorProp[qco_, "Order"] := collectOrders[qco["NormalOrders"]]

QuantumCircuitOperatorProp[qco_, "InputOrder"] := qco["Order"][[2]]

QuantumCircuitOperatorProp[qco_, "OutputOrder"] := qco["Order"][[1]]

QuantumCircuitOperatorProp[qco_, "Depth"] := Max[1, Counts[Catenate[Union @@@ qco["NormalOrders"]]]]

QuantumCircuitOperatorProp[qco_, "Arity"] := Length @ qco["InputOrder"]

QuantumCircuitOperatorProp[qco_, "Min"] := Replace[Min[qco["NormalOrders"]], Infinity -> 1]

QuantumCircuitOperatorProp[qco_, "Max"]	:= Replace[Max[qco["NormalOrders"]], - Infinity -> 1]

QuantumCircuitOperatorProp[qco_, "FreeOrder"] := Complement[Range[Min[qco["Min"], 1], qco["Max"]], Union @@ Catenate[qco["NormalOrders"]]]

QuantumCircuitOperatorProp[qco_, "FullInputOrder"] := Union[qco["InputOrder"], qco["FreeOrder"]]

QuantumCircuitOperatorProp[qco_, "FullOutputOrder"] := Union[qco["OutputOrder"], qco["FreeOrder"]]

QuantumCircuitOperatorProp[qco_, "InputQudits"] := Length[qco["FullInputOrder"]]

QuantumCircuitOperatorProp[qco_, "OutputQudits"] := Length[qco["FullOutputOrder"]]

QuantumCircuitOperatorProp[qco_, "Width"] := Max[qco["Max"], 1] - Min[qco["Min"], 1] + 1

QuantumCircuitOperatorProp[qco_, "Span"] := qco["Max"] - qco["Min"] + 1

QuantumCircuitOperatorProp[qco_, "InputOrderQuditMapping"] := Thread[# -> Range[Length[#]]] & @ qco["InputOrder"]

QuantumCircuitOperatorProp[qco_, "OutputOrderQuditMapping"] := Thread[# -> Range[Length[#]]] & @ qco["OutputOrder"]

QuantumCircuitOperatorProp[qco_, "InputDimensions"] :=
    (q |-> ResourceFunction["LookupPart"][#["InputDimensions"], q /. #["InputOrderQuditMapping"], 2] & @
        SelectFirst[qco["NormalOperators"], op |-> MemberQ[op["InputOrder"], q]]) /@ qco["InputOrder"]

QuantumCircuitOperatorProp[qco_, "InputDimension"] := Times @@ qco["InputDimensions"]

QuantumCircuitOperatorProp[qco_, "OutputDimensions"] :=
    (q |-> ResourceFunction["LookupPart"][#["OutputDimensions"], q /. #["OutputOrderQuditMapping"], 2] & @
        SelectFirst[Reverse @ qco["NormalOperators"], op |-> MemberQ[op["OutputOrder"], q]]) /@ qco["OutputOrder"]

QuantumCircuitOperatorProp[qco_, "OutputDimension"] := Times @@ qco["OutputDimensions"]

QuantumCircuitOperatorProp[qco_, "TraceDimensions"] := Extract[qco["OutputDimensions"], Lookup[PositionIndex[qco["OutputOrder"]], qco["TraceOrder"]]]

QuantumCircuitOperatorProp[qco_, "Input"] := If[Length[qco["InputOrder"]] > 0,
    With[{ops = qco["NormalOperators"]},
        QuantumTensorProduct[
            (q |-> #["Input"]["Extract", {q /. #["InputOrderQuditMapping"]}] & @
                SelectFirst[ops, op |-> MemberQ[op["InputOrder"], q]]) /@ qco["InputOrder"]
        ]
    ],
    QuditBasis[]
]

QuantumCircuitOperatorProp[qco_, "Output"] := If[Length[qco["OutputOrder"]] > 0,
    With[{ops = Reverse @ qco["NormalOperators"]},
        QuantumTensorProduct[
            (q |-> #["Output"]["Extract", {q /. #["OutputOrderQuditMapping"]}] & @
                SelectFirst[ops, op |-> MemberQ[op["OutputOrder"], q]]) /@ qco["OutputOrder"]
        ]
    ],
    QuditBasis[]
]

QuantumCircuitOperatorProp[qco_, "Basis"] := QuantumBasis[
    "Output" -> qco["Output"], "Input" -> qco["Input"],
    "Label" -> qco["Label"],
    "Picture" -> If[Length[#] > 0 && SameQ @@ #, First[#], "Schrodinger"] & @ DeleteMissing @ Through[qco["NormalOperators"]["Picture"]],
    "ParameterSpec" -> DeleteDuplicatesBy[Join @@ Through[qco["NormalOperators"]["ParameterSpec"]], First]
]

QuantumCircuitOperatorProp[qco_, "TensorNetworkBasis"] := Enclose @ Block[{net = Confirm @ qco["TensorNetwork", "PrependInitial" -> False], ops = qco["Flatten"]["NormalOperators"], indices, quditBases},
    indices = TensorNetworkIndices[net];
    ConfirmAssert[Length[indices] == Length[ops]];
    quditBases = Catenate @ MapThread[Join[Thread[Cases[#1, _Superscript] -> #2["Output"]["Decompose"]], Thread[Cases[#1, _Subscript] -> #2["Input"]["Decompose"]]] &, {indices, ops}];
    QuantumBasis[##,
            "Label" -> qco["Label"],
            "Picture" -> If[Length[#] > 0 && SameQ @@ #, First[#], "Schrodinger"] & @ DeleteMissing @ Through[ops["Picture"]],
            "ParameterSpec" -> DeleteDuplicatesBy[Join @@ Through[ops["ParameterSpec"]], First]
        ] & @@ Normal @ Map[
        QuantumTensorProduct[Lookup[quditBases, #]] &,
        <|
            "Output" -> {}, "Input" -> {},
            KeyMap[Replace[{Superscript -> "Output", Subscript -> "Input"}], GroupBy[TensorNetworkFreeIndices[net], Head]]
        |>
    ]
]

QuantumCircuitOperatorProp[qco_, "Measurements"] := Count[qco["Flatten"]["Operators"], _ ? QuantumMeasurementOperatorQ]

QuantumCircuitOperatorProp[qco_, "Channels"] := Count[qco["Flatten"]["Operators"], _ ? QuantumChannelQ]

QuantumCircuitOperatorProp[qco_, "Target"] :=
    Fold[
        If[ QuantumMeasurementOperatorQ[#2],
            Join[#1, #2["Target"]],
            If[Equal @@ Sort /@ #2["Order"] || ContainsNone[#2["InputOrder"], 1 - #1], #1, {}]
        ] &,
        {},
        qco["Flatten"]["Operators"]
    ]

QuantumCircuitOperatorProp[qco_, "TargetCount"] := Length @ qco["Target"]

QuantumCircuitOperatorProp[qco_, "TargetOrder"] := qco["InputOrder"]

QuantumCircuitOperatorProp[qco_, "TargetArity"] := Length @ qco["Target"]

QuantumCircuitOperatorProp[qco_, "TraceOrder"] := Fold[
    If[QuantumChannelQ[#2[[1]]], Join[#1, Select[#2[[2, 1]], NonPositive]], DeleteElements[#1, #2[[2, 2]]]] &,
    {},
    Thread[{qco["Flatten"]["Operators"], qco["Flatten"]["NormalOrders", False]}]
]

QuantumCircuitOperatorProp[qco_, "Eigenorder"] := Fold[
    If[QuantumMeasurementOperatorQ[#2[[1]]] || QuantumMeasurementQ[#2[[1]]], Join[#1, Select[#2[[2, 1]], NonPositive]], DeleteElements[#1, #2[[2, 2]]]] &,
    {},
    Thread[{qco["Flatten"]["Operators"], qco["Flatten"]["NormalOrders", False]}]
]

QuantumCircuitOperatorProp[qco_, "TraceQudits"] := Length[qco["TraceOrder"]]

QuantumCircuitOperatorProp[qco_, "Eigenqudits"] := Length[qco["Eigenorder"]]


QuantumCircuitOperatorProp[qco_, "QiskitCircuit" | "Qiskit"] := QuantumCircuitOperatorToQiskit[qco]

QuantumCircuitOperatorProp[qco_, "Stats" | "Statistics"] := Counts[#["Arity"] & /@ qco["Operators"]]

QuantumCircuitOperatorProp[qco_, "Flatten", n : _Integer ? NonNegative | Infinity : Infinity] :=
    QuantumCircuitOperator[
        If[ n === Infinity,
            FixedPoint[Flatten[Replace[#, c_ ? QuantumCircuitOperatorQ :> c["FullElements"], {1}], 1] &, qco["FullElements"]],
            Nest[Flatten[Replace[#, c_ ? QuantumCircuitOperatorQ :> c["FullElements"], {1}], 1] &, qco["FullElements"], n]
        ],
        qco["Label"]
    ]

QuantumCircuitOperatorProp[qco_, "Sort"] := QuantumCircuitOperator[If[BarrierQ[#], #, #["Sort"]] & /@ qco["Elements"], qco["Label"]]


QuantumCircuitOperatorProp[qco_, "Shift", n : _Integer : 1] :=
    QuantumCircuitOperator[#["Shift", n] & /@ qco["Operators"], qco["Label"]]


QuantumCircuitOperatorProp[qco_, "Dagger"] :=
    QuantumCircuitOperator[If[BarrierQ[#], #, #["Dagger"]] & /@ Reverse @ qco["NormalOperators", True], simplifyLabel[SuperDagger[qco["Label"]]]]

QuantumCircuitOperatorProp[qco_, prop : "Conjugate" | "Dual" | "Double"] :=
    QuantumCircuitOperator[If[BarrierQ[#], #, #[prop]] & /@ qco["Elements"], Switch[prop, "Double", Style[#, Bold] &, _, SuperStar][qco["Label"]]]

QuantumCircuitOperatorProp[qco_, prop : "Computational" | "Simplify" | "FullSimplify" | "Chop" | "ComplexExpand", args___] :=
    QuantumCircuitOperator[If[BarrierQ[#], #, #[prop, args]] & /@ qco["Elements"], qco["Label"]]


QuantumCircuitOperatorProp[qco_, "Bend"] :=
    QuantumCircuitOperator[
        Map[
            If[ #["MatrixQ"],
                If[QuantumMeasurementOperatorQ[#], QuantumMeasurementOperator, Identity] @ QuantumOperator[#]["Bend", qco["Width"]], 
                With[{op = QuantumOperator[#]}, Splice[If[QuantumChannelQ[#], Map[QuantumChannel], Identity] @ {op, op["Conjugate"]["Shift", qco["Width"]]}]]
            ] &,
            qco["NormalOperators"]
        ],
        qco["Label"]
    ]

QuantumCircuitOperatorProp[qco_, "DiscardExtraQudits"] := QuantumCircuitOperator @ Prepend[
	qco["Association"],
	"Elements" -> Replace[qco["NormalElements"], q : _QuantumMeasurementOperator | _QuantumChannel | _QuantumCircuitOperator :> q["DiscardExtraQudits"], {1}]
]


QuantumCircuitOperatorProp[qco_, "Normal", args___] :=
    QuantumCircuitOperator[qco["NormalOperators", args], qco["Label"]]

QuantumCircuitOperatorProp[qco_, "TensorNetwork", opts : OptionsPattern[QuantumTensorNetwork]] := QuantumTensorNetwork[qco["Flatten"], opts]

QuantumCircuitOperatorProp[qco_, "Hypergraph", opts : OptionsPattern[QuantumCircuitHypergraph]] := QuantumCircuitHypergraph[qco, opts]

QuantumCircuitOperatorProp[qco_, "ZXTensorNetwork", opts : OptionsPattern[ZXTensorNetwork]] := ZXTensorNetwork[qco, opts]


QuantumCircuitOperatorProp[qco_, "QASM"] :=
    Enclose[StringTemplate["OPENQASM 3.0;\nqubit[``] q;\nbit[``] c;\n"][qco["Arity"], qco["TargetCount"]] <>
        StringRiffle[ConfirmBy[#["QASM"], StringQ] & /@ qco["Flatten"]["Operators"], "\n"]]


Options[CircuitTopology] = Join[{PlotLegends -> None}, Options[Graph]]

CircuitTopology[circuit_QuantumCircuitOperator, opts : OptionsPattern[]] := Block[{g, labels, colorMap},
	g = Graph[
		DeleteDuplicates @ Catenate @ MapThread[{label, order} |->
			UndirectedEdge @@ Append[Sort[#], label] & /@ If[Length[order] > 1, Subsets, Tuples][order, {2}],
			{#["Label"] & /@ circuit["Operators"], circuit["InputOrders"]}
		],
		VertexLabels -> Automatic,
		GraphLayout -> "SpringElectricalEmbedding"
	];
	labels = DeleteDuplicates[EdgeTags[g]];
	colorMap = Association @ MapIndexed[#1 -> ColorData[93][#2[[1]]] &, labels];
	g = Graph[g,
        FilterRules[{opts}, Options[Graph]],
        EdgeStyle -> UndirectedEdge[_, _, label_] :> colorMap[label]
    ];
    If[ MatchQ[OptionValue[PlotLegends], Automatic | True],
        Legended[g, SwatchLegend[Values[colorMap], Keys[colorMap]]],
        g
    ]
]

QuantumCircuitOperatorProp[qco_, "Topology", opts___] := CircuitTopology[qco, opts]


QuantumCircuitOperatorProp[qco_, "Parameters"] := DeleteDuplicates[Join @@ Through[qco["Operators"]["Parameters"]]]

(* operator properties *)

QuantumCircuitOperatorProp[qco_, args : PatternSequence[prop_String, ___] | PatternSequence[{prop_String, ___}, ___]] /;
    MemberQ[Intersection[Last[qco["Operators"]]["Properties"], qco["Properties"]], prop] := qco["CircuitOperator"][args]

