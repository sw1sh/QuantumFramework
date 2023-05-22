Package["Wolfram`QuantumFramework`"]



$QuantumCircuitOperatorProperties = {
    "Operators", "Diagram", "Gates", "Orders", "CircuitOperator", "QiskitCircuit", "Label",
    "Depth", "Arity", "Width", "TensorNetwork", "Topology"
};


QuantumCircuitOperator["Properties"] := Sort @ $QuantumCircuitOperatorProperties

QuantumCircuitOperatorProp[qco_, "Properties"] := Enclose @ Union @ Join[
    QuantumCircuitOperator["Properties"],
    ConfirmBy[Last[qco["Operators"]], QuantumFrameworkOperatorQ]["Properties"]
]


qds_QuantumCircuitOperator["ValidQ"] := QuantumCircuitOperatorQ[qds]


QuantumCircuitOperator::undefprop = "property `` is undefined for this circuit";

(qds_QuantumCircuitOperator[prop_ ? propQ, args___]) /; QuantumCircuitOperatorQ[qds] := With[{
    result = QuantumCircuitOperatorProp[qds, prop, args]
},
    If[ TrueQ[$QuantumFrameworkPropCache] && ! MemberQ[{"Elements", "Diagram", "Qiskit", "QiskitCircuit", "QuantumOperator"}, propName[prop]],
        QuantumCircuitOperatorProp[qds, prop, args] = result,
        result
    ] /; !MatchQ[Unevaluated @ result, _QuantumCircuitOperatorProp] || Message[QuantumCircuitOperator::undefprop, prop]
]

QuantumCircuitOperatorProp[QuantumCircuitOperator[data_Association], key_String] /; KeyExistsQ[data, key] := data[key]

QuantumCircuitOperatorProp[qco_, "FullElements"] := Replace[qco["Elements"], {} :> {QuantumOperator["I"]}]

QuantumCircuitOperatorProp[qco_, "Operators"] := Replace[DeleteCases[qco["Elements"], _ ? BarrierQ], {} :> {QuantumOperator["I"]}]

QuantumCircuitOperatorProp[qco_, "NormalOperators", elements_ : False] := Block[{m = 0},
    Map[
        Which[
            BarrierQ[#],
            #,
            QuantumMeasurementOperatorQ[#],
            QuantumOperator[
                #["POVM"]["QuantumOperator"],
                {Join[Reverse @ Table[m--, #["Eigenqudits"]], Select[#["OutputOrder"], Positive]], #["InputOrder"]},
                "Label" -> "Measurement"
            ],
            QuantumChannelQ[#],
            QuantumOperator[
                #["QuantumOperator"],
                {Join[Reverse @ Table[m--, #["TraceQudits"]], Select[#["OutputOrder"], Positive]], #["InputOrder"]}
            ],
            True,
            With[{op = If[ QuantumCircuitOperatorQ[#], QuantumCircuitOperator[#["Flatten"]["NormalOperators"], #["Label"]], #]},
                m += Count[Complement[op["InputOrder"], op["OutputOrder"]], _ ? NonPositive];
                m = Min[m, 0];
                op
            ]
        ] &,
        qco[If[elements, "FullElements", "Operators"]]
    ]
]

QuantumCircuitOperatorProp[qco_, "NormalOrders", elements_ : False] := Block[{m = 0},
    Map[
        Which[
            BarrierQ[#],
            Table[circuitElementPosition[#, qco["Min"], qco["Max"]] + qco["Min"] - 1, 2],
            QuantumMeasurementOperatorQ[#],
            {Join[Reverse @ Table[m--, #["Eigenqudits"]], Select[#["OutputOrder"], Positive]], #["InputOrder"]},
            QuantumChannelQ[#],
            {Join[Reverse @ Table[m--, #["TraceQudits"]], Select[#["OutputOrder"], Positive]], #["InputOrder"]},
            True,
            m += Count[Complement[#["InputOrder"], #["OutputOrder"]], _ ? NonPositive];
            m = Min[m, 0];
            #["Order"]
        ] &,
        qco[If[elements, "FullElements", "Operators"]]
    ]
]

QuantumCircuitOperatorProp[qco_, "Diagram", opts : OptionsPattern[Options[CircuitDraw]]] :=
    CircuitDraw[qco, opts, ImageSize -> Medium]

QuantumCircuitOperatorProp[qco_, "Icon", opts : OptionsPattern[Options[CircuitDraw]]] :=
    CircuitDraw[
        qco, opts,
        "ShowGateLabels" -> False, "ShowMeasurementWire" -> False, "WireLabels" -> None,
        "SubcircuitOptions" -> {"ShowLabel" -> False},
        ImageSize -> Tiny
    ]


Options[quantumCircuitCompile] = {Method -> Automatic}

quantumCircuitCompile[qco_QuantumCircuitOperator, OptionsPattern[]] :=
    Switch[
        OptionValue[Method],
        "Schrodinger" | "Schroedinger" | "Schrödinger",
        Fold[ReverseApplied[Construct], qco["Flatten"]["Operators"]],
        Automatic | "TensorNetwork",
        TensorNetworkCompile[qco],
        "QuEST",
        QuESTCompile[qco],
        "Qiskit",
        qco["Qiskit"]["QuantumOperator"],
        _,
        $Failed
    ]

QuantumCircuitOperatorProp[qco_, "QuantumOperator" | "CircuitOperator" | "Compile", opts : OptionsPattern[quantumCircuitCompile]] := quantumCircuitCompile[qco, opts]

QuantumCircuitOperatorProp[qco_, "Gates"] := Length @ qco["Flatten"]["Operators"]

QuantumCircuitOperatorProp[qco_, "InputOrders"] := qco["NormalOrders"][[All, 2]]

QuantumCircuitOperatorProp[qco_, "OutputOrders"] := qco["NormalOrders"][[All, 1]]

QuantumCircuitOperatorProp[qco_, "Orders"] := Through[qco["Operators"]["Order"]]

QuantumCircuitOperatorProp[qco_, "Order"] :=
    Fold[{Union[#2[[1]], Complement[#1[[1]], #2[[2]]]], Union[#1[[2]], Complement[#2[[2]], #1[[1]]]]} &, qco["NormalOrders"]]

QuantumCircuitOperatorProp[qco_, "InputOrder"] := qco["Order"][[2]]

QuantumCircuitOperatorProp[qco_, "OutputOrder"] := qco["Order"][[1]]

QuantumCircuitOperatorProp[qco_, "Depth"] := Max[1, Counts[Catenate[Union @@@ qco["NormalOrders"]]]]

QuantumCircuitOperatorProp[qco_, "Arity"] := Length @ qco["InputOrder"]

QuantumCircuitOperatorProp[qco_, "Min"] := Replace[Min[qco["NormalOrders"]], Infinity -> 1]

QuantumCircuitOperatorProp[qco_, "Max"]	:= Replace[Max[qco["NormalOrders"]], - Infinity -> 1]

QuantumCircuitOperatorProp[qco_, "Width"] := Max[qco["Max"], 1] - Min[qco["Min"], 1] + 1

QuantumCircuitOperatorProp[qco_, "Span"] := qco["Max"] - qco["Min"] + 1

QuantumCircuitOperatorProp[qco_, "InputDimensions"] :=
    (q |-> #["InputDimensions"][[ q /. #["InputOrderQuditMapping"] ]] & @
        SelectFirst[qco["Operators"], op |-> MemberQ[op["FullInputOrder"], q]]) /@ qco["InputOrder"]

QuantumCircuitOperatorProp[qco_, "InputDimension"] := Times @@ qco["InputDimensions"]

QuantumCircuitOperatorProp[qco_, "OutputDimensions"] :=
    (q |-> #["OutputDimensions"][[ q /. #["OutputOrderQuditMapping"] ]] & @
        SelectFirst[Reverse @ qco["Operators"], op |-> MemberQ[op["FullOutputOrder"], q]]) /@ qco["OutputOrder"]

QuantumCircuitOperatorProp[qco_, "OutputDimension"] := Times @@ qco["OutputDimensions"]

QuantumCircuitOperatorProp[qco_, "Input"] := If[Length[qco["InputOrder"]] > 0,
    With[{ops = qco["NormalOperators"]},
        QuantumTensorProduct[
            (q |-> #["Input"]["Extract", {q /. #["InputOrderQuditMapping"]}] & @
                SelectFirst[ops, op |-> MemberQ[op["FullInputOrder"], q]]) /@ qco["InputOrder"]
        ]
    ],
    QuditBasis[]
]

QuantumCircuitOperatorProp[qco_, "Output"] := If[Length[qco["OutputOrder"]] > 0,
    With[{ops = Reverse @ qco["NormalOperators"]},
        QuantumTensorProduct[
            (q |-> #["Output"]["Extract", {q /. #["OutputOrderQuditMapping"]}] & @
                SelectFirst[ops, op |-> MemberQ[op["FullOutputOrder"], q]]) /@ qco["OutputOrder"]
        ]
    ],
    QuditBasis[]
]

QuantumCircuitOperatorProp[qco_, "Basis"] := QuantumBasis[qco["Output"], qco["Input"]]

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

QuantumCircuitOperatorProp[qco_, "Targets"] := Length @ qco["Target"]

QuantumCircuitOperatorProp[qco_, "TargetOrder"] := qco["InputOrder"]

QuantumCircuitOperatorProp[qco_, "TargetArity"] := Length @ qco["Target"]

QuantumCircuitOperatorProp[qco_, "Eigenqudits"] := Total[#["Eigenqudits"] & /@ Select[qco["Flatten"]["Operators"], QuantumMeasurementOperatorQ]]

QuantumCircuitOperatorProp[qco_, "TraceOrder"] := Union @@ (#["TraceOrder"] & /@ Select[qco["Flatten"]["Operators"], QuantumChannelQ])

QuantumCircuitOperatorProp[qco_, "TraceQudits"] := Total[#["TraceQudits"] & /@ Select[qco["Flatten"]["Operators"], QuantumChannelQ]]


QuantumCircuitOperatorProp[qco_, "QiskitCircuit" | "Qiskit"] := QuantumCircuitOperatorToQiskit[qco]

QuantumCircuitOperatorProp[qco_, "Stats" | "Statistics"] := Counts[#["Arity"] & /@ qco["Operators"]]

QuantumCircuitOperatorProp[qco_, "Flatten", n : _Integer ? NonNegative | Infinity : Infinity] :=
    QuantumCircuitOperator[
        If[ n === Infinity,
            FixedPoint[Flatten[Replace[#, c_ ? QuantumCircuitOperatorQ :> c["Elements"], {1}], 1] &, qco["Elements"]],
            Nest[Flatten[Replace[#, c_ ? QuantumCircuitOperatorQ :> c["Elements"], {1}], 1] &, qco["Elements"], n]
        ],
        qco["Label"]
    ]

QuantumCircuitOperatorProp[qco_, "Sort"] := QuantumCircuitOperator[#["Sort"] & /@ qco["Operators"]]


QuantumCircuitOperatorProp[qco_, "Shift", n : _Integer ? NonNegative : 1] :=
    QuantumCircuitOperator[#["Shift", n] & /@ qco["Operators"], qco["Label"]]


QuantumCircuitOperatorProp[qco_, "Dagger"] :=
    QuantumCircuitOperator[#["Dagger"] & /@ Reverse @ qco["Operators"], simplifyLabel[SuperDagger[qco["Label"]]]]

QuantumCircuitOperatorProp[qco_, prop : "Conjugate" | "Dual"] :=
    QuantumCircuitOperator[#[prop] & /@ qco["Operators"], SuperStar[qco["Label"]]]


QuantumCircuitOperatorProp[qco_, "Normal"] :=
    QuantumCircuitOperator[Which[QuantumMeasurementOperatorQ[#], #["SuperOperator"], QuantumChannelQ[#], #["QuantumOperator"], True, #] & /@ qco["Operators"], qco["Label"]]

QuantumCircuitOperatorProp[qco_, "TensorNetwork", opts : OptionsPattern[QuantumTensorNetwork]] := QuantumTensorNetwork[qco["Flatten"], opts]


QuantumCircuitOperatorProp[qco_, "QASM"] :=
    Enclose[StringTemplate["OPENQASM 3.0;\nqubit[``] q;\nbit[``] c;\n"][qco["Width"], qco["Targets"]] <>
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


(* operator properties *)

QuantumCircuitOperatorProp[qco_, args : PatternSequence[prop_String, ___] | PatternSequence[{prop_String, ___}, ___]] /;
    MemberQ[Intersection[Last[qco["Operators"]]["Properties"], qco["Properties"]], prop] := qco["CircuitOperator"][args]

