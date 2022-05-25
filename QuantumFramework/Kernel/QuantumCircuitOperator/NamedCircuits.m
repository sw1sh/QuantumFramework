Package["Wolfram`QuantumFramework`"]

PackageScope["$QuantumCircuitOperatorNames"]



$QuantumCircuitOperatorNames = {
    "Graph",
    "GroverDiffusion", "GroverDiffusion0",
    "GroverPhaseDiffusion", "GroverPhaseDiffusion0",
    "BooleanOracle", "PhaseOracle",
    "Grover", "GroverPhase",
    "Grover0", "GroverPhase0",
    "Toffoli"
}


QuantumCircuitOperator[{"Graph", g_Graph}, opts___] := QuantumCircuitOperator[
    QuantumCircuitOperator[QuantumOperator["CZ", {#1, #2}] & @@@ EdgeList[IndexGraph @ g], "\[ScriptCapitalG]"],
    opts
]


QuantumCircuitOperator[{"GroverAmplification" | "GroverDiffusion", xs : {_Integer ? Positive..}, m : _Integer ? Positive | Automatic | None : Automatic}] := Module[{
    target = Replace[m, Automatic -> Max[xs]],
    ys
},
    ys = DeleteCases[xs, target];
    QuantumCircuitOperator[{
        Splice[Table[QuantumOperator["H", {q}], {q, ys}]],
        Splice[Table[QuantumOperator["X", {q}], {q, ys}]],
        QuantumOperator[{"Controlled", "NOT", ys}, {target}],
        Splice[Table[QuantumOperator["X", {q}], {q, ys}]],
        Splice[Table[QuantumOperator["H", {q}], {q, ys}]]
    }]
]

QuantumCircuitOperator[{"GroverPhaseAmplification" | "GroverPhaseDiffusion", xs : {_Integer ? Positive..}, m : _Integer ? Positive | Automatic | None : Automatic}, opts___] := Module[{
    target = Replace[m, Automatic -> Max[xs]]
},
    QuantumCircuitOperator[{
        Splice[Table[QuantumOperator["H", {q}], {q, xs}]],
        Splice[Table[QuantumOperator["X", {q}], {q, xs}]],
        QuantumOperator[{"Controlled", "Z", DeleteCases[xs, target]}, {target}],
        Splice[Table[QuantumOperator["X", {q}], {q, xs}]],
        Splice[Table[QuantumOperator["H", {q}], {q, xs}]]
    }, opts]
]

QuantumCircuitOperator[{"GroverAmplification0" | "GroverDiffusion0", xs : {_Integer ? Positive..}, m : _Integer ? Positive | Automatic | None : Automatic}, opts___] := Module[{
    target = Replace[m, Automatic -> Max[xs]],
    ys
},
    ys = DeleteCases[xs, target];
    QuantumCircuitOperator[{
        Splice[Table[QuantumOperator["H", {q}], {q, ys}]],
        QuantumOperator[{"Controlled0", "NOT", ys}, {target}],
        Splice[Table[QuantumOperator["H", {q}], {q, ys}]]
    }, opts]
]

QuantumCircuitOperator[{"GroverPhaseAmplification0" | "GroverPhaseDiffusion0", xs : {_Integer ? Positive..}, m : _Integer ? Positive | Automatic | None : Automatic}, opts___] := Module[{
    target = Replace[m, Automatic -> Max[xs]]
},
    QuantumCircuitOperator[{
        Splice[Table[QuantumOperator["H", {q}], {q, xs}]],
        QuantumOperator[{"Controlled0", - QuantumOperator["Z"], DeleteCases[xs, target]}, {target}],
        Splice[Table[QuantumOperator["H", {q}], {q, xs}]]
    }, opts]
]

QuantumCircuitOperator[{
    name : "GroverAmplification" | "GroverAmplification0" | "GroverDiffusion" | "GroverDiffusion0" |
    "GroverPhaseAmplification" | "GroverPhaseDiffusion" | "GroverPhaseAmplification0" | "GroverPhaseDiffusion0",
    n_Integer ? Positive, m : _Integer ? Positive | Automatic : Automatic}, opts___] :=
    QuantumCircuitOperator[{name, Range[n], m}, opts]

QuantumCircuitOperator[{"GroverOperator" | "Grover", formula_, m : _Integer ? Positive | Automatic | None : Automatic}, opts___] := Enclose @ Module[{
    oracle = Confirm @ QuantumCircuitOperator[{"BooleanOracle", formula}], n
},
    n = Replace[m, Automatic -> Max[oracle["OutputOrder"]]];
    QuantumCircuitOperator[QuantumCircuitOperator[{"GroverDiffusion", oracle["OutputOrder"], n}][oracle], opts]
]

QuantumCircuitOperator[{"GroverOperator0" | "Grover0", formula_, m : _Integer ? Positive | Automatic | None : Automatic}, opts___] := Enclose @ Module[{
    oracle = Confirm @ QuantumCircuitOperator[{"BooleanOracle", formula}], n
},
    n = Replace[m, Automatic -> Max[oracle["OutputOrder"]]];
    QuantumCircuitOperator[QuantumCircuitOperator[{"GroverDiffusion0", oracle["OutputOrder"], n}][oracle], opts]
]

QuantumCircuitOperator[{"GroverPhaseOperator" | "GroverPhase", formula_, m : _Integer ? Positive | Automatic | None : Automatic}, opts___] := Enclose @ Module[{
    oracle = Confirm @ QuantumCircuitOperator[{"PhaseOracle", formula}], n
},
    n = Replace[m, Automatic -> Max[oracle["OutputOrder"]]];
    QuantumCircuitOperator[QuantumCircuitOperator[{"GroverPhaseDiffusion", oracle["OutputOrder"], n}][oracle], opts]
]

QuantumCircuitOperator[{"GroverPhaseOperator0" | "GroverPhase0", formula_, m : _Integer ? Positive | Automatic | None : Automatic}, opts___] := Enclose @ Module[{
    oracle = Confirm @ QuantumCircuitOperator[{"PhaseOracle", formula}], n
},
    n = Replace[m, Automatic -> Max[oracle["OutputOrder"]]];
    QuantumCircuitOperator[QuantumCircuitOperator[{"GroverPhaseDiffusion0", oracle["OutputOrder"], n}][oracle], opts]
]



QuantumCircuitOperator[{"BooleanOracle", formula_, defaultVars : _List | Automatic : Automatic, n : _Integer ? NonNegative : 0}, opts___] := Enclose @ Module[{
    esop = Confirm[BooleanConvert[formula, "ESOP"]] /. And -> List,
    vars = Replace[defaultVars, Automatic -> Replace[BooleanVariables[formula], m_Integer :> Array[\[FormalX], m]]],
    indices, targetQubits
},
    If[ MatchQ[esop, _Function],
        esop = esop @@ vars
    ];
    esop = Replace[esop, clause : Except[_Xor] :> {clause}]  /. Xor -> List;
    esop = Replace[esop, clause : Except[_List] :> {clause}, {1}];
	indices = <|0 -> {}, 1 -> {}, PositionIndex @ Lookup[#, vars]|> & /@ Map[If[MatchQ[#, _Not], #[[1]] -> 0, # -> 1] &, esop, {2}];
    targetQubits = {Max[Length[vars] + 1, n]};
	QuantumCircuitOperator[QuantumCircuitOperator[QuantumOperator[{"Controlled", "NOT", #[1], #[0]}, targetQubits] & /@ indices, formula], opts]
]

QuantumCircuitOperator[{"BooleanOracle", formula_, vars : KeyValuePattern[_ -> _Integer ? Positive], n : _Integer ? NonNegative : 0}, opts___] :=
    QuantumCircuitOperator[{"BooleanOracle", formula, Lookup[Reverse /@ Normal @ vars, Range[Max[vars]]], n}, opts]


QuantumCircuitOperator[{"PhaseOracle", formula_, defaultVars : _List | Automatic : Automatic}, opts___] := Enclose @ Module[{
    esop = Confirm[BooleanConvert[formula, "ESOP"]] /. And -> List,
    vars = Replace[defaultVars, Automatic -> Replace[BooleanVariables[formula], m_Integer :> Array[\[FormalX], m]]],
    indices
},
    If[ MatchQ[esop, _Function],
        esop = esop @@ vars
    ];
    esop = Replace[esop, clause : Except[_Xor] :> {clause}]  /. Xor -> List;
    esop = Replace[esop, clause : Except[_List] :> {clause}, {1}];
	indices = <|0 -> {}, 1 -> {}, PositionIndex @ Lookup[#, vars]|> & /@ Map[If[MatchQ[#, _Not], #[[1]] -> 0, # -> 1] &, esop, {2}];
	QuantumCircuitOperator[
        QuantumCircuitOperator[
            If[ #[1] === {},
                If[ #[0] === {},
                    QuantumOperator[{"Identity", 2, Max[Length[vars], 1]}],
                    QuantumOperator[{"Controlled0", - QuantumOperator["Z"], Rest @ #[0]}, {First @ #[0]}]
                ],
                QuantumOperator[{"Controlled", "Z", Rest @ #[1], #[0]}, {First @ #[1]}]
            ] & /@ indices,
            formula
        ],
        opts
    ]
]

QuantumCircuitOperator[{"PhaseOracle", formula_, vars : KeyValuePattern[_ -> _Integer ? Positive], n : _Integer ? NonNegative : 0}, opts___] :=
    QuantumCircuitOperator[{"PhaseOracle", formula, Lookup[Reverse /@ Normal @ vars, Range[Max[vars]]], n}, opts]



QuantumCircuitOperator["Toffoli", opts___] := QuantumCircuitOperator[{"Toffoli", 1}, opts]

QuantumCircuitOperator[{"Toffoli", n : _Integer ? Positive : 1}, opts___] := QuantumCircuitOperator[
    QuantumCircuitOperator[{
        QuantumOperator["H", {n + 2}],
        QuantumOperator["CNOT", {n + 1, n + 2}],
        QuantumOperator["T", {n + 2}]["Dagger"],
        QuantumOperator["CNOT", {n, n + 2}],
        QuantumOperator["T", {n + 2}],
        QuantumOperator["CNOT", {n + 1, n + 2}],
        QuantumOperator["T", {n + 2}]["Dagger"],
        QuantumOperator["CNOT", {n, n + 2}],
        QuantumOperator["T", {n + 1}]["Dagger"],
        QuantumOperator["T", {n + 2}],
        QuantumOperator["H", {n + 2}],
        QuantumOperator["CNOT", {n, n + 1}],
        QuantumOperator["T", {n + 1}]["Dagger"],
        QuantumOperator["CNOT", {n, n + 1}],
        QuantumOperator["T", {n}],
        QuantumOperator["S", {n + 1}]
    }, "Toffoli"],
    opts
]

