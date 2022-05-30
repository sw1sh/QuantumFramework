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


QuantumCircuitOperator[{"GroverAmplification" | "GroverDiffusion", xs : {_Integer ? Positive..}, m : _Integer ? Positive | Automatic | None : Automatic, gate_ : "NOT"}] := Module[{
    target = Replace[m, Automatic -> Max[xs]],
    ys
},
    ys = DeleteCases[xs, target];
    QuantumCircuitOperator[{
        Splice[Table[QuantumOperator["H", {q}], {q, ys}]],
        Splice[Table[QuantumOperator["X", {q}], {q, ys}]],
        QuantumOperator[{"Controlled", gate, ys}, {target}],
        Splice[Table[QuantumOperator["X", {q}], {q, ys}]],
        Splice[Table[QuantumOperator["H", {q}], {q, ys}]]
    }]
]

QuantumCircuitOperator[{"GroverPhaseAmplification" | "GroverPhaseDiffusion", xs : {_Integer ? Positive..}, m : _Integer ? Positive | Automatic | None : Automatic, gate_ : "Z"}, opts___] := Module[{
    target = Replace[m, Automatic -> Max[xs]]
},
    QuantumCircuitOperator[{
        Splice[Table[QuantumOperator["H", {q}], {q, xs}]],
        Splice[Table[QuantumOperator["X", {q}], {q, xs}]],
        QuantumOperator[{"Controlled", gate, DeleteCases[xs, target]}, {target}],
        Splice[Table[QuantumOperator["X", {q}], {q, xs}]],
        Splice[Table[QuantumOperator["H", {q}], {q, xs}]]
    }, opts]
]

QuantumCircuitOperator[{"GroverAmplification0" | "GroverDiffusion0", xs : {_Integer ? Positive..}, m : _Integer ? Positive | Automatic | None : Automatic, gate_ : "NOT"}, opts___] := Module[{
    target = Replace[m, Automatic -> Max[xs]],
    ys
},
    ys = DeleteCases[xs, target];
    QuantumCircuitOperator[
        QuantumCircuitOperator[{
            Splice[Table[QuantumOperator["H", {q}], {q, ys}]],
            QuantumOperator[{"Controlled0", gate, ys}, {target}],
            Splice[Table[QuantumOperator["H", {q}], {q, ys}]]
        }, "GroverDiffusion"],
        opts
    ]
]

QuantumCircuitOperator[{"GroverPhaseAmplification0" | "GroverPhaseDiffusion0", xs : {_Integer ? Positive..}, m : _Integer ? Positive | Automatic | None : Automatic, gate_ : "NOT"}, opts___] := Module[{
    target = Replace[m, Automatic -> Max[xs]]
},
    QuantumCircuitOperator[
        QuantumCircuitOperator[{
            Splice[Table[QuantumOperator["H", {q}], {q, xs}]],
            QuantumOperator[{"Controlled0", - QuantumOperator[gate], DeleteCases[xs, target]}, {target}],
            Splice[Table[QuantumOperator["H", {q}], {q, xs}]]
        }, "GroverDiffusion"],
        opts
    ]
]

QuantumCircuitOperator[{
    name : "GroverAmplification" | "GroverAmplification0" | "GroverDiffusion" | "GroverDiffusion0" |
    "GroverPhaseAmplification" | "GroverPhaseDiffusion" | "GroverPhaseAmplification0" | "GroverPhaseDiffusion0",
    n_Integer ? Positive, m : _Integer ? Positive | Automatic : Automatic}, opts___] :=
    QuantumCircuitOperator[{name, Range[n], m}, opts]


QuantumCircuitOperator[{
        name : "GroverOperator" | "Grover" | "GroverOperator0" | "Grover0" |
        "GroverPhaseOperator" | "GroverPhase" | "GroverPhaseOperator0" | "GroverPhase0",
        op_ ? QuantumFrameworkOperatorQ,
        m : _Integer ? Positive | Automatic | None : Automatic,
        gate_ : Automatic
    },
    opts___
] := With[{
    n = Replace[m, Automatic -> Max[op["OutputOrder"]]]
},
    QuantumCircuitOperator[
        QuantumCircuitOperator[{
            "Grover" <> If[StringContainsQ[name, "Phase"], "Phase", ""] <> "Diffusion" <> If[StringEndsQ[name, "0"], "0", ""],
            op["OutputOrder"], n, Replace[gate, Automatic -> If[StringContainsQ[name, "Phase"], "Z", "NOT"]]}
        ] @ op,
        opts
    ]
]


QuantumCircuitOperator[{
        name : "GroverOperator" | "Grover" | "GroverOperator0" | "Grover0" |
        "GroverPhaseOperator" | "GroverPhase" | "GroverPhaseOperator0" | "GroverPhase0",
        formula_,
        m : _Integer ? Positive | Automatic | None : Automatic,
        gate_ : Automatic
    },
    opts___
] := Enclose @ With[{
    oracle = Confirm @ QuantumCircuitOperator[{If[StringContainsQ[name, "Phase"], "PhaseOracle", "BooleanOracle"], formula}]
},
    QuantumCircuitOperator[{name, oracle, m, gate}, opts]
]

indicesPattern = {KeyValuePattern[0 | 1 -> {_Integer ? Positive...}]..}

BooleanIndices[formula_, defaultVars : _List | Automatic : Automatic] := Enclose @ Module[{
    esop = Confirm[BooleanConvert[formula, "ESOP"]] /. And -> List,
    vars = Replace[defaultVars, Automatic -> Replace[BooleanVariables[formula], m_Integer :> Array[\[FormalX], m]]],
    indices
},
    If[ MatchQ[esop, _Function],
        esop = esop @@ vars
    ];
    esop = Replace[esop, clause : Except[_Xor] :> {clause}]  /. Xor -> List;
    esop = Replace[esop, clause : Except[_List] :> {clause}, {1}];
	indices = <|0 -> {}, 1 -> {}, KeySelect[Not @* MissingQ] @ PositionIndex @ Lookup[#, vars]|> & /@ Map[If[MatchQ[#, _Not], #[[1]] -> 0, # -> 1] &, esop, {2}];
	indices = SortBy[indices, Values /* Catenate /* Length];
    indices
]


QuantumCircuitOperator[{"BooleanOracle", formula_, defaultVars : _List | Automatic : Automatic, n : _Integer ? NonNegative | Automatic : Automatic, gate_ : "NOT"}, opts___] := Enclose @ Module[{
    indices = ConfirmMatch[BooleanIndices[formula, defaultVars], indicesPattern], targetQubits
},
    targetQubits = {Max[Max[indices, If[defaultVars === Automatic, 0, Length[defaultVars]]] + 1, Replace[n, Automatic -> 0]]};
	QuantumCircuitOperator[QuantumCircuitOperator[QuantumOperator[{"Controlled", gate, #[1], #[0]}, targetQubits] & /@ indices, formula], opts]
]

QuantumCircuitOperator[{"BooleanOracleZ", formula_, defaultVars : _List | Automatic : Automatic, n : _Integer ? NonNegative | Automatic : Automatic}, opts___] := Enclose @ Module[{
    indices = ConfirmMatch[BooleanIndices[formula, defaultVars], indicesPattern], m, angles, targetQubit
},
    m = Max[indices, If[defaultVars === Automatic, 0, Length[defaultVars]]];
    angles = ConfirmMatch[BooleanGrayAngles[indices], {{Repeated[{_, _Integer}, 2 ^ m]}..}];
    targetQubit = Max[m + 1, Replace[n, Automatic -> 0]];
	QuantumCircuitOperator[Flatten @ Map[{If[#[[1]] == 0, Nothing, QuantumOperator[{"ZRotation", #[[1]]}, {targetQubit}]], QuantumOperator["CNOT", {#[[2]], targetQubit}]} &, angles, {2}], opts]
]

QuantumCircuitOperator[{name : "BooleanOracle" | "BooleanOracleZ", formula_, vars : KeyValuePattern[_ -> _Integer ? Positive], n : _Integer ? NonNegative : Automatic, gate_ : "NOT"}, opts___] :=
    QuantumCircuitOperator[{name, formula, Lookup[Reverse /@ Normal @ vars, Range[Max[vars]]], n, gate}, opts]

GrayMatrix[n_] := With[{range = Range[0, 2 ^ n - 1]}, Outer[(-1)^Dot[##] &, IntegerDigits[range, 2, n], PadLeft[#, n] & /@ ResourceFunction["GrayCode"][range], 1]]

GrayOrders[n_] := ResourceFunction["SymmetricDifference"] @@@ Partition[Append[ResourceFunction["GrayCodeSubsets"][Range[n]], {}], 2, 1]

BooleanGrayAngles[indices : indicesPattern] :=
	With[{n = Length[#[0]] + Length[#[1]], order = Union @@ #},
		Thread[{1 / 2 ^ n Transpose[GrayMatrix[n]] . ReplacePart[ConstantArray[0, 2 ^ n], Fold[BitSet, 0, n - Lookup[PositionIndex[order], #[1]]] + 1 -> Pi], Extract[order, GrayOrders[n]]}]
	] & /@ indices


QuantumCircuitOperator[{"PhaseOracle", formula_, defaultVars : _List | Automatic : Automatic, n : _Integer ? NonNegative | Automatic : Automatic}, opts___] := Enclose @ Module[{
    esop = Confirm[BooleanConvert[formula, "ESOP"]] /. And -> List,
    vars = Replace[defaultVars, Automatic -> Replace[BooleanVariables[formula], m_Integer :> Array[\[FormalX], m]]],
    indices,
    m
},
    m = Replace[n, Automatic -> Length[vars]];
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
                    QuantumOperator[{"Controlled0", - QuantumOperator["Z"], DeleteCases[m] @ #[0]}, {m}]
                ],
                If[ !MemberQ[#[0], m],
                    QuantumOperator[{"Controlled", "Z", DeleteCases[m] @ #[1], #[0]}, {m}],
                    QuantumOperator[{"Controlled", - QuantumOperator["Z"], #[1], DeleteCases[m] @ #[0]}, {m}]
                ]
            ] & /@ indices,
            formula
        ],
        opts
    ]
]

QuantumCircuitOperator[{"PhaseOracle", formula_, vars : KeyValuePattern[_ -> _Integer ? Positive], n : _Integer ? NonNegative : Automatic}, opts___] :=
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


QuantumCircuitOperator[pauliString_String] := With[{chars = Characters[pauliString]},
    QuantumCircuitOperator[MapIndexed[QuantumOperator, chars]] /; ContainsOnly[chars, {"I", "X", "Y", "Z"}]
]

