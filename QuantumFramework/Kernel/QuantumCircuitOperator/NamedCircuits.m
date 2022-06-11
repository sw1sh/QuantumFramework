Package["Wolfram`QuantumFramework`"]

PackageScope["$QuantumCircuitOperatorNames"]



$QuantumCircuitOperatorNames = {
    "Graph",
    "GroverDiffusion", "GroverDiffusion0",
    "GroverPhaseDiffusion", "GroverPhaseDiffusion0",
    "BooleanOracle", "PhaseOracle",
    "BooleanOracleR",
    "Grover", "GroverPhase",
    "Grover0", "GroverPhase0",
    "Toffoli"
}


QuantumCircuitOperator[{"Graph", g_Graph, gate_ : "CNOT"}, opts___] := QuantumCircuitOperator[
    QuantumCircuitOperator[QuantumOperator[gate, {#1, #2}] & @@@ EdgeList[IndexGraph @ g], "\[ScriptCapitalG]"],
    opts
]


QuantumCircuitOperator[{"GroverAmplification" | "GroverDiffusion",
    xs : {_Integer ? Positive..},
    gate : _ ? QuantumOperatorQ | Automatic : Automatic
}, opts___] := Module[{
    op = If[gate === Automatic, QuantumOperator["NOT", {Max[xs]}], QuantumOperator[gate]], ys
},
    ys = DeleteCases[xs, Alternatives @@ op["OutputOrder"]];
    QuantumCircuitOperator[{
        Splice[Table[QuantumOperator["H", {q}], {q, ys}]],
        Splice[Table[QuantumOperator["X", {q}], {q, ys}]],
        QuantumOperator[{"Controlled", op, ys}],
        Splice[Table[QuantumOperator["X", {q}], {q, ys}]],
        Splice[Table[QuantumOperator["H", {q}], {q, ys}]]
    },
        opts
    ]
]

QuantumCircuitOperator[{"GroverPhaseAmplification" | "GroverPhaseDiffusion",
    xs : {_Integer ? Positive..},
    gate : _ ? QuantumOperatorQ | Automatic : Automatic
}, opts___] := With[{
    op = If[gate === Automatic, QuantumOperator["Z", {Max[xs]}], QuantumOperator[gate]]
},
    QuantumCircuitOperator[{
        Splice[Table[QuantumOperator["H", {q}], {q, xs}]],
        Splice[Table[QuantumOperator["X", {q}], {q, xs}]],
        QuantumOperator[{"Controlled", op, DeleteCases[xs, Alternatives @@ op["OutputOrder"]]}],
        Splice[Table[QuantumOperator["X", {q}], {q, xs}]],
        Splice[Table[QuantumOperator["H", {q}], {q, xs}]]
    },
        opts
    ]
]

QuantumCircuitOperator[{"GroverAmplification0" | "GroverDiffusion0",
    xs : {_Integer ? Positive..},
    gate : _ ? QuantumOperatorQ | Automatic : Automatic
}, opts___] := Module[{
    op = If[gate === Automatic, QuantumOperator["NOT", {Max[xs]}], QuantumOperator[gate]], ys
},
    ys = DeleteCases[xs, Alternatives @@ op["OutputOrder"]];
    QuantumCircuitOperator[
        QuantumCircuitOperator[{
            Splice[Table[QuantumOperator["H", {q}], {q, ys}]],
            QuantumOperator[{"Controlled0", op, ys}],
            Splice[Table[QuantumOperator["H", {q}], {q, ys}]]
        }, "GroverDiffusion"],
        opts
    ]
]

QuantumCircuitOperator[{"GroverPhaseAmplification0" | "GroverPhaseDiffusion0",
    xs : {_Integer ? Positive..},
    gate : _ ? QuantumOperatorQ | Automatic : Automatic
}, opts___] := Module[{
    op = If[gate === Automatic, QuantumOperator["Z", {Max[xs]}], QuantumOperator[gate]]
},
    QuantumCircuitOperator[
        QuantumCircuitOperator[{
            Splice[Table[QuantumOperator["H", {q}], {q, xs}]],
            QuantumOperator[{"Controlled0", - op, DeleteCases[xs, Alternatives @@ op["OutputOrder"]]}],
            Splice[Table[QuantumOperator["H", {q}], {q, xs}]]
        }, "GroverDiffusion"],
        opts
    ]
]

QuantumCircuitOperator[{
    name : "GroverAmplification" | "GroverAmplification0" | "GroverDiffusion" | "GroverDiffusion0" |
    "GroverPhaseAmplification" | "GroverPhaseDiffusion" | "GroverPhaseAmplification0" | "GroverPhaseDiffusion0",
    n_Integer ? Positive, gate_ : Automatic}, opts___] :=
    QuantumCircuitOperator[{name, Range[n], gate}, opts]


QuantumCircuitOperator[{
        name : "GroverOperator" | "Grover" | "GroverOperator0" | "Grover0" |
        "GroverPhaseOperator" | "GroverPhase" | "GroverPhaseOperator0" | "GroverPhase0",
        op_ ? QuantumFrameworkOperatorQ,
        gate_ : Automatic
    },
    opts___
] := QuantumCircuitOperator[
    QuantumCircuitOperator[{
        "Grover" <> If[StringContainsQ[name, "Phase"], "Phase", ""] <> "Diffusion" <> If[StringEndsQ[name, "0"], "0", ""],
        op["OutputOrder"],
        gate
    }
    ] @ op,
    opts
]


QuantumCircuitOperator[{
        name : "GroverOperator" | "Grover" | "GroverOperator0" | "Grover0" |
        "GroverPhaseOperator" | "GroverPhase" | "GroverPhaseOperator0" | "GroverPhase0",
        formula_,
        m : _Integer ? Positive | Automatic | None : Automatic,
        gate_ : Automatic
    },
    opts___
] := Enclose @ Module[{
    oracle = Confirm @ QuantumCircuitOperator[{If[StringContainsQ[name, "Phase"], "PhaseOracle", "BooleanOracle"], formula, m}], n
},
    n = Replace[m, Automatic -> Last @ oracle["OutputOrder"]];
    QuantumCircuitOperator[{
        name,
        oracle,
        QuantumOperator[Replace[gate, Automatic -> QuantumOperator[If[StringContainsQ[name, "Phase"], "Z", "NOT"], {n}]], {n}]
    }, opts]
]

indicesPattern = {KeyValuePattern[0 | 1 -> {_Integer ? Positive...}]..}

BooleanIndices[formula_, vars : _List] := Enclose @ Module[{
    esop = Confirm[BooleanConvert[formula, "ESOP"]] /. And -> List,
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


QuantumCircuitOperator[{"BooleanOracle", formula_, varSpec : _List | _Association | Automatic : Automatic, n : _Integer ? NonNegative | Automatic : Automatic, gate_ : "NOT"}, opts___] := Enclose @ Module[{
    vars, order, indices, negIndices, isNegative = False, targetQubits
},
    vars = Replace[varSpec, {
        Automatic | {__Integer} -> Replace[BooleanVariables[formula], k_Integer :> Array[\[FormalX], k]],
        rules : KeyValuePattern[{_ -> _Integer ? Positive}] :> Keys[rules]
    }];
    order = Replace[varSpec, {rules : KeyValuePattern[{_ -> _Integer ? Positive}] :> Values[rules], Except[{__Integer}] :> Range[Length[vars]]}];
    ConfirmAssert[orderQ[order]];
    indices = ConfirmMatch[BooleanIndices[formula, vars], indicesPattern];
    negIndices = ConfirmMatch[BooleanIndices[Not[Replace[formula, bf_BooleanFunction :> bf @@ vars]], vars], indicesPattern];
    If[ Length[negIndices] < Length[indices],
        indices = negIndices;
        isNegative = True;
    ];
    indices = With[{repl = Thread[Range[Length[order]] -> order]}, Replace[indices, repl, {3}]];
    targetQubits = {If[MemberQ[order, n], First[DeleteCases[Range @@ ({0, 1} + MinMax[order]), n]], Replace[n, Automatic -> Max[order] + 1]]};
	QuantumCircuitOperator[
        QuantumCircuitOperator[
            Prepend[
                QuantumOperator[{"Controlled", gate, #[1], #[0]}, targetQubits] & /@ indices,
                If[isNegative, QuantumOperator[gate, targetQubits]["Dagger"], Nothing]
            ],
            formula
        ],
        opts
    ]
]

QuantumCircuitOperator[{"BooleanOracleR",
    formula_,
    varSpec : _List | _Association | Automatic : Automatic,
    n : _Integer ? NonNegative | Automatic : Automatic,
    rotationGate : {"YRotation" | "ZRotation", _ ? NumericQ} : {"ZRotation", Pi}
}, opts___] := Enclose @ Module[{
    vars, order, indices, negIndices, isNegative = False, m, angles, targetQubit
},
    vars = Replace[varSpec, {
        Automatic | {__Integer} -> Replace[BooleanVariables[formula], k_Integer :> Array[\[FormalX], k]],
        rules : KeyValuePattern[_ -> _Integer ? Positive] :> Keys[rules]
    }];
    order = Replace[varSpec, {rules : KeyValuePattern[{_ -> _Integer ? Positive}] :> Values[rules], Except[{__Integer}] :> Range[Length[vars]]}];
    ConfirmAssert[orderQ[order]];
    indices = ConfirmMatch[BooleanIndices[formula, vars], indicesPattern];
    negIndices = ConfirmMatch[BooleanIndices[Not[Replace[formula, bf_BooleanFunction :> bf @@ vars]], vars], indicesPattern];
    If[ Length[negIndices] < Length[indices],
        indices = negIndices;
        isNegative = True;
    ];
    m = Length[order];
    angles = ConfirmMatch[BooleanGrayAngles[indices, rotationGate[[2]]], {{Repeated[{_, _Integer}, 2 ^ m]}..}];
    indices = With[{repl = Thread[Range[Length[order]] -> order]}, Replace[indices, repl, {3}]];
    targetQubit = If[MemberQ[order, n], First[DeleteCases[Range @@ ({0, 1} + MinMax[order]), n]], Replace[n, Automatic -> Max[order] + 1]];
	QuantumCircuitOperator[
        Prepend[
            Flatten @ Map[{If[#[[1]] == 0, Nothing, QuantumOperator[{rotationGate[[1]], #[[1]]}, {targetQubit}]], QuantumOperator["CNOT", {#[[2]], targetQubit}]} &, angles, {2}],
            If[isNegative, QuantumOperator[MapAt[Minus, rotationGate, {2}], {targetQubit}], Nothing]
        ],
        opts
    ]
]

GrayMatrix[n_] := With[{range = Range[0, 2 ^ n - 1]}, Outer[(-1)^Dot[##] &, IntegerDigits[range, 2, n], PadLeft[#, n] & /@ ResourceFunction["GrayCode"][range], 1]]

GrayOrders[n_] := ResourceFunction["SymmetricDifference"] @@@ Partition[Append[ResourceFunction["GrayCodeSubsets"][Range[n]], {}], 2, 1]

BooleanGrayAngles[indices : indicesPattern, angle_ : Pi] := KeyValueMap[
	With[{n = Length[#1], order = #1},
		Thread[{
            1 / 2 ^ n Transpose[GrayMatrix[n]] . ReplacePart[
                ConstantArray[0, 2 ^ n],
                Thread[Fold[BitSet, 0, n - Lookup[PositionIndex[order], #[1]]] + 1 & /@ #2 -> angle]
            ],
            Extract[order, GrayOrders[n]]
        }]
	] &,
    GroupBy[indices, Apply[Union]]
]

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

