Package["Wolfram`QuantumFramework`"]

PackageScope["$QuantumCircuitOperatorNames"]



$QuantumCircuitOperatorNames = {
    "Graph", "GHZ",
    "GroverDiffusion", "GroverDiffusion0",
    "GroverPhaseDiffusion", "GroverPhaseDiffusion0",
    "BooleanOracle", "PhaseOracle",
    "BooleanOracleR",
    "Grover", "GroverPhase",
    "Grover0", "GroverPhase0",
    "Bell", "Toffoli", "Fredkin",
    "BernsteinVaziraniOracle", "BernsteinVazirani",
    "Fourier", "InverseFourier",
    "PhaseEstimation",
    "Number", "PhaseNumber",
    "Controlled",
    "Trotterization",
    "Magic",
    "Multiplexer",
    "QuantumState",
    "CHSH"
}


QuantumCircuitOperator[{"Graph", HoldPattern[g_Graph : RandomGraph[{5, 8}]], m : _Integer ? NonNegative : 0, gate_ : {"C", "1"}}, opts___] := With[{
    ig = If[AllTrue[VertexList[g], Internal`PositiveIntegerQ], g, IndexGraph @ g]
},
    QuantumCircuitOperator[
        Join["H" -> # & /@ Range[Max[VertexList[ig]]], QuantumOperator[gate, {#1, #2} + m] & @@@
            EdgeList[ig]], "\[ScriptCapitalG]", opts
    ]
]


QuantumCircuitOperator[{"GHZ", n : _Integer ? Positive : 3}, opts___] :=
    QuantumCircuitOperator[{"H", Splice["CNOT" -> # & /@ Partition[Range[n], 2, 1]]}, "GHZ", opts]


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
    op = If[gate === Automatic, QuantumOperator["1", {Max[xs]}], QuantumOperator[gate]]
},
    QuantumCircuitOperator[{
        Splice[Table[QuantumOperator["H", {q}], {q, xs}]],
        Splice[Table[QuantumOperator["X", {q}], {q, xs}]],
        QuantumOperator[{"Controlled", op, DeleteElements[xs, op["OutputOrder"]]}],
        Splice[Table[QuantumOperator["X", {q}], {q, xs}]],
        Splice[Table[QuantumOperator["H", {q}], {q, xs}]]
    },
        opts,
        "GroverDiffusion"
    ]
]

QuantumCircuitOperator[{"GroverAmplification0" | "GroverDiffusion0",
    xs : {_Integer ? Positive..},
    gate : _ ? QuantumOperatorQ | Automatic : Automatic
}, opts___] := Block[{
    op = If[gate === Automatic, QuantumOperator["NOT", {Max[xs]}], QuantumOperator[gate]], ys
},
    ys = DeleteCases[xs, Alternatives @@ op["OutputOrder"]];
    QuantumCircuitOperator[
        {
            Splice[Table[QuantumOperator["H", {q}], {q, ys}]],
            QuantumOperator[{"Controlled0", op, ys}],
            Splice[Table[QuantumOperator["H", {q}], {q, ys}]]
        },
        opts,
        "GroverDiffusion"
    ]
]

QuantumCircuitOperator[{"GroverPhaseAmplification0" | "GroverPhaseDiffusion0",
    xs : {_Integer ? Positive..},
    gate : _ ? QuantumOperatorQ | Automatic : Automatic
}, opts___] := With[{
    op = If[gate === Automatic, QuantumOperator["1", {Max[xs]}], QuantumOperator[gate]]
},
    QuantumCircuitOperator[
        {
            Splice[Table[QuantumOperator["H", {q}], {q, xs}]],
            QuantumOperator[{"Controlled0", If[op["Label"] === "1", QuantumOperator["0", op["Order"]], - op], DeleteElements[xs, op["OutputOrder"]]}],
            Splice[Table[QuantumOperator["H", {q}], {q, xs}]]
        },
        opts,
        "GroverDiffusion"
    ]
]

QuantumCircuitOperator[{
    name : "GroverAmplification" | "GroverAmplification0" | "GroverDiffusion" | "GroverDiffusion0" |
    "GroverPhaseAmplification" | "GroverPhaseDiffusion" | "GroverPhaseAmplification0" | "GroverPhaseDiffusion0",
    n : _Integer ? Positive : 3, gate_ : Automatic}, opts___] :=
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
        formula_ : BooleanFunction[2 ^ 6, 3],
        varSpec : _List | _Association | Automatic : Automatic,
        m : _Integer | Automatic | None : Automatic,
        gate_ : Automatic
    },
    opts___
] := Enclose @ Module[{
    oracle = Confirm @ QuantumCircuitOperator[{If[StringContainsQ[name, "Phase"], "PhaseOracle", "BooleanOracle"], formula, varSpec, If[StringContainsQ[name, "Phase"], Nothing, m]}], n
},
    n = Replace[m, Automatic :> Last @ oracle["OutputOrder"]];
    QuantumCircuitOperator[{
        name,
        oracle,
        QuantumOperator[Replace[gate, Automatic :> QuantumOperator[If[StringContainsQ[name, "Phase"], "1", "NOT"], {n}]], {n}]
    }, opts]
]

indicesPattern = {KeyValuePattern[0 | 1 -> {_Integer ? Positive...}]..}

BooleanIndices[formula_, vars : _List] := Enclose @ Module[{
    esop = formula /. And -> List,
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


QuantumCircuitOperator[{"BooleanOracle",
    formula_ : BooleanFunction[2 ^ 6, 3],
    varSpec : _List | _Association | Automatic : Automatic,
    n : _Integer | Automatic : Automatic,
    m : _Integer : 0,
    gate_ : "NOT"
}, opts___] := Enclose @ Block[{
    esopFormula, vars, order, indices, negIndices, isNegative = False, targetQubits
},
    esopFormula = BooleanConvert[formula, "ESOP"];
    vars = Replace[varSpec, {
        Automatic | {__Integer} -> Replace[BooleanVariables[formula], k_Integer :> Array[\[FormalX], k]],
        rules : KeyValuePattern[{_ -> _Integer ? Positive}] :> Keys[rules]
    }];
    order = m + Replace[varSpec, {rules : KeyValuePattern[{_ -> _Integer ? Positive}] :> Values[rules], Except[{__Integer}] :> Range[Length[vars]]}];
    ConfirmAssert[orderQ[order]];
    indices = ConfirmMatch[BooleanIndices[esopFormula, vars], indicesPattern];
    negIndices = ConfirmMatch[BooleanIndices[BooleanConvert[Not[Replace[formula, bf_BooleanFunction :> bf @@ vars]], "ESOP"], vars], indicesPattern];
    If[ Length[negIndices] < Length[indices],
        indices = negIndices;
        isNegative = True;
    ];
    indices = With[{repl = Thread[Range[Length[order]] -> order]}, Replace[indices, repl, {3}]];
    targetQubits = {If[MemberQ[order, n], First[DeleteCases[Range @@ ({0, 1} + MinMax[order]), n]], Replace[n, Automatic -> Max[order] + 1]]};

    QuantumCircuitOperator[
        Join[
            Prepend[
                QuantumOperator[{"Controlled", QuantumOperator[gate, targetQubits], #[1], #[0]}] & /@ indices,
                If[isNegative, QuantumOperator[gate, targetQubits]["Dagger"], Nothing]
            ],
            QuantumOperator["I", {#}] & /@ Complement[Range[Max[indices, Length[vars]]], Flatten @ Values[indices]]
        ],
        opts,
        If[MatchQ[formula, _BooleanFunction], esopFormula, formula]
    ]
]

QuantumCircuitOperator[{"BooleanOracleR",
    formula_ : BooleanFunction[2 ^ 6, 3],
    varSpec : _List | _Association | Automatic : Automatic,
    n : _Integer ? NonNegative | Automatic : Automatic,
    m : _Integer ? NonNegative : 0,
    rotationGate : {"YRotation" | "ZRotation", _ ? NumericQ} : {"ZRotation", Pi}
}, opts___] := Enclose @ Block[{
    esopFormula, vars, order, indices, negIndices, isNegative = False, l, angles, targetQubit
},
    esopFormula = BooleanConvert[formula, "ESOP"];
    vars = Replace[varSpec, {
        Automatic | {__Integer} -> Replace[BooleanVariables[formula], k_Integer :> Array[\[FormalX], k]],
        rules : KeyValuePattern[_ -> _Integer ? Positive] :> Keys[rules]
    }];
    order = m + Replace[varSpec, {rules : KeyValuePattern[{_ -> _Integer ? Positive}] :> Values[rules], Except[{__Integer}] :> Range[Length[vars]]}];
    ConfirmAssert[orderQ[order]];
    indices = ConfirmMatch[BooleanIndices[esopFormula, vars], indicesPattern];
    negIndices = ConfirmMatch[BooleanIndices[BooleanConvert[Not[Replace[formula, bf_BooleanFunction :> bf @@ vars]], "ESOP"], vars], indicesPattern];
    If[ Length[negIndices] < Length[indices],
        indices = negIndices;
        isNegative = True;
    ];
    l = Length[order];
    angles = ConfirmMatch[BooleanGrayAngles[indices, rotationGate[[2]]], {{Repeated[{_, _Integer}, 2 ^ l]}..}];
    indices = With[{repl = Thread[Range[Length[order]] -> order]}, Replace[indices, repl, {3}]];
    targetQubit = If[MemberQ[order, n], First[DeleteCases[Range @@ ({0, 1} + MinMax[order]), n]], Replace[n, Automatic -> Max[order] + 1]];
	QuantumCircuitOperator[
        Join[
            Prepend[
                Flatten @ Map[{If[#[[1]] == 0, Nothing, QuantumOperator[{rotationGate[[1]], #[[1]]}, {targetQubit}]], QuantumOperator["CNOT", {#[[2]], targetQubit} + m]} &, angles, {2}],
                If[isNegative, QuantumOperator[MapAt[Minus, rotationGate, {2}], {targetQubit}], Nothing]
            ],
            QuantumOperator["I", {#}] & /@ Complement[Range[Max[indices, Length[vars]]], Flatten @ Values[indices]]
        ],
        opts,
        If[MatchQ[formula, _BooleanFunction], esopFormula, formula]
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

QuantumCircuitOperator[{"PhaseOracle",
    formula_ : BooleanFunction[2 ^ 6, 3],
    defaultVars : _List | Automatic : Automatic,
    m : _Integer ? NonNegative : 0
}, opts___] := Enclose @ Block[{
    esopFormula = Confirm @ BooleanConvert[formula, "ESOP"],
    esop,
    vars = Replace[defaultVars, Automatic :> Replace[BooleanVariables[formula], k_Integer :> Array[\[FormalX], k]]],
    indices
},
    esop = esopFormula /. And -> List;
    If[ MatchQ[esop, _Function],
        esop = esop @@ vars
    ];
    esop = Replace[esop, clause : Except[_Xor] :> {clause}]  /. Xor -> List;
    esop = Replace[esop, clause : Except[_List] :> {clause}, {1}];
	indices = m + <|0 -> {}, 1 -> {}, KeySelect[Not @* MissingQ] @ PositionIndex @ Lookup[#, vars]|> & /@ Map[If[MatchQ[#, _Not], #[[1]] -> 0, # -> 1] &, esop, {2}];
    QuantumCircuitOperator[
        Join[
            If[ #[1] === {},
                If[ #[0] === {},
                    QuantumOperator[{"I", 2, Max[Length[vars], 1]}],
                    QuantumOperator[{"Controlled0", "0", Most[#[0]]}, {Last[#[0]]}]
                ],
                QuantumOperator[{"Controlled", "1", Most[#[1]], #[0]}, {Last[#[1]]}]
            ] & /@ indices,
            QuantumOperator["I", {#}] & /@ Complement[Range[Max[indices, Length[vars]]], Flatten @ Values[indices]]
        ],
        opts,
        If[MatchQ[formula, _BooleanFunction], esopFormula, formula]
    ]
]

QuantumCircuitOperator[{"PhaseOracle", formula_ : BooleanFunction[2 ^ 6, 3], vars : KeyValuePattern[_ -> _Integer ? Positive], n : _Integer ? NonNegative : 0}, opts___] :=
    QuantumCircuitOperator[{"PhaseOracle", formula, Lookup[Reverse /@ Normal @ vars, Range[Max[vars]]], n}, opts]



QuantumCircuitOperator[name : "Bell" | "Toffoli" | "Fredkin", opts___] := QuantumCircuitOperator[{name}, opts]

QuantumCircuitOperator[{"Bell", n : _Integer : 0}, opts___]  :=
    QuantumCircuitOperator[{"H", "CNOT"}, opts, "Bell"]["Shift", n]

QuantumCircuitOperator[{"Toffoli", n : _Integer : 0}, opts___] := QuantumCircuitOperator[
    {
        "H" -> 3, "CNOT" -> {2, 3}, SuperDagger["T"] -> 3, "CNOT" -> {1, 3}, "T" -> 3, "CNOT" -> {2, 3}, SuperDagger["T"] -> 3, "CNOT" -> {1, 3},
        "T" -> 2, "T" -> 3, "H" -> 3, "CNOT" -> {1, 2},  "T" -> 1, SuperDagger["T"] -> 2, "CNOT" -> {1, 2}
    },
    opts,
    "Toffoli"
]["Shift", n]

QuantumCircuitOperator[{"Fredkin", n : _Integer : 0}, opts___] := QuantumCircuitOperator[{
        "CNOT" -> {3, 2}, "H" -> 3, "T" -> 1, "T" -> 2, "T" -> 3, "CNOT" -> {2, 1}, "CNOT" -> {3, 2}, "CNOT" -> {1, 3}, SuperDagger["T"] -> 2, "CNOT" -> {1, 2},
        SuperDagger["T"] -> 1, SuperDagger["T"] -> 2, "T" -> 3, "CNOT" -> {3, 2}, "CNOT" -> {1, 3}, "H" -> 3, "CNOT" -> {2, 1}, "CNOT" -> {3, 2}
    },
    opts,
    "Fredkin"
]["Shift", n]

QuantumCircuitOperator[{"BernsteinVaziraniOracle", secret : {(0 | 1) ...} : {1, 0, 1}, m : _Integer ? NonNegative : 0}, opts___] := With[{n = Length[secret]},
    QuantumCircuitOperator[
        If[MatchQ[secret, {0 ...}], Append[QuantumOperator["I", {n + 1 + m}]], Identity] @
            MapIndexed[If[#1 === 1, QuantumOperator["CNOT", {First[#2], n + 1} + m], QuantumOperator["I", #2 + m]] & , secret],
        opts,
        "BV Oracle"
    ]
]

QuantumCircuitOperator[{"BernsteinVaziraniOracle", secret_String : "101", m : _Integer ? NonNegative : 0} /; StringMatchQ[secret, ("0" | "1") ...], opts___] :=
    QuantumCircuitOperator[{"BernsteinVaziraniOracle", Characters[secret] /. {"0" -> 0, "1" -> 1}, m}, opts]

QuantumCircuitOperator[{"BernsteinVazirani", secret : {(0 | 1) ...} | (secret_String /; StringMatchQ[secret, ("0" | "1") ...]) : "101", m : _Integer ? NonNegative : 0}, opts___] := With[{
    n = If[ListQ[secret], Length[secret], StringLength[secret]]
},
    QuantumCircuitOperator[{
        Splice @ Table[QuantumOperator["H", {i + m}], {i, n + 1}],
        QuantumOperator["Z", {n + 1 + m}],
        QuantumCircuitOperator[{"BernsteinVaziraniOracle", secret, m}, opts],
        Splice @ Table[QuantumOperator["H", {i + m}], {i, n}],
        Splice @ Table[QuantumMeasurementOperator[{i + m}], {i, n}]
    }]
]


QuantumCircuitOperator[{"Number",  n : _Integer ? NonNegative : 0, qubits : _Integer | Automatic : Automatic}, opts___] := Block[{
    qs = Replace[qubits, Automatic :> Ceiling[Max[Log2[n], 0]]], bits
},
    bits = IntegerDigits[n, 2, qs];
    QuantumCircuitOperator[MapIndexed[If[#1 == 1, "X", "I"] -> #2 &, bits], opts, n]
]

QuantumCircuitOperator[{"PhaseNumber", n : _Integer ? NonNegative : 0, qubits : _Integer | Automatic : Automatic, h : True | False : True}, opts___] := Block[{
    qs = Replace[qubits, Automatic :> Ceiling[Max[Log2[n], 0]]], bits
},
    bits = IntegerDigits[n, 2, qs];
	QuantumCircuitOperator[
        Join[
            If[h, Thread["H" -> Range[qs]], {}],
            Catenate @ Table[Map["PhaseShift"[#] -> q &, Catenate @ Position[bits[[- q ;;]], 1, {1}, Heads -> False]], {q, qs}]
        ],
        opts,
        n
    ]
]

QuantumCircuitOperator[name : "Fourier" | "InverseFourier", opts___] := QuantumCircuitOperator[{name, 2}, opts]

QuantumCircuitOperator[{"Fourier", n_Integer ? Positive, m : _Integer ? NonNegative : 0}, opts___] := QuantumCircuitOperator[Join[
		Catenate @ Table[{
			QuantumOperator["H", {i + m}],
			Splice[QuantumOperator[{"Controlled", {"PhaseShift", # + 1}, {# + i + m}}, {i + m}] & /@ Range[n - i]]
		},
		{i, n}],
		QuantumOperator["SWAP", {# + m, n - # + 1 + m}] & /@ Range[Floor[n / 2]]
	],
    opts,
	"QFT"
]

QuantumCircuitOperator[{"InverseFourier", n_Integer ? Positive, m : _Integer ? NonNegative : 0}] := QuantumCircuitOperator[{"Fourier", n, m}]["Dagger"]


QuantumCircuitOperator[{
    "PhaseEstimation",
    (op : _ ? QuantumOperatorQ : QuantumOperator["RandomUnitary"]),
    n : _Integer ? Positive : 4,
    m : _Integer ? NonNegative : 0,
    params : OptionsPattern[{"PowerExpand" -> False}]
} /; op["InputDimensions"] === op["OutputDimensions"] && MatchQ[op["OutputDimensions"], {2 ..}] , opts___] :=
QuantumCircuitOperator[{
    Splice @ Table[QuantumOperator["X", {n + i + m}], {i, op["InputQudits"]}],
    Splice @ Table[QuantumOperator["H", {i + m}], {i, n}],
    With[{qo = QuantumOperator[op, op["QuditOrder"] + n + m]},
        If[ TrueQ[Lookup[{params}, "PowerExpand"]],
            Splice @ Catenate @ Table[Table[QuantumOperator[{"Controlled", qo, {i + m}}], 2 ^ (n - i)], {i, n}],
            Splice @ Table[QuantumOperator[{"Controlled", qo ^ 2 ^ (n - i), {i + m}}], {i, n}]
        ]
    ],
    QuantumCircuitOperator[{"InverseFourier", n}],
    Splice @ Table[QuantumMeasurementOperator[{i + m}], {i, n}]
},
    opts
]

QuantumCircuitOperator[{
    "PhaseEstimation",
    op_ ? QuantumCircuitOperatorQ /; op["InputDimensions"] === op["OutputDimensions"] && MatchQ[op["OutputDimensions"], {2 ..}],
    n : _Integer ? Positive : 4,
    m : _Integer ? NonNegative : 0
}, opts___] :=
QuantumCircuitOperator[{
    Splice @ Table[QuantumOperator["X", {n + i + m}], {i, op["InputQudits"]}],
    Splice @ Table[QuantumOperator["H", {i + m}], {i, n}],
    With[{qo = op["Shift", m]},
        Splice @ Catenate @ Table[Table[QuantumCircuitOperator[{"Controlled", qo, {i + m}}], 2 ^ (n - i)], {i, n}]
    ],
    QuantumCircuitOperator[{"InverseFourier", n}],
    Splice @ Table[QuantumMeasurementOperator[{i + m}], {i, n}]
},
    opts
]

QuantumCircuitOperator[{"C" | "Controlled", qc_ ? QuantumCircuitOperatorQ, control1 : _ ? orderQ | Automatic : Automatic, control0 : _ ? orderQ : {}}, opts___] :=
    QuantumCircuitOperator[If[QuantumOperatorQ[#], QuantumOperator[{"Controlled", #, control1, control0}], #] & /@ qc["Elements"], Subscript["C", qc["Label"]][control1, control0]]

QuantumCircuitOperator[{"C" | "Controlled", qc_ : "Magic", control1 : _ ? orderQ | Automatic : Automatic, control0 : _ ? orderQ : {}}, opts___] :=
    QuantumCircuitOperator[{"C", QuantumCircuitOperator @ FromCircuitOperatorShorthand[qc], control1, control0}, opts]


QuantumCircuitOperator[pauliString_String, opts___] := With[{chars = Characters[pauliString]},
    QuantumCircuitOperator[MapIndexed[QuantumOperator, chars], opts] /; ContainsOnly[chars, {"I", "X", "Y", "Z", "H", "S", "T", "V", "P"}]
]


trotterCoeffs[l_, 1, c_ : 1] := ConstantArray[c, l]
trotterCoeffs[l_, 2, c_ : 1] := With[{s = trotterCoeffs[l, 1, c / 2]}, Join[s, Reverse[s]]]
trotterCoeffs[l_, n_ ? EvenQ, c_ : 1] := With[{p = 1 / (4 - 4 ^ (1 / (n - 1)))},
	With[{s = trotterCoeffs[l, n - 2, c p]}, Join[s, s, trotterCoeffs[l, n - 2, (1 - 4 p) c], s, s]]
]
trotterCoeffs[l_, n_ ? OddQ, c_ : 1] := trotterCoeffs[l, Round[n, 2], c]

trotterExpand[l_List, 1] := l
trotterExpand[l_List, 2] := Join[l, Reverse[l]]
trotterExpand[l_List, n_ ? EvenQ] := Catenate @ Table[trotterExpand[l, n - 2], 5]
trotterExpand[l_List, n_ ? OddQ] := trotterExpand[l, Round[n, 2]]

Trotterization[ops : {__QuantumOperator}, order : _Integer ? Positive : 1, reps : _Integer ? Positive : 1, const_ : 1] := Block[{
    coeffs = const * trotterCoeffs[Length[ops], order, 1 / reps],
	newOps
},
    newOps = MapThread[
        QuantumOperator[Exp[- I #1], "Label" -> Subscript["R", #2][2 #3]] &,
        {
            Thread[Times[coeffs, trotterExpand[ops, order]]],
            trotterExpand[Through[ops["Label"]], order],
            coeffs
        }
    ];
	Table[newOps, reps]
]

QuantumCircuitOperator[{"Trotterization", opArgs_ : {"X", "Y", "Z"}, args___}, opts___] := Block[{
    ops = QuantumCircuitOperator[opArgs]["Flatten"]["Operators"],
    trotterization
},
    trotterization = Trotterization[ops, args];
    QuantumCircuitOperator[
        If[ Length[trotterization] > 1,
            MapIndexed[QuantumCircuitOperator[#1, First[#2]] &, trotterization],
            Catenate[trotterization]
        ],
        opts,
        "Trotterization"
    ]
]

QuantumCircuitOperator["Magic", opts___] := QuantumCircuitOperator[{"S" -> 1, "S" -> 2, "H" -> 2, "CNOT" -> {2, 1}}, opts]


QuantumCircuitOperator[{"Multiplexer"| "Multiplexor"}, opts___] := QuantumCircuitOperator[{"Multiplexer", "X", "Y", "Z"}, opts]

QuantumCircuitOperator[{"Multiplexer"| "Multiplexor", ops__} -> defaultK : _Integer ? Positive | Automatic : Automatic, opts___] := Block[{
    n = Length[{ops}],
    m, k, seq
},
    m = Ceiling[Log2[n]];
    k = Replace[defaultK, Automatic :> m + 1];
    seq = Values[<|0 -> {}, 1 -> {}, PositionIndex[#]|>] & /@ Take[Tuples[{1, 0}, m], n];
    QuantumCircuitOperator[MapThread[If[MatchQ[#1, "I" | {"I"..}], Nothing, {"C", #1, Splice[#2 /. c_Integer /; c == k :> m + 1]}] &, {{ops}, seq}], opts]
]

QuantumCircuitOperator[{"Multiplexer"| "Multiplexor", ops__}, opts___] := QuantumCircuitOperator[{"Multiplexer", ops} -> Automatic, opts]



RZY[vec_ ? VectorQ] := Block[{a, b, phi, psi, y, z, phase},
    If[Length[vec] == 0, Return[{}]];
    {{a, phi}, {b, psi}} = AbsArg[Simplify[Normal[vec]]];
    phase = Sow[phi + psi, "Phase"];
    y = Simplify[If[TrueQ[Simplify[a == b == 0]], Pi / 2, 2 ArcSin[(a - b) / Sqrt[2 (a ^ 2 + b ^ 2)]]]];
    z = Simplify[phi - psi];
    Replace[
        {If[TrueQ[Chop[z] == 0], Nothing, {"RZ", z}], If[TrueQ[Chop[y] == 0], Nothing, {"RY", y}]},
        {} -> {"I"}
    ]
]


multiplexer[qs_, n_, i_] := Block[{rzy, rzyDagger, qc, multiplexer, multiplexerDagger},
    rzy = RZY /@ Partition[qs["StateVector"], 2];
    rzyDagger = Reverse /@ rzy /. {r : "RZ" | "RY", angle_} :> {r, - angle};
    {multiplexer, multiplexerDagger} = {"Multiplexer", Splice[#]} & /@ {rzy, rzyDagger};
    qc = If[i === 0,
        QuantumCircuitOperator[{multiplexer, Splice["H" -> # & /@ Range[qs["Qudits"]]]}],
        QuantumCircuitOperator[{multiplexer, {"Permutation", Cycles[{{n, i}}]}}]
    ];
    Sow[
        If[i === 0,
            {multiplexerDagger, Splice["H" -> # & /@ Range[qs["Qudits"]]]},
            {multiplexerDagger, {"Permutation", Cycles[{{n, i}}]}}
        ],
        "Operators"
    ];
    qc[qs]
]

stateEvolution[qs_] := With[{n = qs["Qudits"]},
    FoldList[multiplexer[#1, n, #2] &, qs, Range[n - 1, 0, -1]]
]

QuantumCircuitOperator[qs_QuantumState | {"QuantumState", qs_QuantumState}, opts___] /; MatchQ[qs["Dimensions"], {2 ..}] := Block[{
    operators, phases, phase, n = qs["Qudits"]
},
    {operators, phases} = Reap[stateEvolution[qs["Split", n]], {"Operators", "Phase"}][[2, All, 1]];
    phase = Total[phases] / n / 2 ^ n - I Log[qs["Norm"]];
    operators = Reverse @ Catenate @ operators;
    If[ TrueQ[Chop[phase] != 0],
        AppendTo[operators, {"GlobalPhase", phase}]
    ];
    operators = Join["0" -> # & /@ Range[n], operators];
    If[ qs["InputQudits"] > 0,
        operators = Join[
            "I" -> # -> n + # & /@ Range[qs["InputQudits"]],
            operators,
            "Cap" -> {qs["OutputQudits"] + #, n + #} & /@ Range[qs["InputQudits"]]
        ]
    ];
    QuantumCircuitOperator[operators, opts, qs["Label"]]["Flatten"]
]

QuantumCircuitOperator["QuantumState", opts___] := QuantumCircuitOperator[{"QuantumState", QuantumState[{"UniformSuperposition", 3}]}, opts]


QuantumCircuitOperator["CHSH"] :=
    QuantumCircuitOperator[{
        "Cup" -> {1, 4},
        QuantumCircuitOperator[{"+" -> 2, "+" -> 3}, "Charlie"],
        "Barrier",
        "I",
        {"C", "H"} -> {2, 1},
        {"RY", Pi/4} -> 4,
        {"C0", "H"} -> {3, 4},
        "Barrier",
        {1, 2}, {3, 4}
    }]
