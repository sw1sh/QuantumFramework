Package["Wolfram`QuantumFramework`"]

PackageScope["$QuantumOperatorNames"]
PackageScope["pauliZGate"]
PackageScope["controlledZGate"]
PackageScope["FromOperatorShorthand"]



$QuantumOperatorNames = {
    "Identity", "I", "Permutation", "Curry", "Uncurry",
    "Fourier", "InverseFourier",
    "XRotation", "YRotation", "ZRotation", "U", "Phase", "P", "RX", "RY", "RZ", "R",
    "Diagonal", "GlobalPhase",
    "PhaseShift",
    "Shift", "ShiftPhase", "PhaseSpaceDisplacement", "PhasePoint", "Fano",
    "SUM", "RootNOT",
    "X", "Y", "Z", "PauliX", "PauliY", "PauliZ", "H", "Hadamard",
    "0", "1",
    "SWAP", "RootSWAP", "CSWAP", "Fredkin",
    "C", "Controlled", "C0", "Controlled0", "CX", "CY", "CZ", "CH", "CT", "CS", "CPHASE", "CNOT",
    "S", "T", "V",
    "Toffoli", "Deutsch",
    "RandomUnitary", "RandomHermitian",
    "Spider", "ZSpider", "XSpider",
    "Cup", "Cap",
    "Switch",
    "Discard",
    "Multiplexer",
    "WignerD", "JX", "JY", "JZ"
}


pauliZGate = SparseArray[{j_, j_} :> Exp[(2 Pi I j / 2) + I Pi], {2, 2}];

controlledZGate = ReplacePart[
    identityMatrix[4],
    Thread[
        Flatten[Table[{i, j}, {i, 4 - Length[pauliZGate] + 1, 4}, {j, 4 - Length[pauliZGate] + 1, 4}], {2, 1}] ->
        Flatten[pauliZGate]
    ]
];

$ShorthandOperatorPattern =
    _Rule |
    _String |
    _SuperDagger |
    ({name_String, ___} /; MemberQ[$QuantumOperatorNames, name]) |
    (c : g_Symbol[___] /; ! NumericQ[c] && MemberQ[Attributes[g], NumericFunction])

FromOperatorShorthand[f_Symbol[
    left___,
    op : $ShorthandOperatorPattern,
    right___]
] /; MemberQ[Attributes[f], NumericFunction] :=
    With[{qo = QuantumOperator[Unevaluated[op]]}, FromOperatorShorthand[Unevaluated[f[left, qo, right]]]]
FromOperatorShorthand[op_ ? QuantumFrameworkOperatorQ] := op
FromOperatorShorthand[order_ ? orderQ] := QuantumMeasurementOperator[order]
FromOperatorShorthand[name_] /; MemberQ[$QuantumOperatorNames, name] := QuantumOperator[name]
FromOperatorShorthand[name_] /; MemberQ[$QuantumChannelNames, name] := QuantumChannel[name]
FromOperatorShorthand[{name_, args___}] /; MemberQ[$QuantumOperatorNames, name] := QuantumOperator[{name, args}]
FromOperatorShorthand[name_ -> order_ ? autoOrderQ] /; MemberQ[$QuantumOperatorNames, name] := QuantumOperator[name, order]
FromOperatorShorthand[{name_, args___} -> order_ ? autoOrderQ] /; MemberQ[$QuantumOperatorNames, name] := QuantumOperator[{name, args}, order]
FromOperatorShorthand[name_ -> order_ ? autoOrderQ] /; MemberQ[$QuantumChannelNames, name] := QuantumChannel[name, order]
FromOperatorShorthand[({name_, args___} | name_) -> order_ ? autoOrderQ] /; MemberQ[$QuantumChannelNames, name] := QuantumChannel[{name, args}, order]
FromOperatorShorthand[(qc_ ? QuantumChannelQ) -> order_ ? autoOrderQ] := QuantumChannel[qc, order]
FromOperatorShorthand[lhs_ -> order_ ? autoOrderQ] := QuantumOperator[Unevaluated[lhs], order]
FromOperatorShorthand[lhs_ -> n_Integer] := FromOperatorShorthand[Unevaluated[lhs -> {n}]]
FromOperatorShorthand[lhs_ -> n : _Integer | _ ? orderQ -> m : _Integer | _ ? orderQ] := FromOperatorShorthand[Unevaluated[lhs -> {Flatten[{m}], Flatten[{n}]}]]
FromOperatorShorthand[(lhs_ -> rhs_) -> label : Except[OptionsPattern[]]] := FromOperatorShorthand[Unevaluated[(lhs -> rhs) -> ("Label" -> label)]]
FromOperatorShorthand[{name_, args___} -> rest_] /; MemberQ[$QuantumOperatorNames, name] := QuantumOperator[{name, args}, Sequence @@ Developer`ToList[rest]]
FromOperatorShorthand[{name_, args___} -> rest_] /; MemberQ[$QuantumChannelNames, name] := QuantumChannel[{name, args}, Sequence @@ Developer`ToList[rest]]
FromOperatorShorthand[lhs_ -> rest_] := QuantumOperator[Unevaluated[lhs], Sequence @@ Developer`ToList[rest]]
FromOperatorShorthand[args_List] := FromOperatorShorthand /@ args
FromOperatorShorthand[arg_] := QuantumOperator[arg]



QuantumOperator[] := QuantumOperator["Identity"]

QuantumOperator[arg : _String | {_String, ___}, n_Integer ? Positive, opts___] := QuantumOperator[arg, Range[n], opts]

QuantumOperator[arg_, inputOrder : _ ? orderQ | Automatic -> outputOrder : _ ? orderQ | Automatic, opts___] := QuantumOperator[arg, {outputOrder, inputOrder}, opts]

QuantumOperator["Identity" | "I", order : _ ? orderQ | Automatic, opts___] :=
    QuantumOperator[{"Identity", Table[2, If[order === Automatic, 1, Length[order]]]}, order, opts]

QuantumOperator[{"Identity" | "I", dims : {_Integer ? Positive ..}}, args___] := QuantumOperator[
    QuantumState[SparseArrayFlatten @ identityMatrix[Times @@ dims], QuantumBasis[QuditBasis[dims], QuditBasis[dims], "Label" -> "I"]],
    args
]

QuantumOperator[{"Identity" | "I", qb_ ? QuditBasisQ}, args___] := QuantumOperator[
    QuantumOperator[{"Identity", qb["FullDimensions"]}, args],
    "Output" -> qb, "Input" -> qb["Dual"]
]

QuantumOperator[{"Identity" | "I", params__}, opts___] :=
    Enclose @ QuantumOperator[{"Identity", ConfirmBy[QuditBasis[params], QuditBasisQ]}, opts]


QuantumOperator[name : "XRotation" | "YRotation" | "ZRotation" | "RX" | "RY" | "RZ", opts___] :=  QuantumOperator[{name, Pi / 2}, opts]

QuantumOperator[{"XRotation" | "RX", angle_, dimension : _Integer ? Positive : 2}, opts___] := QuantumOperator[
    QuantumOperator[
        Exp[- I angle / 2 QuantumOperator[{"PauliX", dimension}]],
        "Label" -> Subscript["R", "X"][angle]
    ],
    opts
]

QuantumOperator[{"YRotation" | "RY", angle_, dimension : _Integer ? Positive : 2}, opts___] := QuantumOperator[
    QuantumOperator[
        Exp[- I angle / 2 QuantumOperator[{"PauliY", dimension}]],
        "Label" -> Subscript["R", "Y"][angle]
    ],
    opts
]

QuantumOperator[{"ZRotation" | "RZ", angle_, dimension : _Integer ? Positive : 2}, opts___] := QuantumOperator[
    QuantumOperator[
        Exp[- I angle / 2 QuantumOperator[{"PauliZ", dimension}]],
        "Label" -> Subscript["R", "Z"][angle]
    ],
    opts
]

QuantumOperator[{"R", angle_, args__}, opts___] := Enclose @ Block[{ops = ConfirmBy[QuantumOperator[#], QuantumOperatorQ] & /@ {args}, op, orders},
    op = QuantumCircuitOperator[ops]["QuantumOperator", Method -> "Schrodinger"]["Sort"];
    orders = #["FullInputOrder"] & /@ ops;
    QuantumOperator[
        Exp[- I angle / 2 op],
        opts,
        "Label" -> Subscript["R", op["Label"]][angle]
    ]
]

QuantumOperator[{"U" | "U3", theta_, phi_ : Pi, lambda_ : Pi}, opts___] := QuantumOperator[
    QuantumOperator[
        {{Cos[theta / 2], - Exp[I lambda] Sin[theta / 2]}, {Exp[I phi] Sin[theta / 2], Exp[I * (phi + lambda)] Cos[theta / 2]}},
         "Label" -> "U"[theta, phi, lambda]
    ],
    opts
]

QuantumOperator[{"U2", phi_, lambda_}, opts___] := QuantumOperator[
    QuantumOperator[
        1 / Sqrt[2] {{1, - Exp[I lambda]}, {Exp[I phi], Exp[I * (phi + lambda)]}},
        "Label" -> "U2"[phi, lambda]
    ],
    opts
]

QuantumOperator["Phase" | "P" | "U1", opts___] := QuantumOperator[{"Phase", Pi}, opts]

QuantumOperator[{"Phase" | "P" | "U1", angle_, dimension : _Integer ? Positive : 2}, opts___] := QuantumOperator[
    QuantumOperator[
        SparseArray[{{i_, i_} /; i < dimension -> 1, {dimension, dimension} -> Exp[I angle]}],
        dimension,
        "Label" -> "P"[angle]
    ],
    opts
]

QuantumOperator["ShiftPhase", opts___] := QuantumOperator[{"ShiftPhase", 2}, opts]

QuantumOperator[{"ShiftPhase", dimension : _Integer ? Positive : 2}, opts___] := QuantumOperator[
    QuantumOperator[
        SparseArray[{{i_, i_} :> Exp[2 Pi I (i - 1) / dimension]}, {dimension, dimension}],
        dimension,
        "Label" -> "ShiftPhase"
    ],
    opts
]

QuantumOperator["PhaseShift", opts___] := QuantumOperator[{"PhaseShift", 1}, opts]

QuantumOperator[{"PhaseShift", k : _Integer | _Symbol : 1}, opts___] := QuantumOperator[
    QuantumOperator[{"Phase", Sign[k] 2 Pi / 2 ^ Abs[k]}, "Label" -> "PhaseShift"[k]],
    opts
]


QuantumOperator["GlobalPhase", opts___] := QuantumOperator[{"GlobalPhase", Pi}, opts]

QuantumOperator[{"GlobalPhase", angle_, dimension : _Integer ? Positive : 1}, opts___] := QuantumOperator[
    QuantumOperator[
        ReplacePart[identityMatrix[dimension], {i_, i_} -> Exp[I angle]],
        dimension,
        "Label" -> "GlobalPhase"[angle]
    ],
    opts
]

QuantumOperator[{"Diagonal", x_, dimension : _Integer ? Positive : 2}, opts___] := QuantumOperator[{"Diagonal", x, dimension}, {1}, opts]

QuantumOperator[{"Diagonal", x_, dimension : _Integer ? Positive : 2}, order_ ? orderQ, opts___] :=
    QuantumOperator[{"Diagonal", Table[x, dimension ^ Length[order]], dimension}, order, opts]

QuantumOperator[{"Diagonal", x_List, dimension : _Integer ? Positive : 2}, order_ ? orderQ, opts___] := With[{
    size = Ceiling[Log[dimension, Length[x]]]
},
    QuantumOperator[
        QuantumOperator[
            DiagonalMatrix[PadRight[x, dimension ^ Max[size, Length[order]]]],
            dimension,
            "Label" -> OverHat[If[MatchQ[x, {_}], First[x], x]]
        ],
        Join[order, Max[order] + Range[Max[size - Length[order], 0]]],
        opts
    ]
]


QuantumOperator["Shift", opts___] := QuantumOperator[{"Shift", 1, 2}, opts]

QuantumOperator[{"Shift", shift_Integer, dimension : _Integer ? Positive : 2}, opts___] := QuantumOperator[
    QuantumOperator[
        SparseArray[{{i_, j_} /; Mod[i - j - shift, dimension] == 0 -> 1}, {dimension, dimension}],
        dimension,
        "Label" -> "Shift"[shift]
    ],
    opts
]

QuantumOperator[{"PhaseSpaceDisplacement", i_Integer, j_Integer, dimension : _Integer ? Positive : 2}, opts___] :=
    Exp[- 2 Pi I i j / dimension] QuantumOperator[{"Shift", 1, dimension}] ^ i @ QuantumOperator[{"ShiftPhase", dimension}] ^ j

QuantumOperator[{"PhasePoint" | "Fano", i_Integer, j_Integer, dimension : _Integer ? Positive : 2}, opts___] :=
    Sum[Exp[- 2 Pi I (j k - i l)] QuantumOperator[{"PhaseSpaceDisplacement", k, l, dimension}, opts], {k, 0, dimension - 1}, {l, 0, dimension - 1}]


QuantumOperator["S", opts___] := QuantumOperator[QuantumOperator[{"Phase", Pi / 2}, "Label" -> "S"], opts]

QuantumOperator["T", opts___] := QuantumOperator[QuantumOperator[{"Phase", Pi / 4}, "Label" -> "T"], opts]

QuantumOperator[name : "V" | "SX", opts___] := QuantumOperator[QuantumOperator[Sqrt[QuantumOperator["X"]], "Label" -> "V"], opts]


QuantumOperator["CNOT", opts___] := QuantumOperator[{"CNOT", 2}, opts]

QuantumOperator[{"CNOT", dimension_Integer}, opts___] := QuantumOperator[{"Controlled", {"NOT", dimension} -> 2}, opts]


QuantumOperator["CPHASE" | "CP", opts___] := QuantumOperator[{"CPHASE", Pi}, opts]

QuantumOperator[{"CPHASE" | "CP", angle_, dimension : _Integer ? Positive : 2}, opts___] :=
    QuantumOperator[{"Controlled", {"Phase", angle, dimension} -> 2}, opts]


controlledMatrix[matrix_ ? MatrixQ, dimension_Integer] := ReplacePart[
    identityMatrix[dimension ^ 2],
    Thread[
        Flatten[
            Table[{i, j},
                {i, (dimension ^ 2) - Length[matrix] + 1, dimension ^ 2},
                {j, (dimension ^ 2) - Length[matrix] + 1, dimension ^ 2}
            ],
            {2, 1}
        ] -> Flatten[matrix]
    ]
]

QuantumOperator[name : "CX" | "CY" | "CZ", opts___] := QuantumOperator[{name, 2}, opts]

QuantumOperator[{"CX", dimension : _Integer ? Positive}, opts___] :=
    QuantumOperator[{"Controlled", {"PauliX", dimension} -> {2}}, opts]

QuantumOperator[{"CY", dimension : _Integer ? Positive}, opts___] :=
    QuantumOperator[{"Controlled", {"PauliY", dimension} -> {2}}, opts]

QuantumOperator[{"CZ", dimension : _Integer ? Positive}, opts___] :=
    QuantumOperator[{"Controlled", {"PauliZ", dimension} -> {2}}, opts]

QuantumOperator["CH", opts___] := QuantumOperator[{"Controlled", "H" ->{2}}, opts]

QuantumOperator["CT", opts___] := QuantumOperator[{"Controlled", "T" -> {2}}, opts]

QuantumOperator["CS", opts___] := QuantumOperator[{"Controlled", "S" -> {2}}, opts]


QuantumOperator[{"C" | "Controlled", args___, ctrl_Integer ? NonNegative, order : _ ? orderQ | Automatic : Automatic}, opts___] := Block[{
    controlSize, controlOrder
},
    controlSize = Max[IntegerLength[ctrl, 2], 1];
    controlOrder = Replace[order, Automatic :> Range[controlSize]];
    controlOrder = Join[controlOrder, Max[controlOrder] + Range[Max[controlSize - Length[controlOrder], 0]]];
    controlSize = Max[controlSize, Length[controlOrder]];
    QuantumOperator[{"C", args, Splice[controlOrder[[#]] & /@ Lookup[PositionIndex[IntegerDigits[ctrl, 2, controlSize]], {1, 0}, {}]]}, opts]
]

QuantumOperator[{"C" | "Controlled", qo : Except[_QuantumOperator], control1 : _ ? orderQ | {}, control0 : _ ? orderQ | {} : {}}, opts___] :=
    QuantumOperator[{"Controlled", QuantumOperator[qo, opts], control1, control0}]

QuantumOperator[{name : "C" | "Controlled", params : PatternSequence[___, Except[_ ? orderQ | {}]]}, order_ ? orderQ, opts___] :=
    Enclose @ With[{op = ConfirmBy[QuantumOperator[params, Replace[Rest @ order, {} -> {First @ order + 1}]], QuantumOperatorQ]},
        QuantumOperator[{name, op, {First @ order}}, opts]
    ]

QuantumOperator[{"C0" | "Controlled0", params : PatternSequence[___, Except[_ ? orderQ | {}]]}, order : _ ? orderQ, opts___] :=
    Enclose @ With[{op = ConfirmBy[QuantumOperator[params, Replace[Rest @ order, {} -> {First @ order + 1}]], QuantumOperatorQ]},
        QuantumOperator[{"Controlled", op, {}, {First @ order}}, opts]
    ]

QuantumOperator[{name : "C" | "Controlled", params : Shortest @ PatternSequence[Except[_QuantumOperator], ___], defaultControl : _ ? orderQ | Automatic : Automatic, control0 : _ ? orderQ | {} : {}}, opts___] :=
    Enclose @ Block[{op = ConfirmBy[QuantumOperator[params], QuantumOperatorQ], control},
        control = Replace[defaultControl, Automatic -> {First[Complement[Range[Max[op["InputOrder"]] + 1], op["InputOrder"]]]}];
        QuantumOperator[{"C", op, control, control0}, opts]
    ]

QuantumOperator[{"C0" | "Controlled0", params : Shortest @ PatternSequence[Except[_QuantumOperator], ___], defaultControl0 : _ ? orderQ | Automatic : Automatic}, opts___] :=
    Enclose @ Block[{op = ConfirmBy[QuantumOperator[params], QuantumOperatorQ], control0},
        control0 = Replace[defaultControl0, Automatic -> {First[Complement[Range[Max[op["InputOrder"]] + 1], op["InputOrder"]]]}];
        QuantumOperator[{"C", op, {}, control0}, opts]
    ]

QuantumOperator[{name : "C" | "Controlled" | "C0" | "Controlled0", qo_ ? QuantumOperatorQ, Automatic, control0 : _ ? orderQ : {}}, opts___] := With[{
    control = {First[Complement[Range[Max[qo["InputOrder"]] + 1], qo["InputOrder"]]]}
},
    QuantumOperator[{name, qo, control, control0}, opts]
]

QuantumOperator[{name : "C" | "Controlled", params : Shortest @ PatternSequence[Except[_QuantumOperator], ___], control : _ ? orderQ | {}, control0 : _ ? orderQ | {} : {}}, target_ ? orderQ, opts___] :=
    Enclose @ QuantumOperator[{name, ConfirmBy[QuantumOperator[params, target], QuantumOperatorQ], control, control0}, opts]

QuantumOperator[{name : "C0" | "Controlled0", params : Shortest @ PatternSequence[Except[_QuantumOperator], ___], control0 : _ ? orderQ | {} : {}}, target_ ? orderQ, opts___] :=
    Enclose @ QuantumOperator[{name, ConfirmBy[QuantumOperator[params, target], QuantumOperartorQ], control0}, opts]


QuantumOperator[{name : "C" | "Controlled" | "Controlled0", qo_ ? QuantumOperatorQ}, opts___] :=
    QuantumOperator[{name, qo, {First[Complement[Range[Max[qo["InputOrder"]] + 1], qo["InputOrder"]]]}}, opts]

QuantumOperator[{name : "C" | "Controlled" | "Controlled0", qo_ ? QuantumOperatorQ, control___}, target_ ? orderQ, opts___] :=
    QuantumOperator[{name, QuantumOperator[qo, target], control}, opts]

QuantumOperator[{"C0" | "Controlled0", qo_ ? QuantumOperatorQ, control0 : _ ? orderQ | {}}, opts___] := QuantumOperator[{"Controlled", qo, {}, control0}, opts]


QuantumOperator[{"C" | "Controlled", qo_ ? QuantumOperatorQ /; qo["ControlOrder"] =!= {}, control1 : _ ? orderQ | {}, control0 : _ ? orderQ | {} : {}}, opts___] :=
    QuantumOperator[{"Controlled", qo["TargetOperator"],
        Complement[Union[qo["ControlOrder1"], control1], control0],
        Complement[Union[qo["ControlOrder0"], control0], control1]
    }, opts]

QuantumOperator[{"C" | "Controlled", qo_ ? QuantumOperatorQ, control1 : _ ? orderQ | {}, control0 : _ ? orderQ | {} : {}}, opts___] := Enclose @ With[{
    controls1 = Length[control1],
    controls0 = Length[control0],
    control = Join[control0, control1]
},
    (*ConfirmAssert[! IntersectingQ[qo["Order"], control], "Target and control qudits shouldn't intersect"];*)
    QuantumOperator[
        blockDiagonalMatrix[{
            identityMatrix[(2 ^ controls1 - 1) qo["OutputDimension"]],
            qo["Matrix"],
            identityMatrix[(2 ^ controls0 - 1) 2 ^ controls1 qo["OutputDimension"]]
        }],
        With[{order = Join @@ NestWhile[
                Apply[
                    Block[{
                        lhs = Intersection[#1, #2], rhs},
                        rhs = Take[DeleteElements[Range[Min[#1, #2], Max[#1, #2] + Length[lhs]], #1], UpTo[Length[lhs]]];
                        {Join[#1, rhs], DeleteElements[#2, lhs]}
                    ] &
                ],
                {control, Sort[qo["InputOrder"]]},
                Apply[IntersectingQ]
            ]
        },
            {order, order}
        ],
        QuantumTensorProduct[
            QuantumBasis[QuditBasis[2, controls1], QuditBasis[2, controls1]],
            QuantumBasis[QuditBasis[2, controls0], QuditBasis[2, controls0]],
            qo["Basis"]
        ],
        opts,
        "Label" -> Subscript["C", qo["Label"]][control1, control0]
    ]
]


QuantumOperator[{"Multiplexer" | "BlockDiagonal", qos__}, opts___] := Block[{sorted = QuantumOperator[#]["Sort"] & /@ {qos}},
    QuantumOperator[
        QuantumOperator[
            blockDiagonalMatrix[#["MatrixRepresentation"] & /@ sorted],
            {Union @@ (#["OutputOrder"] & /@ sorted), Union @@ (#["InputOrder"] & /@ sorted)},
            Plus @@ (QuantumBasis[#["OutputDimensions"], #["InputDimensions"]] & /@ sorted)
        ],
        opts,
        "Label" -> CirclePlus @@ (#["Label"] & /@ sorted)
    ]["SetFullOrder"]
]

QuantumOperator[{"Multiplexer" | "BlockDiagonal", qos__}, order : _ ? autoOrderQ, opts___] := With[{
    op = QuantumOperator[{"Multiplexer", qos}, opts]
},
    QuantumOperator[op, order, QuantumBasis[Join[Table @@@ FactorInteger[op["OutputDimension"]]], Sequence @@ op["Basis"]["Meta"]]] /; op["OutputDimension"] == op["InputDimension"]
]


QuantumOperator["Fourier", opts___] := QuantumOperator[{"Fourier", 2}, opts]

QuantumOperator[{"Fourier", dimension : _Integer ? Positive}, order : (_ ? orderQ) : {1}, opts___] := QuantumOperator[
    QuantumOperator[
        SparseArray[
            ({i_, j_} :>  Exp[2 Pi I (i - 1) (j - 1) / (dimension ^ Length[order])] / Sqrt[dimension ^ Length[order]]),
            {dimension ^ Length[order], dimension ^ Length[order]}
        ],
        dimension,
        Length[order]
    ],
    {order, order},
    opts,
    "Label" -> "QFT"
]

QuantumOperator["InverseFourier", opts___] := QuantumOperator[{"InverseFourier", 2}, opts]

QuantumOperator[{"InverseFourier", dimension : _Integer ? Positive}, order : (_ ? orderQ) : {1}, opts___] := QuantumOperator[
    QuantumOperator[
        SparseArray[
            ({i_, j_} :> Exp[-2 Pi I (i - 1) (j - 1) / (dimension ^ Length[order])] / Sqrt[dimension ^ Length[order]]),
            {dimension ^ Length[order], dimension ^ Length[order]}
        ],
        dimension,
        Length[order]
    ],
    {order, order},
    opts,
    "Label" -> SuperDagger["QFT"]
]


swapMatrix[dimension_] := SparseArray[# -> 1 & /@
    Thread[{
        Range[dimension ^ 2],
        Flatten @ Table[j + i - 1, {i, dimension}, {j, 1, dimension ^ 2, dimension}]
    }]
]

QuantumOperator["SWAP", opts___] := QuantumOperator[{"SWAP", 2}, opts]

QuantumOperator[{"SWAP", dimension : _Integer ? Positive}, opts___] :=
    QuantumOperator[QuantumOperator[{"Permutation", {dimension, dimension}, Cycles[{{1, 2}}]}, opts], "Label" -> "SWAP"]

QuantumOperator["RootSWAP", opts___] := QuantumOperator[{"RootSWAP", 2}, opts]

QuantumOperator[{"RootSWAP", dimension : _Integer ? Positive}, opts___] :=
    QuantumOperator[Sqrt[QuantumOperator[{"SWAP", dimension}, opts]], "Label" -> "RootSWAP"]


QuantumOperator["SUM", opts___] := QuantumOperator[{"SUM", 2}, opts]

QuantumOperator[{"SUM", dimension : _Integer ? Positive}, opts___] := QuantumOperator[
    QuantumOperator[
        SparseArray[{input_, output_} :>
            With[{
                i1 = First[IntegerDigits[input - 1, dimension, 2]],
                j1 = IntegerDigits[input - 1, dimension, 2][[2]],
                i2 = First[IntegerDigits[output - 1, dimension, 2]],
                j2 = IntegerDigits[output - 1, dimension, 2][[2]]
            },
            If[i1 == i2 && j2 == Mod[i1 + j1, dimension], 1, 0]
            ],
            {dimension ^ 2, dimension ^ 2}
        ],
        dimension,
        2,
        "Label" -> "SUM"
    ],
    opts
]


QuantumOperator[name : "X" | "Y" | "Z" | "PauliX" | "PauliY" | "PauliZ" | "NOT", opts___] := QuantumOperator[{name, 2}, opts]

QuantumOperator[{"PauliX" | "X", dimension : _Integer ? Positive}, opts___] := QuantumOperator[
    QuantumOperator[pauliMatrix[1, dimension], dimension, "Label" -> "X"],
    opts
]

QuantumOperator[{"PauliY" | "Y", dimension : _Integer ? Positive}, opts___] := QuantumOperator[
    QuantumOperator[pauliMatrix[2, dimension], dimension, "Label" -> "Y"],
    opts
]

QuantumOperator[{"PauliZ" | "Z", dimension : _Integer ? Positive}, opts___] := QuantumOperator[
    QuantumOperator[pauliMatrix[3, dimension], dimension, "Label" -> "Z"],
    opts
]

QuantumOperator["0", opts___] := QuantumOperator[- "Z", opts, "Label" -> "0"]

QuantumOperator["1", opts___] := QuantumOperator["Z", opts, "Label" -> "1"]

QuantumOperator[{"NOT", dimension : _Integer ? Positive}, opts___] := QuantumOperator[QuantumOperator[{"X", dimension}, "Label" -> "NOT"], opts]


QuantumOperator["RootNOT", opts___] := QuantumOperator[{"RootNOT", 2}, opts]

QuantumOperator[{"RootNOT", dimension : _Integer ? Positive}, opts___] := QuantumOperator[
    QuantumOperator[
        MatrixPower[
            SparseArray[({i_, j_} /; Mod[i - 1, dimension, 1] == j) -> 1, {dimension, dimension}],
            1 / 2
        ],
        dimension,
        "Label" -> Sqrt["NOT"]
    ],
    opts
]


QuantumOperator["Hadamard" | "H", order_ ? orderQ, opts___] :=
    QuantumOperator[
        HadamardMatrix[2 ^ Length[order], Method -> "BitComplement"],
        {order, order},
        opts,
        "Label" -> If[Length[order] > 1, Superscript["H", CircleTimes[Length[order]]], "H"]
    ]


QuantumOperator["Toffoli", order : (_ ? orderQ) : {1, 2, 3}] :=
    QuantumOperator[{"Controlled", "NOT", Most[order]}, {Last[order]}]


QuantumOperator["CSWAP" | "Fredkin", opts___] := QuantumOperator[{"Controlled", "SWAP" -> {2, 3}}, opts]



QuantumOperator["RandomUnitary", order : {outputOrder_ ? orderQ, inputOrder_ ? orderQ}, opts___] := Enclose @
    QuantumOperator[{"RandomUnitary", ConfirmBy[QuantumBasis[QuditBasis[2, Length[outputOrder]], QuditBasis[2, Length[inputOrder]], opts, "Label" -> None], QuantumBasisQ]}, order]


QuantumOperator["RandomUnitary", order : (_ ? orderQ) : {1}, opts___] := Enclose @
    QuantumOperator["RandomUnitary", {order, order}, opts]

QuantumOperator[{"RandomUnitary", qb_ ? QuantumBasisQ}, order : (_ ? autoOrderQ), opts___] :=
    QuantumOperator[
        Which[
            qb["InputDimension"] == 1,
            RandomVariate @ CircularUnitaryMatrixDistribution[qb["OutputDimension"]],
            IntegerQ[Sqrt[qb["Dimension"]]],
            RandomVariate @ CircularUnitaryMatrixDistribution[Sqrt[qb["Dimension"]]],
            True,
            QuantumState["RandomPure", qb["Dimensions"]]["Split", qb["OutputQudits"]]
        ],
        order, qb, opts
    ]

QuantumOperator[{"RandomUnitary", args___}, order : (_ ? autoOrderQ) : {1}, opts___] := Enclose @
    QuantumOperator[{"RandomUnitary", ConfirmBy[QuantumBasis[args], QuantumBasisQ]}, order, opts]


QuantumOperator["RandomHermitian", order : (_ ? orderQ) : {1}, opts___] := Enclose @
    QuantumOperator[{"RandomHermitian", ConfirmBy[QuantumBasis[2, Length[order], opts], QuantumBasisQ]}, order]

QuantumOperator[{"RandomHermitian", qb_ ? QuantumBasisQ}, order : (_ ? orderQ) : {1}, opts___] :=
    QuantumState["RandomMixed", QuantumBasis[qb, opts]]["Operator", order]

QuantumOperator[{"RandomHermitian", args___}, order : (_ ? orderQ) : {1}] := Enclose @
    QuantumOperator[{"RandomHermitian", ConfirmBy[QuantumBasis[args], QuantumBasisQ]}, order]


QuantumOperator["Permutation", opts___] := QuantumOperator[{"Permutation", 2, Cycles[{{1, 2}}]}, opts]

QuantumOperator[{"Permutation", perm_Cycles}, opts___] := QuantumOperator[{"Permutation", 2, perm}, opts]

QuantumOperator[{"Permutation", dims : {_Integer ? Positive..} | Automatic : Automatic, perm_List}, opts___] :=
    QuantumOperator[{"Permutation", Replace[dims, Automatic :> ConstantArray[2, Length[perm]]], PermutationCycles[perm]}, opts]

QuantumOperator[{"Permutation", dim : _Integer ? Positive, perm_Cycles}, opts___] := QuantumOperator[{"Permutation", Table[dim, Max[PermutationMax[perm], 1]], perm}, opts]

QuantumOperator[{"Permutation", dims : {_Integer ? Positive..}, perm_Cycles}, opts___] :=
    QuantumOperator[
        QuantumState[
            SparseArrayFlatten @ TensorTranspose[ArrayReshape[identityMatrix[Times @@ dims], Join[dims, dims]], perm],
            QuantumBasis[QuditBasis[Permute[dims, perm]], QuditBasis[dims], "Label" -> "\[Pi]" @@ PermutationList[perm, Length[dims]]]
        ],
        opts
    ]

QuantumOperator["Uncurry", opts___] := QuantumOperator[{"Uncurry", 2}, opts]

QuantumOperator[{"Uncurry", dim : _Integer ? Positive ..}, opts___] := QuantumOperator[{"Uncurry", {dim, dim}}, opts]

QuantumOperator[{"Uncurry", dims : {_Integer ? Positive ..}}, opts___] :=
    QuantumOperator[
        QuantumOperator[identityMatrix[Times @@ dims], QuantumBasis[QuditBasis[Times @@ dims], QuditBasis[dims]]],
        opts
    ]

QuantumOperator[name : "Curry" | {"Curry", ___}, opts___] := QuantumOperator[QuantumOperator[name /. "Curry" -> "Uncurry"]["ConjugateTranspose"], opts]


QuantumOperator[(name : "XSpider" | "YSpider" | "ZSpider" | "Spider"), opts___] := QuantumOperator[{name}, opts]


QuantumOperator[{name : "XSpider" | "YSpider" | "ZSpider", phase_ : 0}, order : {outputOrder : _ ? orderQ, inputOrder : _ ? orderQ}, opts___] := QuantumOperator[{
        "Spider",
        QuantumBasis[
            QuditBasis[StringTake[name, 1], Length[outputOrder]],
            QuditBasis[StringTake[name, 1], Length[inputOrder]]
        ],
        phase
    },
    order,
    opts,
    "Label" -> name[phase]
]


QuantumOperator["Spider", opts___] := QuantumOperator[{"Spider", QuantumBasis[QuditBasis[2], QuditBasis[2]]}, opts]

QuantumOperator[{"Spider", basis_ ? QuantumBasisQ, phase_ : 0}, opts___] /;
    Equal @@ basis["Dimensions"] || basis["OutputDimension"] == basis["InputDimension"] == 1 := Block[{
    phases, dim
},
    dim = First[basis["Dimensions"], 2];
    phases = PadRight[Flatten[{phase}], dim];
    QuantumOperator[
        QuantumState[
            If[ basis["Dimension"] <= 1,
                {Exp[I First[phases, 0]]},
                SparseArrayFlatten @ SparseArray[Thread[Transpose[Table[Range[dim], basis["Qudits"]]] -> Exp[I phases]], basis["Dimensions"]]
            ],
            basis
        ],
        opts
    ]
]


QuantumOperator[({name : "XSpider" | "YSpider" | "ZSpider" | "Spider", args___}) | (name : "XSpider" | "YSpider" | "ZSpider" | "Spider"), order : _ ? orderQ : {1}, opts___] :=
    QuantumOperator[{name, args}, {order, order}, opts]


QuantumOperator["Cup" | {"Cup", dim : _Integer ? Positive : 2}, order : _ ? orderQ : {1, 2}, opts___] /; Length[order] == 2 :=
    QuantumOperator[QuantumOperator[{"I", dim}]["SplitDual", 2], {order, {}}, "Label" -> "Cup", opts]

QuantumOperator["Cap" | {"Cap", dim : _Integer ? Positive : 2}, order : _ ? orderQ : {1, 2}, opts___] /; Length[order] == 2 :=
    QuantumOperator[QuantumOperator[{"I", dim}]["SplitDual", 0], {{}, order}, "Label" -> "Cap", opts]


QuantumOperator[{"Deutsch", theta_}, order : _ ? orderQ : {1, 2, 3}] := With[{
    controlOrder = PadRight[Most[order], 2, Range[2] + Max[order]],
    targetOrder = {Last[order]}
},
    QuantumOperator[{
        "Controlled",
        QuantumOperator[I QuantumOperator[{"XRotation", theta}, targetOrder], "Label" -> "D"[theta]],
        controlOrder
    }]
]


QuantumOperator[{"Switch", a_ ? QuantumOperatorQ, b_ ? QuantumOperatorQ}, order : _ ? orderQ : {1, 2}] /;
    a["InputDimension"] == a["OutputDimension"] == b["InputDimension"] == b["OutputDimension"] && Length[order] == 2 :=
QuantumPartialTrace[
	QuantumOperator[
        QuantumOperator[{"Controlled0", "SWAP"}, Prepend[order, Max[order] + 1]] @
        QuantumOperator[b, order[[{2}]], order[[{2}]]] @
        QuantumOperator[a, order[[{1}]], order[[{1}]]] @
        QuantumOperator["CSWAP", Prepend[order, Max[order] + 1]],
        "Label" -> "\[ScriptCapitalS]"[a["Label"], b["Label"]]
    ],
	{Max[order] + 1}
]


QuantumOperator["Discard", order : _ ? orderQ : {1}, args___] := With[{basis = QuantumBasis[args]},
    QuantumOperator[
        QuantumState["UniformSuperposition",
            If[ basis["Qudits"] < Length[order],
                QuantumBasis[basis, Ceiling[Length[order] / Max[1, basis["Qudits"]]]],
                basis
            ]
        ]["Dagger"],
        {{}, order},
        "Label" -> "Discard"
    ]
]


QuantumOperator["HeisenbergWeyl", opts___] := QuantumOperartor[{"HeisenbergWeyl", 2}, opts]

QuantumOperator[{"HeisenbergWeyl", p_Integer ? Positive, i_Integer : 0, a_ : \[FormalA]}, order : _ ? orderQ : {1}, opts___] := QuantumOperator[
    QuantumOperator[
        With[{d = p ^ Length[order]},
            Total @ Table[Exp[I 2 Pi a l / d] KroneckerProduct[UnitVector[d, Mod[i + l, d] + 1], UnitVector[d, l + 1]] , {l, 0, d - 1}]
        ],
        p,
        Length[order]
    ],
    {order, order},
    opts,
    "Label" -> "HeisenbergWeyl"
]

wignerD[j_, {a_, b_, c_}] := Table[WignerD[{j, m1, m2}, a, b, c], {m1, -j, j}, {m2, -j, j}]

wignerD[j_, b_] := Table[WignerD[{j, m1, m2}, b], {m1, -j, j}, {m2, -j, j}]

jUp[j_] := Table[Sqrt[(j - m2) (j + m2 + 1)] KroneckerDelta[m1, m2 + 1], {m2, -j, j}, {m1, -j, j}]

jDown[j_] := Table[Sqrt[(j + m2) (j - m2 + 1)] KroneckerDelta[m1, m2 - 1], {m2, -j, j}, {m1, -j, j}]

jX[j_] := 1 / 2 (jUp[j] + jDown[j])

jY[j_] := 1 / (2 I) (jUp[j] - jDown[j])

jZ[j_] := DiagonalMatrix[Table[m, {m, j, -j, -1}]]

QuantumOperator[{"WignerD", j_, {a_, b_, c_}}, arg___] :=  QuantumOperator[wignerD[j, {a, b, c}], arg, QuantumBasis[2 j + 1]]

QuantumOperator[{"WignerD", j_, b_}, arg___] := QuantumOperator[wignerD[j, b], arg, QuantumBasis[2 j + 1]]

QuantumOperator[{"JX" | "AngularMomentumX", j_}, arg___] := QuantumOperator[jX[j], arg, QuantumBasis[2 j + 1]]

QuantumOperator[{"JY" | "AngularMomentumY", j_}, arg___] := QuantumOperator[jY[j], arg, QuantumBasis[2 j + 1]]

QuantumOperator[{"JZ" | "AngularMomentumZ", j_}, arg___] := QuantumOperator[jZ[j], arg, QuantumBasis[2 j + 1]]


QuantumOperator[chain_String, opts___] := With[{chars = Characters[chain]},
    QuantumOperator[QuantumTensorProduct[MapIndexed[QuantumOperator, chars]], opts] /;
        ContainsOnly[chars, {"I", "X", "Y", "Z", "H", "S", "T", "V", "P"}]
]


$upperCasesOperatorNames := AssociationThread[ToUpperCase @ $QuantumOperatorNames, $QuantumOperatorNames]

QuantumOperator[name_String, opts___] /; ToUpperCase[name] =!= name && KeyExistsQ[$upperCasesOperatorNames, name] :=
    QuantumOperator[$upperCasesOperatorNames[name], opts]

QuantumOperator[{name_String, params___}, opts___] /; ToUpperCase[name] =!= name && KeyExistsQ[$upperCasesOperatorNames, name] :=
    QuantumOperator[{$upperCasesOperatorNames[name], params}, opts]

QuantumOperator[rule : _Rule, opts___] := QuantumOperator[FromOperatorShorthand[Unevaluated[rule]], opts]

QuantumOperator[f_Symbol[args___], opts___] /; MemberQ[Attributes[f], NumericFunction] := QuantumOperator[FromOperatorShorthand[Unevaluated[f[args]]], opts]

QuantumOperator[ops : {Except[_QuantumOperator], ___}, opts___] /; AllTrue[ops, MatchQ[_Rule | _Integer | _String | ({name_, ___} /; MemberQ[$QuantumOperatorNames, name]) | _QuantumOperator]] :=
    Enclose @ QuantumOperator[
        QuantumCircuitOperator[Flatten[ConfirmBy[FromOperatorShorthand[#], QuantumOperatorQ] & /@ ops]]["QuantumOperator", Method -> "Schrodinger"],
        opts
    ]

QuantumOperator[SuperDagger[arg_], opts___] := QuantumOperator[arg, opts]["Dagger"]

