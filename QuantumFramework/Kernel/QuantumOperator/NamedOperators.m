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
    "SWAP", "RootSWAP", "CSWAP", "Fredkin",
    "C", "Controlled", "C0", "Controlled0", "CX", "CY", "CZ", "CH", "CT", "CS", "CPHASE", "CNOT",
    "S", "T", "V",
    "Toffoli", "Deutsch",
    "RandomUnitary", "RandomHermitian",
    "Spider", "ZSpider", "XSpider",
    "Switch",
    "Discard",
    "Multiplexer"
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
    (g_Symbol[___] /; MemberQ[Attributes[g], NumericFunction])

FromOperatorShorthand[f_Symbol[
    left___,
    op : $ShorthandOperatorPattern,
    right___]
] /; MemberQ[Attributes[f], NumericFunction] :=
    With[{qo = QuantumOperator[Unevaluated[op]]}, FromOperatorShorthand[Unevaluated[f[left, qo, right]]]]
FromOperatorShorthand[op_ ? QuantumFrameworkOperatorQ] := op
FromOperatorShorthand[order_ ? orderQ] := QuantumMeasurementOperator[order]
FromOperatorShorthand[name_] /; MemberQ[$QuantumOperatorNames, name] := QuantumOperator[name]
FromOperatorShorthand[{name_, args___}] /; MemberQ[$QuantumOperatorNames, name] := QuantumOperator[{name, args}]
FromOperatorShorthand[{name_, args___} -> order_ ? orderQ] /; MemberQ[$QuantumOperatorNames, name] := QuantumOperator[{name, args}, order]
FromOperatorShorthand[{name_, args___} -> rest_] /; MemberQ[$QuantumOperatorNames, name] := QuantumOperator[{name, args}, Sequence @@ Developer`ToList[rest]]
FromOperatorShorthand[lhs_ -> order_ ? orderQ] := QuantumOperator[QuantumOperator[Unevaluated[lhs]], order]
FromOperatorShorthand[lhs_ -> n_Integer] := FromOperatorShorthand[Unevaluated[lhs -> {n}]]
FromOperatorShorthand[lhs_ -> rest_] := QuantumOperator[Unevaluated[lhs], Sequence @@ Developer`ToList[rest]]
FromOperatorShorthand[args_List] := FromOperatorShorthand /@ args
FromOperatorShorthand[arg_] := QuantumOperator[arg]

(* QuantumOperator[name_ ? nameQ, basisName : Except[Alternatives @@ $QuantumBasisPictures, _ ? nameQ]] :=
    QuantumOperator[QuantumOperator[name], QuantumBasis[basisName]] *)


QuantumOperator[] := QuantumOperator["Identity"]

QuantumOperator["Identity" | "I", order : _ ? orderQ | Automatic : Automatic, opts___] :=
    QuantumOperator[{"Identity", Table[2, If[order === Automatic, 1, Length[order]]]}, order, opts]

QuantumOperator[{"Identity" | "I", dims : {_Integer ? Positive ..}}, order : _ ? orderQ | Automatic : Automatic, opts___] := QuantumOperator[
    QuantumState[SparseArrayFlatten @ identityMatrix[Times @@ dims], QuantumBasis[QuditBasis[dims], QuditBasis[dims], "Label" -> "I"]],
    {Sort[#], #} & @ Replace[order, Automatic -> Range[Length[dims]]],
    opts
]

QuantumOperator[{"Identity" | "I", qb_ ? QuditBasisQ}, opts___] := QuantumOperator[
    QuantumOperator[{"Identity", qb["Dimensions"]}, opts],
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
        SparseArray[{{i_, i_} -> Exp[2 Pi I (i - 1) / dimension]}, {dimension, dimension}],
        dimension,
        "Label" -> "ShiftPhase"
    ],
    opts
]

QuantumOperator["PhaseShift", opts___] := QuantumOperator[{"PhaseShift", 1}, opts]

QuantumOperator[{"PhaseShift", k : _Integer | _Symbol : 1}, opts___] := QuantumOperator[
    QuantumOperator[{"Phase", 2 Pi / 2 ^ k}, "Label" -> "PhaseShift"[k]],
    opts
]


QuantumOperator["GlobalPhase", opts___] := QuantumOperator[{"GlobalPhase", Pi}, opts]

QuantumOperator[{"GlobalPhase", angle_, dimension : _Integer ? Positiver : 2}, opts___] := QuantumOperator[{"Diagonal", Exp[I angle], dimension}, opts]

QuantumOperator[{"Diagonal", x_, dimension : _Integer ? Positive : 2}, opts___] := QuantumOperator[
    QuantumOperator[
        ReplacePart[identityMatrix[dimension], {i_, i_} -> x],
        dimension,
        "Label" -> OverHat[x]
    ],
    opts
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

QuantumOperator[name : "V" | "SX", opts___] := QuantumOperator[QuantumOperator[Sqrt[QuantumOperator["X"]], "Label" -> name], opts]


QuantumOperator["CNOT", opts___] := QuantumOperator[{"CNOT", 2}, opts]

QuantumOperator[{"CNOT", dimension_Integer}, opts___] := QuantumOperator[{"Controlled", {"NOT", dimension} -> 2}, opts]


QuantumOperator["CPHASE" | "CP", opts___] := QuantumOperator[{"CPHASE", Pi}, opts]

QuantumOperator[{"CPHASE" | "CP", angle_, dimension : _Integer ? Positive : 2}, opts___] := QuantumOperator[{"Controlled", {"Phase", angle, dimension} -> 2}, opts]


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
    control = Join[control1, control0]
},
    (*ConfirmAssert[! IntersectingQ[qo["Order"], control], "Target and control qudits shouldn't intersect"];*)
    QuantumOperator[
        blockDiagonalMatrix[{
            identityMatrix[(2 ^ controls1 - 1) qo["OutputDimension"]],
            qo["Matrix"],
            identityMatrix[(2 ^ controls0 - 1) 2 ^ controls1 qo["OutputDimension"]]
        }],
        With[{order = Join[
            control0,
            control1,
            qo["InputOrder"] /. NestWhile[
                Apply[
                    Block[{lhs = Intersection[#1, #2], rhs},
                        rhs = Take[DeleteCases[Range[Min[#1, #2], Max[#1, #2] + Length[lhs]], Alternatives @@ #2], UpTo[Length[lhs]]];
                        {DeleteCases[#1, Alternatives @@ lhs], rhs, Join[#3, Thread[lhs -> rhs]]}
                    ] &
                ],
                {qo["InputOrder"], control, {}},
                Apply[IntersectingQ[#1, #2] &]
            ][[3]]
        ]},
            {order, order}
        ],
        QuantumTensorProduct[
            QuantumBasis[QuditBasis[2, controls0], QuditBasis[2, controls0]],
            QuantumBasis[QuditBasis[2, controls1], QuditBasis[2, controls1]],
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

QuantumOperator[{"Multiplexer" | "BlockDiagonal", qos__}, order : _ ? orderQ, opts___] := With[{
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


QuantumOperator["Hadamard" | "H", order : _ ? orderQ : {1}, opts___] := QuantumOperator[{"H", Length @ order}, order, opts]

QuantumOperator[{"Hadamard" | "H", qudits_Integer ? Positive}, opts : PatternSequence[] | PatternSequence[Except[_ ? orderQ], ___]] :=
    QuantumOperator[{"H", qudits}, Range[qudits], opts]

QuantumOperator[{"Hadamard" | "H", qudits_Integer ? Positive}, order_ ? orderQ, opts___] :=
    QuantumOperator[
        HadamardMatrix[2 ^ qudits, Method -> "BitComplement"],
        {order, order},
        opts,
        "Label" -> If[qudits > 1, Superscript["H", CircleTimes[qudits]], "H"]
    ]


QuantumOperator["Toffoli", order : (_ ? orderQ) : {1, 2, 3}] :=
    QuantumOperator[{"Controlled", "NOT", Most[order]}, {Last[order]}]


QuantumOperator["CSWAP" | "Fredkin", opts___] := QuantumOperator[{"Controlled", "SWAP" -> {2, 3}}, opts]



QuantumOperator["RandomUnitary", order : (_ ? orderQ) : {1}, opts___] := Enclose @
    QuantumOperator[{"RandomUnitary", ConfirmBy[QuantumBasis[2, Length[order], opts, "Label" -> None], QuantumBasisQ]}, order]

QuantumOperator[{"RandomUnitary", qb_ ? QuantumBasisQ}, order : (_ ? orderQ) : {1}, opts___] :=
With[{
    padOrder = Join[order, Complement[Min[order] - 1 + Range[qb["FullQudits"]], order]]
},
    QuantumOperator[
           RandomVariate @ CircularUnitaryMatrixDistribution[qb["Dimension"]], order, qb, opts
    ]
]

QuantumOperator[{"RandomUnitary", args___}, order : (_ ? orderQ) : {1}, opts___] := Enclose @
    QuantumOperator[{"RandomUnitary", ConfirmBy[QuantumBasis[args], QuantumBasisQ]}, order, opts]


QuantumOperator["RandomHermitian", order : (_ ? orderQ) : {1}, opts___] := Enclose @
    QuantumOperator[{"RandomHermitian", ConfirmBy[QuantumBasis[2, Length[order], opts], QuantumBasisQ]}, order]

QuantumOperator[{"RandomHermitian", qb_ ? QuantumBasisQ}, order : (_ ? orderQ) : {1}, opts___] :=
With[{
    padOrder = Join[order, Complement[Min[order] - 1 + Range[qb["FullQudits"]], order]]
},
    QuantumState["RandomMixed", qb]["Operator", opts]
]

QuantumOperator[{"RandomHermitian", args___}, order : (_ ? orderQ) : {1}] := Enclose @
    QuantumOperator[{"RandomHermitian", ConfirmBy[QuantumBasis[args], QuantumBasisQ]}, order]


QuantumOperator["Permutation", opts___] := QuantumOperator[{"Permutation", 2, Cycles[{{1, 2}}]}, opts]

QuantumOperator[{"Permutation", perm_Cycles}, opts___] := QuantumOperator[{"Permutation", 2, perm}, opts]

QuantumOperator[{"Permutation", perm_List}, opts___] := QuantumOperator[{"Permutation", PermutationCycles[perm]}, opts]

QuantumOperator[{"Permutation", dim : _Integer ? Positive, perm_Cycles}, opts___] := QuantumOperator[{"Permutation", Table[dim, PermutationMax[perm]], perm}, opts]

QuantumOperator[{"Permutation", dims : {_Integer ? Positive..}, perm_Cycles}, opts___] :=
    QuantumOperator[
        QuantumState[
            SparseArrayFlatten @ TensorTranspose[ArrayReshape[identityMatrix[Times @@ dims], Join[dims, dims]], perm],
            QuantumBasis[QuditBasis[Permute[dims, perm]], QuditBasis[dims], "Label" -> "\[Pi]" @@ PermutationList[perm]]
        ],
        opts
    ]

QuantumOperator["Uncurry", opts___] := QuantumOperator[{"Uncurry", {2, 2}}, opts]

QuantumOperator[{"Uncurry", dims : {_Integer ? Positive ..}}] := QuantumOperator[{"Uncurry", dims}, {1}, Range[Length[dims]]]

QuantumOperator[{"Uncurry", dims : {_Integer ? Positive ..}}, opts___] :=
    QuantumOperator[
        QuantumOperator[identityMatrix[Times @@ dims], QuantumBasis[QuditBasis[Times @@ dims], QuditBasis[dims]]],
        opts
    ]


QuantumOperator[name : "Curry" | {"Curry", ___}, opts___] := QuantumOperator[name /. "Curry" -> "Uncurry", opts]["ConjugateTranspose"]


QuantumOperator[name : "ZSpider" | "XSpider" | "Spider", opts___] := QuantumOperator[{name}, opts]

QuantumOperator[{"ZSpider", out : _Integer ? Positive : 1, in : _Integer ? Positive : 1, phase_ : 0}, opts___] := Module[{
    phases = If[ListQ[phase], phase, {0, phase}],
    dim, basis
},
    dim = Length[phases];
    basis = QuantumBasis[QuditBasis[{"PauliZ", dim}, out], QuditBasis[{"PauliZ", dim}, in], "Label" -> "ZSpider"];
    QuantumOperator[
        QuantumState[
            If[ basis["Dimension"] <= 1,
                {Exp[I Last[phases]]}[[;; basis["Dimension"]]],
                SparseArrayFlatten @ SparseArray[Thread[Transpose[Table[Range[dim], basis["Qudits"]]] -> Exp[I phases]], basis["Dimensions"]]
            ],
            basis
        ],
        opts
    ]
]


QuantumOperator[{"XSpider", params___}, opts___] := With[{
    zSpider = QuantumOperator[{"ZSpider", params}, opts]
},
    QuantumOperator[
        QuantumOperator[zSpider, QuantumBasis[
            QuditBasis[{"PauliX", First[zSpider["Dimensions"], 1]}, zSpider["OutputQudits"]],
            QuditBasis[{"PauliX", First[zSpider["Dimensions"], 1]}, zSpider["InputQudits"]]]
        ]["Matrix"],
        zSpider["Order"],
        zSpider["Basis"],
        "Label" -> "XSpider"
    ]
]


QuantumOperator["Spider", opts___] := QuantumOperator[{"Spider", QuantumBasis[QuditBasis[2], QuditBasis[2]]}, opts]

QuantumOperator[{"Spider", basis_ ? QuantumBasisQ, phase_ : 0}, opts___] /;
    Equal @@ basis["Dimensions"] || basis["OutputDimension"] == basis["InputDimension"] == 1 :=
With[{
    zSpider = QuantumOperator[
        {"ZSpider", basis["OutputQudits"], basis["InputQudits"], PadLeft[If[ListQ[phase], phase, {phase}], First @ basis["OutputDimensions"]]},
        opts
    ]
},
    QuantumOperator[
        QuantumOperator[zSpider, basis]["Matrix"],
        zSpider["Order"],
        zSpider["Basis"],
        "Label" -> basis["Label"]
    ]
]


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
    {}, order
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

