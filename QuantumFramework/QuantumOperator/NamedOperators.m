Package["QuantumFramework`"]

PackageScope["pauliZGate"]
PackageScope["controlledZGate"]



pauliZGate = SparseArray[{j_, j_} :> Exp[(2 Pi I j / 2) + I Pi], {2, 2}];

controlledZGate = ReplacePart[
    IdentityMatrix[4],
    Thread[
        Flatten[Table[{i, j}, {i, 4 - Length[pauliZGate] + 1, 4}, {j, 4 - Length[pauliZGate] + 1, 4}], {2, 1}] ->
        Flatten[pauliZGate]
    ]
];

QuantumOperator["Identity", args___] := QuantumOperator[{"Identity", 2}, args]

QuantumOperator[{"Identity", dimension_Integer}, args___] := QuantumOperator[IdentityMatrix[dimension], dimension, args]


QuantumOperator[{"XRotation", angle_}, args___] := QuantumOperator[
    {{Cos[angle / 2], I Sin[angle / 2]}, {I Sin[angle / 2], Cos[angle / 2]}},
    "Label" -> Superscript["X", angle],
    args
]

QuantumOperator[{"YRotation", angle_}, args___] := QuantumOperator[
    {{Cos[angle / 2], - Sin[angle / 2]}, {Sin[angle / 2], Cos[angle / 2]}},
    "Label" -> Superscript["Y", angle],
    args
]

QuantumOperator[{"ZRotation", angle_}, args___] := QuantumOperator[
    SparseArray[{{1, 1} -> 1, {2, 2} -> Exp[I angle]}],
    "Label" -> Superscript["Z", angle],
    args
]


QuantumOperator["S", args___] := QuantumOperator[{"ZRotation", Pi / 2}, "Label" -> "S", args]

QuantumOperator["T", args___] := QuantumOperator[{"ZRotation", Pi / 4}, "Label" -> "T", args]


QuantumOperator["CNOT", args___] := QuantumOperator[{"CNOT", 2}, args]

QuantumOperator[{"CNOT", dimension_Integer}, args___] := QuantumOperator[
    SparseArray[{i_, j_} :> 
        If[ IntegerDigits[j - 1, dimension, 2] == {
                First[IntegerDigits[i - 1, dimension, 2]],
                Mod[-Total[IntegerDigits[i - 1, dimension, 2]], dimension]
            },
            1,
            0
        ],
        {dimension ^ 2, dimension ^ 2}
    ],
    dimension,
    "Label" -> "Controlled"["NOT"],
    args
]


QuantumOperator["CPHASE", args___] := QuantumOperator[{"CPHASE", 2}, args]

QuantumOperator[{"CPHASE", dimension_Integer}, args___] := QuantumOperator[
    SparseArray[{i_, j_} :>  Which[
            (i == j && First[IntegerDigits[i - 1, dimension, 2]] == 0),
            1,

            i == j && First[IntegerDigits[j - 1, dimension, 2]] > 0 && First[IntegerDigits[i - 1, dimension, 2]] > 0,
            Exp[2 Pi I (IntegerDigits[i - 1, dimension, 2][[2]]) (IntegerDigits[j - 1, dimension, 2][[2]]) / dimension],

            True,
            0
        ],
        {dimension ^ 2, dimension ^ 2}
    ],
    dimension,
    "Label" -> "Controlled"["PHASE"],
    args
]


controlledMatrix[matrix_ ? MatrixQ, dimension_Integer] := ReplacePart[
    IdentityMatrix[dimension ^ 2],
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

QuantumOperator[name : "CX" | "CY" | "CZ", args___] := QuantumOperator[{name, 2}, args]

QuantumOperator[{"CX", dimension_Integer}, args___] := QuantumOperator[{"ControlledU", {"PauliX", dimension}}, args]

QuantumOperator[{"CY", dimension_Integer}, args___] := QuantumOperator[{"ControlledU", {"PauliY", dimension}}, args]

QuantumOperator[{"CZ", dimension_Integer}, args___] := QuantumOperator[{"ControlledU", {"PauliZ", dimension}}, args]

QuantumOperator["CH", args___] := QuantumOperator[{"ControlledU", QuantumOperator["Hadamard"]}, args]


QuantumOperator[{"ControlledU", params___}, args___] :=
    QuantumOperator[{"ControlledU", QuantumOperator[params]}, args]

QuantumOperator[{"ControlledU", qo_ ? QuantumOperatorQ}, args___] :=
    QuantumOperator[controlledMatrix[qo["MatrixRepresentation"], qo["InputDimension"]], qo["InputDimension"], "Label" -> "Controlled"[qo["Label"]], args]


QuantumOperator["Fourier", args___] := QuantumOperator[{"Fourier", 2}, args]

QuantumOperator[{"Fourier", dimension_Integer}, args___, order_ ? orderQ] := QuantumOperator[
    SparseArray[
        ({i_, j_} :>  Exp[2 Pi I (i - 1) (j - 1) / (dimension ^ Length[order])] / Sqrt[dimension ^ Length[order]]),
        {dimension ^ Length[order], dimension ^ Length[order]}
    ],
    dimension,
    "Label" -> "QFT",
    args,
    order
]

QuantumOperator["InverseFourier", args___] := QuantumOperator[{"InverseFourier", 2}, args]

QuantumOperator[{"InverseFourier", dimension_Integer}, args___, order_?orderQ] := QuantumOperator[
     SparseArray[
        ({i_, j_} :> Exp[-2 Pi I (i - 1) (j - 1) / (dimension ^ Length[order])] / Sqrt[dimension ^ Length[order]]),
        {dimension ^ Length[order], dimension ^ Length[order]}
    ],
    dimension,
    "Label" -> Superscript["QFT", "\[Dagger]"],
    args,
    order
]


swapMatrix[dimension_] := SparseArray[# -> 1 & /@
    Thread[{
        Range[dimension ^ 2],
        Flatten @ Table[j + i - 1, {i, dimension}, {j, 1, dimension ^ 2, dimension}]
    }]
]

QuantumOperator["SWAP", args___] := QuantumOperator[{"SWAP", 2}, args]

QuantumOperator[{"SWAP", dimension_Integer}, args___] :=
    QuantumOperator[swapMatrix[dimension], dimension, "Label" -> "SWAP", args]

QuantumOperator["RootSWAP", args___] := QuantumOperator[{"RootSWAP", 2}, args]

QuantumOperator[{"RootSWAP", dimension_Integer}, args___] :=
    QuantumOperator[MatrixPower[swapMatrix[dimension], 1 / 2], dimension, "Label" -> "RootSWAP", args]


QuantumOperator["SUM", args___] := QuantumOperator[{"SUM", 2}, args]

QuantumOperator[{"SUM", dimension_Integer}, args___] := QuantumOperator[
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
    "Label" -> "SUM",
    args
]


QuantumOperator[name : "PauliX" | "PauliY" | "PauliZ", args___] := QuantumOperator[{name, 2}, args]

QuantumOperator[{"PauliX", dimension_Integer}, args___] := With[{
    s = (dimension - 1) / 2
},
    QuantumOperator[
        SparseArray[
            {a_, b_} :> (KroneckerDelta[a, b + 1] + KroneckerDelta[a + 1, b]) Sqrt[(s + 1) (a + b - 1) - a b],
            {dimension, dimension}
        ],
        dimension,
        "Label" -> "X",
        args
    ]
]

QuantumOperator[{"PauliY", dimension_Integer}, args___] := With[{
    s = (dimension - 1) / 2
},
    QuantumOperator[
        SparseArray[
            {a_, b_} :> I (KroneckerDelta[a, b + 1] -  KroneckerDelta[a + 1, b]) Sqrt[(s + 1) (a + b - 1) - a b],
            {dimension, dimension}
        ],
        dimension,
        "Label" -> "Y",
        args
    ]
]

QuantumOperator[{"PauliZ", dimension_Integer}, args___] := With[{
    s = (dimension - 1) / 2
},
    QuantumOperator[
        SparseArray[
            {a_, b_} :> 2 (s + 1 - a) KroneckerDelta[a, b],
            {dimension, dimension}
        ],
        dimension,
        "Label" -> "Z",
        args
    ]
]


QuantumOperator["RootNOT", args___] := QuantumOperator[{"RootNOT", 2}, args]

QuantumOperator[{"RootNOT", dimension_Integer}, args___] := QuantumOperator[
    MatrixPower[
        SparseArray[({i_, j_} /; Mod[i - 1, dimension, 1] == j) -> 1, {dimension, dimension}],
        1 / 2
    ],
    dimension,
    "Label" -> "RootNOT",
    args
]


QuantumOperator["Hadamard" | "H", args___] := QuantumOperator[HadamardMatrix[2], "Label" -> "H", args]

QuantumOperator[{"Hadamard" | "H", qudits_Integer ? Positive}, args___] :=
    QuantumOperator[QuantumTensorProduct[Table[QuantumOperator["Hadamard", args], qudits]]]


QuantumOperator["Toffoli", args___, order_?orderQ] := QuantumOperator[{"Toffoli", Length[order]}, args, order]

QuantumOperator[{"Toffoli", arity_Integer}, args___] := QuantumOperator[
    SparseArray[{i_, j_} :>
        If[ (i == j && i < (2 ^ arity) - 1) || (i == (2 ^ arity) - 1 &&
            j == 2 ^ arity) || (j == (2 ^ arity) - 1 && i == 2 ^ arity),
            1,
            0
        ], {2 ^ arity, 2 ^ arity}
    ],
    args
]


QuantumOperator["CSWAP", args___] := QuantumOperator[
    SparseArray[
        {
            {1, 1} -> 1, {2, 2} -> 1, {3, 3} -> 1, {4, 4} -> 1,
            {5, 5} -> 1, {6, 7} -> 1, {7, 6} -> 1, {8, 8} -> 1
        },
        {8, 8}
    ],
    "Label" -> "Controlled"["SWAP"],
    args
]


QuantumOperator[name : "XX" | "YY" | "ZZ", args___] := QuantumOperator[{name, 0}, args]

QuantumOperator[{"XX", angle_}, args___] := QuantumOperator[
    SparseArray[{
        {1, 1} -> Cos[angle], {2, 2} -> Cos[angle], {3, 3} -> Cos[angle], {4, 4} -> Cos[angle],
        {4, 1} -> -I Sin[angle], {3, 2} -> -I Sin[angle], {2, 3} -> -I Sin[angle], {1, 4} -> -I Sin[angle]
    },
        {4, 4}
    ],
    "Label" -> "XX",
    args
]

QuantumOperator[{"YY", angle_}, args___] := QuantumOperator[
    SparseArray[{
        {1, 1} -> Cos[angle], {2, 2} -> Cos[angle], {3, 3} -> Cos[angle], {4, 4} -> Cos[angle],
        {4, 1} -> I Sin[angle], {3, 2} -> -I Sin[angle], {2, 3} -> -I Sin[angle], {1, 4} -> I Sin[angle]
    },
        {4, 4}
    ],
    "Label" -> "YY",
    args
]

QuantumOperator[{"ZZ", angle_}, args___] := QuantumOperator[
    SparseArray[{
        {1, 1} -> Exp[I angle / 2], {2, 2} -> Exp[-I angle / 2],
        {3, 3} -> Exp[-I angle / 2], {4, 4} ->  Exp[I angle / 2]
    },
        {4, 4}
    ],
    "Label" -> "ZZ",
    args
]


QuantumOperator["Deutsch", args___] := QuantumOperator[{"Deutsch", 0}, args]
QuantumOperator[{"Deutsch", angle_}, args___] := QuantumOperator[
    SparseArray[{
        {1, 1} -> 1, {2, 2} -> 1, {3, 3} -> 1, {4, 4} -> 1, {5, 5} -> 1, {6, 6} -> 1,
        {7, 7} -> I Cos[angle], {7, 8} ->  Sin[angle], {8, 7} -> Sin[angle], {8, 8} -> I Cos[angle]
    },
        {8, 8}
    ],
    args
]


QuantumOperator["RandomUnitary", args___] := QuantumOperator[{"RandomUnitary", 2}, args]

QuantumOperator[{"RandomUnitary", dimension_Integer}, args___, order_ ? orderQ] :=
    QuantumOperator[RandomVariate @ CircularUnitaryMatrixDistribution[dimension ^ Length[order]], dimension, args, order]


(*  default orders for operators that use it  *)

QuantumOperator[name : {"Fourier" | "InverseFourier" | "RandomUnitary", ___},
    PatternSequence[args___, order : (_ ? orderQ) : {1}]] := QuantumOperator[name, args, order]

QuantumOperator[name : "Toffoli", PatternSequence[args___, order : (_ ? orderQ) : {1, 2, 3}]] :=
    QuantumOperator[name, args, order]

