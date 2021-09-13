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


QuantumDiscreteOperator[{"XRotation", angle_}, args___] := QuantumDiscreteOperator[
    {{Cos[angle / 2], I Sin[angle / 2]}, {I Sin[angle / 2], Cos[angle / 2]}},
    args
]

QuantumDiscreteOperator[{"YRotation", angle_}, args___] := QuantumDiscreteOperator[
    {{Cos[angle / 2], Sin[angle / 2]}, {-Sin[angle / 2], Cos[angle / 2]}},
    args
]

QuantumDiscreteOperator[{"ZRotation", angle_}, args___] := QuantumDiscreteOperator[
    SparseArray[{{1, 1} -> 1, {2, 2} -> Exp[I angle]}],
    args
]


QuantumDiscreteOperator["S", args___] := QuantumDiscreteOperator[{"ZRotation", Pi / 2}, args]

QuantumDiscreteOperator["T", args___] := QuantumDiscreteOperator[{"ZRotation", Pi / 4}, args]


QuantumDiscreteOperator["CNOT", args___] := QuantumDiscreteOperator[{"CNOT", 2}, args]

QuantumDiscreteOperator[{"CNOT", dimension_Integer}, args___] := QuantumDiscreteOperator[
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
    args
]


QuantumDiscreteOperator["CPHASE", args___] := QuantumDiscreteOperator[{"CPHASE", 2}, args]

QuantumDiscreteOperator[{"CPHASE", dimension_Integer}, args___] := QuantumDiscreteOperator[
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

QuantumDiscreteOperator[name : "CX" | "CY" | "CZ", args___] := QuantumDiscreteOperator[{name, 2}, args]

QuantumDiscreteOperator[{"CX", dimension_Integer}, args___] := QuantumDiscreteOperator[
    controlledMatrix[SparseArray[({i_, j_}  /; Mod[i - 1, dimension, 1] == j) -> 1, {dimension, dimension}], dimension],
    args
]

QuantumDiscreteOperator[{"CY", dimension_Integer}, args___] := QuantumDiscreteOperator[
    (* TODO: generalize for arbitrary dimension *)
    controlledMatrix[SparseArray[{{1, 2} -> -I, {2, 1} -> I}], dimension],
    args
]

QuantumDiscreteOperator[{"CZ", dimension_Integer}, args___] := QuantumDiscreteOperator[
    controlledMatrix[SparseArray[{_, j_} :> Exp[(2 Pi I j / dimension) + I Pi], {dimension, dimension}], dimension],
    args
]


QuantumDiscreteOperator[{"ControlledU", arg_, dimension_Integer : 2}, args___] :=
    QuantumDiscreteOperator[{"ControlledU", QuantumDiscreteOperator[arg], dimension}, args]

QuantumDiscreteOperator[{"ControlledU", qdo_ ? QuantumDiscreteOperatorQ, dimension_Integer}, args___] :=
    controlledMatrix[qdo["MatrixRepresentation"], dimension, args]


QuantumDiscreteOperator["Fourier", args___] := QuantumDiscreteOperator[{"Fourier", 2}, args]

QuantumDiscreteOperator[{"Fourier", dimension_Integer}, args___, order_ ? orderQ] := QuantumDiscreteOperator[
    SparseArray[
        ({i_, j_} :>  Exp[2 Pi I (i - 1) (j - 1) / (dimension ^ Length[order])] / Sqrt[dimension ^ Length[order]]),
        {dimension ^ Length[order], dimension ^ Length[order]}
    ],
    dimension,
    args,
    order
]

QuantumDiscreteOperator["InverseFourier", args___] := QuantumDiscreteOperator[{"InverseFourier", 2}, args]

QuantumDiscreteOperator[{"InverseFourier", dimension_Integer}, args___, order_?orderQ] := QuantumDiscreteOperator[
     SparseArray[
        ({i_, j_} :> Exp[-2 Pi I (i - 1) (j - 1) / (dimension ^ Length[order])] / Sqrt[dimension ^ Length[order]]),
        {dimension ^ Length[order], dimension ^ Length[order]}
    ],
    dimension,
    args,
    order
]


swapMatrix[dimension_] := SparseArray[# -> 1 & /@
    Thread[{
        Range[dimension ^ 2],
        Flatten @ Table[j + i - 1, {i, dimension}, {j, 1, dimension ^ 2, dimension}]
    }]
]

QuantumDiscreteOperator["SWAP", args___] := QuantumDiscreteOperator[{"SWAP", 2}, args]

QuantumDiscreteOperator[{"SWAP", dimension_Integer}, args___] :=
    QuantumDiscreteOperator[swapMatrix[dimension], dimension, args]

QuantumDiscreteOperator["RootSWAP", args___] := QuantumDiscreteOperator[{"RootSWAP", 2}, args]

QuantumDiscreteOperator[{"RootSWAP", dimension_Integer}, args___] :=
    QuantumDiscreteOperator[MatrixPower[swapMatrix[dimension], 1 / 2], dimension, args]


QuantumDiscreteOperator["SUM", args___] := QuantumDiscreteOperator[{"SUM", 2}, args]

QuantumDiscreteOperator[{"SUM", dimension_Integer}, args___] := QuantumDiscreteOperator[
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
    args
]


QuantumDiscreteOperator[name : "PauliX" | "PauliY" | "PauliZ", args___] := QuantumDiscreteOperator[{name, 2}, args]

QuantumDiscreteOperator[{"PauliX", dimension_Integer}, args___] := With[{
    s = (dimension - 1) / 2
},
    QuantumDiscreteOperator[
        SparseArray[
            {a_, b_} :> (KroneckerDelta[a, b + 1] + KroneckerDelta[a + 1, b]) Sqrt[(s + 1) (a + b - 1) - a b],
            {dimension, dimension}
        ],
        dimension,
        args
    ]
]

QuantumDiscreteOperator[{"PauliY", dimension_Integer}, args___] := With[{
    s = (dimension - 1) / 2
},
    QuantumDiscreteOperator[
        SparseArray[
            {a_, b_} :> I (KroneckerDelta[a, b + 1] -  KroneckerDelta[a + 1, b]) Sqrt[(s + 1) (a + b - 1) - a b],
            {dimension, dimension}
        ],
        dimension,
        args
    ]
]

QuantumDiscreteOperator[{"PauliY", dimension_Integer}, args___] := With[{
    s = (dimension - 1) / 2
},
    QuantumDiscreteOperator[
        SparseArray[
            {a_, b_} :> 2 (s + 1 - a) KroneckerDelta[a, b],
            {dimension, dimension}
        ],
        dimension,
        args
    ]
]


QuantumDiscreteOperator["RootNOT", args___] := QuantumDiscreteOperator[{"RootNOT", 2}, args]

QuantumDiscreteOperator[{"RootNOT", dimension_Integer}, args___] := QuantumDiscreteOperator[
    MatrixPower[
        SparseArray[({i_, j_} /; Mod[i - 1, dimension, 1] == j) -> 1, {dimension, dimension}],
        1 / 2
    ],
    args
]


QuantumDiscreteOperator["Hadamard", args___] := QuantumDiscreteOperator[HadamardMatrix[2], args]


QuantumDiscreteOperator["Toffoli", args___, order_?orderQ] := QuantumDiscreteOperator[{"Toffoli", Length[order]}, args, order]

QuantumDiscreteOperator[{"Toffoli", arity_Integer}, args___] := QuantumDiscreteOperator[
    SparseArray[{i_, j_} :>
        If[ (i == j && i < (2 ^ arity) - 1) || (i == (2 ^ arity) - 1 &&
            j == 2 ^ arity) || (j == (2 ^ arity) - 1 && i == 2 ^ arity),
            1,
            0
        ], {2 ^ arity, 2 ^ arity}
    ],
    args
]


QuantumDiscreteOperator["CSWAP", args___] := QuantumDiscreteOperator[
    SparseArray[
        {
            {1, 1} -> 1, {2, 2} -> 1, {3, 3} -> 1, {4, 4} -> 1,
            {5, 5} -> 1, {6, 7} -> 1, {7, 6} -> 1, {8, 8} -> 1
        },
        {8, 8}
    ],
    args
]


QuantumDiscreteOperator[name : "XX" | "YY" | "ZZ", args___] := QuantumDiscreteOperator[{name, 0}, args]

QuantumDiscreteOperator[{"XX", angle_}, args___] := QuantumDiscreteOperator[
    SparseArray[{
        {1, 1} -> Cos[angle], {2, 2} -> Cos[angle], {3, 3} -> Cos[angle], {4, 4} -> Cos[angle],
        {4, 1} -> -I Sin[angle], {3, 2} -> -I Sin[angle], {2, 3} -> -I Sin[angle], {1, 4} -> -I Sin[angle]
    },
        {4, 4}
    ],
    args
]

QuantumDiscreteOperator[{"YY", angle_}, args___] := QuantumDiscreteOperator[
    SparseArray[{
        {1, 1} -> Cos[angle], {2, 2} -> Cos[angle], {3, 3} -> Cos[angle], {4, 4} -> Cos[angle],
        {4, 1} -> I Sin[angle], {3, 2} -> -I Sin[angle], {2, 3} -> -I Sin[angle], {1, 4} -> I Sin[angle]
    },
        {4, 4}
    ],
    args
]

QuantumDiscreteOperator[{"ZZ", angle_}, args___] := QuantumDiscreteOperator[
    SparseArray[{
        {1, 1} -> Exp[I angle / 2], {2, 2} -> Exp[-I angle / 2],
        {3, 3} -> Exp[-I angle / 2], {4, 4} ->  Exp[I angle / 2]
    },
        {4, 4}
    ],
    args
]


QuantumDiscreteOperator["Deutsch", args___] := QuantumDiscreteOperator[{"Deutsch", 0}, args]
QuantumDiscreteOperator[{"Deutsch", angle_}, args___] := QuantumDiscreteOperator[
    SparseArray[{
        {1, 1} -> 1, {2, 2} -> 1, {3, 3} -> 1, {4, 4} -> 1, {5, 5} -> 1, {6, 6} -> 1,
        {7, 7} -> I Cos[angle], {7, 8} ->  Sin[angle], {8, 7} -> Sin[angle], {8, 8} -> I Cos[angle]
    },
        {8, 8}
    ],
    args
]


QuantumDiscreteOperator["RandomUnitary", args___] := QuantumDiscreteOperator[{"RandomUnitary", 2}, args]

QuantumDiscreteOperator[{"RandomUnitary", dimension_Integer}, args___, order_ ? orderQ] :=
    QuantumDiscreteOperator[RandomVariate @ CircularUnitaryMatrixDistribution[dimension ^ Length[order]], dimension, args, order]


(*  default orders for operators that use it  *)

QuantumDiscreteOperator[name : {"Fourier" | "InverseFourier" | "RandomUnitary", ___},
    PatternSequence[args___, order : (_ ? orderQ) : {1}]] := QuantumDiscreteOperator[name, args, order]

QuantumDiscreteOperator[name : "Toffoli", PatternSequence[args___, order : (_ ? orderQ) : {1, 2, 3}]] :=
    QuantumDiscreteOperator[name, args, order]

