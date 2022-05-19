Package["Wolfram`QuantumFramework`"]

PackageScope["$QuantumOperatorNames"]
PackageScope["pauliZGate"]
PackageScope["controlledZGate"]



$QuantumOperatorNames = {
    "Identity", "Permutation", "Curry", "Uncurry",
    "Fourier", "InverseFourier",
    "XRotation", "YRotation", "ZRotation", "U", "Phase", "P",
    "Diagonal", "GlobalPhase",
    "SUM", "RootNOT",
    "X", "Y", "Z", "PauliX", "PauliY", "PauliZ", "H", "Hadamard",
    "SWAP", "RootSWAP", "CSWAP", "Fredkin",
    "Controlled", "Controlled0", "CX", "CY", "CZ", "CH", "CT", "CS", "CPHASE", "CNOT",
    "XX", "YY", "ZZ",
    "S", "T",
    "Toffoli", "Deutsch", "RandomUnitary",
    "Spider", "ZSpider", "XSpider",
    "Switch"
}


pauliZGate = SparseArray[{j_, j_} :> Exp[(2 Pi I j / 2) + I Pi], {2, 2}];

controlledZGate = ReplacePart[
    identityMatrix[4],
    Thread[
        Flatten[Table[{i, j}, {i, 4 - Length[pauliZGate] + 1, 4}, {j, 4 - Length[pauliZGate] + 1, 4}], {2, 1}] ->
        Flatten[pauliZGate]
    ]
];


QuantumOperator[name_ ? nameQ, basisName : Except[Alternatives @@ $QuantumBasisPictures, _ ? nameQ]] :=
    QuantumOperator[QuantumOperator[name], QuantumBasis[basisName]]


QuantumOperator[] := QuantumOperator["Identity"]

QuantumOperator["Identity", opts___] := QuantumOperator[{"Identity", 2}, opts]

QuantumOperator[{"Identity", dims_List}, opts___] := QuantumOperator[{"Permutation", dims, Cycles[{{}}]}, opts]

QuantumOperator[{"Identity", qb_ ? QuditBasisQ}, opts___] := QuantumOperator[
    QuantumOperator[{"Identity", qb["Dimensions"]}, opts],
    "Output" -> qb, "Input" -> qb["Dual"]
]

QuantumOperator[{"Identity", dimension_Integer}, opts___] := QuantumOperator[identityMatrix[dimension], opts, dimension, "Label" -> "I"]


QuantumOperator[name : "XRotation" | "YRotation" | "ZRotation", opts___] :=  QuantumOperator[{name, Pi / 2}, opts]

QuantumOperator[{"XRotation", angle_, dimension_Integer : 2}, opts___] := QuantumOperator[
    Exp[- I angle / 2 QuantumOperator[{"PauliX", dimension}, opts]],
    "Label" -> Subscript["R", "X"][angle]
]

QuantumOperator[{"YRotation", angle_, dimension_Integer : 2}, opts___] := QuantumOperator[
    Exp[- I angle / 2 QuantumOperator[{"PauliY", dimension}, opts]],
    "Label" -> Subscript["R", "Y"][angle]
]

QuantumOperator[{"ZRotation", angle_, dimension_Integer : 2}, opts___] := QuantumOperator[
    Exp[- I angle / 2 QuantumOperator[{"PauliZ", dimension}, opts]],
    "Label" -> Subscript["R", "Z"][angle]
]

QuantumOperator[{"U", theta_, phi_, lambda_}, opts___] := QuantumOperator[
    Exp[I (lambda + phi) / 2] QuantumOperator[{"ZRotation", phi + Pi / 2}, opts] @ QuantumOperator[{"XRotation", theta}, opts] @ QuantumOperator[{"ZRotation", lambda - Pi / 2}, opts],
    "Label" -> "U"[theta, phi, lambda]
]

QuantumOperator["Phase" | "P", opts___] := QuantumOperator[{"Phase", Pi}, opts]

QuantumOperator[{"Phase" | "P", angle_, dimension_Integer : 2}, opts___] := QuantumOperator[
    SparseArray[{{1, 1} -> 1, {dimension, dimension} -> Exp[I angle]}],
    opts,
    "Label" -> "P"[angle]
]

QuantumOperator["GlobalPhase", opts___] := QuantumOperator[{"GlobalPhase", Pi}, opts]

QuantumOperator[{"GlobalPhase", angle_, dimension_Integer : 2}, opts___] := QuantumOperator[{"Diagonal", Exp[I angle], dimension}, opts]

QuantumOperator[{"Diagonal", x_, dimension_Integer : 2}, opts___] := QuantumOperator[
    identityMatrix[dimension] x,
    opts,
    "Label" -> x
]



QuantumOperator["S", opts___] := QuantumOperator[{"Phase", Pi / 2}, opts, "Label" -> "S"]

QuantumOperator["T", opts___] := QuantumOperator[{"Phase", Pi / 4}, opts, "Label" -> "T"]


QuantumOperator["CNOT", opts___] := QuantumOperator[{"CNOT", 2}, opts]

QuantumOperator[{"CNOT", dimension_Integer}, opts___] := QuantumOperator[{"ControlledU", {"NOT", dimension}}, opts]


QuantumOperator["CPHASE", opts___] := QuantumOperator[{"CPHASE", Pi}, opts]

QuantumOperator[{"CPHASE", angle_, dimension_Integer : 2}, opts___] := QuantumOperator[{"ControlledU", {"Phase", angle, dimension}}, opts]


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

QuantumOperator[{"CX", dimension_Integer}, opts___] :=
    QuantumOperator[{"ControlledU", {"PauliX", dimension}}, opts]

QuantumOperator[{"CY", dimension_Integer}, opts___] :=
    QuantumOperator[{"ControlledU", {"PauliY", dimension}}, opts]

QuantumOperator[{"CZ", dimension_Integer}, opts___] :=
    QuantumOperator[{"ControlledU", {"PauliZ", dimension}}, opts]

QuantumOperator["CH", opts___] := QuantumOperator[{"ControlledU", "Hadamard"}, opts]

QuantumOperator["CT", opts___] := QuantumOperator[{"ControlledU", "T"}, opts]

QuantumOperator["CS", opts___] := QuantumOperator[{"ControlledU", "S"}, opts]


QuantumOperator[{"ControlledU" | "Controlled", params : PatternSequence[Except[_ ? QuantumOperatorQ], ___], control_ ? orderQ, control0 : _ ? orderQ : {}}, opts___] :=
    With[{op = QuantumOperator[params]}, QuantumOperator[{"ControlledU", QuantumOperator[op, Max[control] + op["InputOrder"]], control, control0}, opts]]

QuantumOperator[{"ControlledU" | "Controlled", params : PatternSequence[Except[_ ? QuantumOperatorQ], ___], control_ ? orderQ, control0 : _ ? orderQ : {}}, target_ ? orderQ, opts___] :=
    QuantumOperator[{"ControlledU", QuantumOperator[params, target], control, control0}, opts]

QuantumOperator[{"ControlledU" | "Controlled", params : PatternSequence[___, Except[_ ? orderQ]]}, order_ ? orderQ, opts___] :=
    With[{op = QuantumOperator[params, Replace[Rest @ order, {} -> {First @ order + 1}]]},
        QuantumOperator[{"ControlledU", op, {First @ order}}, opts]
    ]

QuantumOperator[{"Controlled0", params : PatternSequence[___, Except[_ ? orderQ]]}, order_ ? orderQ,  opts___] :=
    With[{op = QuantumOperator[params, Replace[Rest @ order, {} -> {First @ order + 1}]]},
        QuantumOperator[{"Controlled", op, {}, {First @ order}}, opts]
    ]

QuantumOperator[{"ControlledU" | "Controlled", params : PatternSequence[Except[_ ? QuantumOperatorQ], ___]}, opts___] := Enclose @
    With[{op = QuantumOperator[params]},
        QuantumOperator[{"ControlledU", ConfirmBy[QuantumOperator[op, op["InputOrder"] + 1], QuantumOperatorQ], {First @ op["InputOrder"]}}, opts]
    ]

QuantumOperator[{"ControlledU" | "Controlled", qo_ ? QuantumOperatorQ}, opts___] := QuantumOperator[{"ControlledU", qo, {qo["LastInputQudit"] + 1}}, opts]

QuantumOperator[{"ControlledU" | "Controlled", qo_ ? QuantumOperatorQ, control___}, target_ ? orderQ, opts___] :=
    QuantumOperator[{"Controlled", QuantumOperator[qo, target], control}, opts]


QuantumOperator[{"ControlledU" | "Controlled", qo_ ? QuantumOperatorQ, control1_ ? orderQ, control0 : _ ? orderQ : {}}, opts___] := Enclose @ With[{
    controls1 = Length[control1],
    controls0 = Length[control0],
    control = Join[control1, control0]
},
    (*ConfirmAssert[! IntersectingQ[qo["Order"], control], "Target and control qudits shouldn't intersect"];*)
    QuantumOperator[
        BlockDiagonalMatrix[
            identityMatrix[(2 ^ controls1 - 1) qo["OutputDimension"]],
            qo["Matrix"],
            identityMatrix[(2 ^ controls0 - 1) 2 ^ controls1 qo["OutputDimension"]]
        ],
        With[{order = Join[
            control0,
            control1,
            If[ IntersectingQ[control, qo["FullInputOrder"]],
                With[{order = Complement[Range @@ MinMax[Join[control, qo["FullInputOrder"]]], control]},
                    Join[order, Max[qo["FullInputOrder"], control] + Range[qo["Arity"] - Length[order]]]
                ],
                qo["InputOrder"]
            ]
        ]},
            {order, order}
        ],
        QuantumTensorProduct[
            QuantumBasis[QuditBasis[2, controls0], QuditBasis[2, controls0]],
            QuantumBasis[QuditBasis[2, controls1], QuditBasis[2, controls1]],
            qo["Basis"]
        ],
        opts,
        "Label" -> "Controlled"[qo["Label"], control1, control0]
    ]["Sort"]
]


QuantumOperator["Controlled0" | {"Controlled0", params___, control : _ ? orderQ : {}}, opts___] :=
    QuantumOperator[{"Controlled", params, {}, control}, opts]


QuantumOperator["Fourier", opts___] := QuantumOperator[{"Fourier", 2}, opts]

QuantumOperator[{"Fourier", dimension_Integer}, opts___, order : (_ ? orderQ) : {1}] := QuantumOperator[QuantumOperator[
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

QuantumOperator[{"InverseFourier", dimension_Integer}, opts___, order : (_ ? orderQ) : {1}] := QuantumOperator[QuantumOperator[
        SparseArray[
            ({i_, j_} :> Exp[-2 Pi I (i - 1) (j - 1) / (dimension ^ Length[order])] / Sqrt[dimension ^ Length[order]]),
            {dimension ^ Length[order], dimension ^ Length[order]}
        ],
        dimension,
        Length[order]
    ],
    {order, order},
    opts,
    "Label" -> Superscript["QFT", "\[Dagger]"]
]


swapMatrix[dimension_] := SparseArray[# -> 1 & /@
    Thread[{
        Range[dimension ^ 2],
        Flatten @ Table[j + i - 1, {i, dimension}, {j, 1, dimension ^ 2, dimension}]
    }]
]

QuantumOperator["SWAP", opts___] := QuantumOperator[{"SWAP", 2}, opts]

QuantumOperator[{"SWAP", dimension_Integer}, opts___] :=
    QuantumOperator[QuantumOperator[swapMatrix[dimension], dimension, 2], opts, "Label" -> "SWAP"]

QuantumOperator["RootSWAP", opts___] := QuantumOperator[{"RootSWAP", 2}, opts]

QuantumOperator[{"RootSWAP", dimension_Integer}, opts___] :=
    QuantumOperator[QuantumOperator[MatrixPower[swapMatrix[dimension], 1 / 2], dimension, 2], opts, "Label" -> "RootSWAP"]


QuantumOperator["SUM", opts___] := QuantumOperator[{"SUM", 2}, opts]

QuantumOperator[{"SUM", dimension_Integer}, opts___] := QuantumOperator[QuantumOperator[
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
        2
    ],
    opts,
    "Label" -> "SUM"
]


QuantumOperator[name : "X" | "Y" | "Z" | "PauliX" | "PauliY" | "PauliZ" | "NOT", opts___] := QuantumOperator[{name, 2}, opts]

QuantumOperator[{"PauliX" | "X", dimension_Integer}, order : _ ? orderQ : {1}, opts___] := QuantumOperator[
    {QuantumOperator[pauliMatrix[1, dimension], dimension, opts, "Label" -> "X"], Length[order]},
    {order, order}
]

QuantumOperator[{"PauliY" | "Y", dimension_Integer}, order : _ ? orderQ : {1}, opts___] := QuantumOperator[
    {QuantumOperator[pauliMatrix[2, dimension], dimension, opts, "Label" -> "Y"], Length[order]},
    {order, order}
]

QuantumOperator[{"PauliZ" | "Z", dimension_Integer}, order : _ ? orderQ : {1}, opts___] := QuantumOperator[
    {QuantumOperator[pauliMatrix[3, dimension], dimension, opts, "Label" -> "Z"], Length[order]},
    {order, order}
]

QuantumOperator[{"NOT", dimension_Integer}, opts___] := QuantumOperator[{"X", dimension}, opts, "Label" -> "NOT"]


QuantumOperator["RootNOT", opts___] := QuantumOperator[{"RootNOT", 2}, opts]

QuantumOperator[{"RootNOT", dimension_Integer}, order : _ ? orderQ : {1}, opts___] := QuantumOperator[
    MatrixPower[
        SparseArray[({i_, j_} /; Mod[i - 1, dimension, 1] == j) -> 1, {dimension, dimension}],
        1 / 2
    ],
    {order, order},
    dimension,
    opts,
     "Label" -> Sqrt["NOT"]
]


QuantumOperator["Hadamard" | "H", order : _ ? orderQ : {1}, opts___] := QuantumOperator[{"H", Length @ order}, order, opts]

QuantumOperator[{"Hadamard" | "H", qudits_Integer ? Positive}, opts : PatternSequence[] | PatternSequence[___, Except[_ ? orderQ]]] :=
    QuantumOperator[{"H", qudits}, Range[qudits], opts]

QuantumOperator[{"Hadamard" | "H", qudits_Integer ? Positive}, order_ ? orderQ, opts___] :=
    QuantumOperator[
        QuantumTensorProduct[Table[QuantumOperator[HadamardMatrix[2], opts], qudits]],
        {order, order},
        "Label" -> If[qudits > 1, Superscript["H", CircleTimes[qudits]], "H"]
    ]


QuantumOperator["Toffoli", order : (_ ? orderQ) : {1, 2, 3}] :=
    QuantumOperator[{"ControlledU", "NOT", Most[order]}, {Last[order]}]


QuantumOperator["CSWAP" | "Fredkin", opts___] := QuantumOperator[{"ControlledU", "SWAP"}, opts]


QuantumOperator[name : "XX" | "YY" | "ZZ", opts___] := QuantumOperator[{name, 0}, opts]

QuantumOperator[{"XX", angle_}, opts___] := QuantumOperator[
    SparseArray[{
        {1, 1} -> Cos[angle / 2], {2, 2} -> Cos[angle / 2], {3, 3} -> Cos[angle / 2], {4, 4} -> Cos[angle / 2],
        {4, 1} -> -I Sin[angle / 2], {3, 2} -> -I Sin[angle / 2], {2, 3} -> -I Sin[angle / 2], {1, 4} -> -I Sin[angle / 2]
    },
        {4, 4}
    ],
    opts,
    "Label" -> Subscript["R", "XX"][angle]
]

QuantumOperator[{"YY", angle_}, opts___] := QuantumOperator[
    SparseArray[{
        {1, 1} -> Cos[angle / 2], {2, 2} -> Cos[angle / 2], {3, 3} -> Cos[angle / 2], {4, 4} -> Cos[angle / 2],
        {4, 1} -> I Sin[angle / 2], {3, 2} -> -I Sin[angle / 2], {2, 3} -> -I Sin[angle / 2], {1, 4} -> I Sin[angle / 2]
    },
        {4, 4}
    ],
    opts,
    "Label" -> Subscript["R", "YY"][angle]
]

QuantumOperator[{"ZZ", angle_}, opts___] := QuantumOperator[
    SparseArray[{
        {1, 1} -> Exp[- I angle / 2], {2, 2} -> Exp[I angle / 2],
        {3, 3} -> Exp[I angle / 2], {4, 4} ->  Exp[- I angle / 2]
    },
        {4, 4}
    ],
    opts,
    "Label" -> Subscript["R", "ZZ"][angle]
]


QuantumOperator["RandomUnitary", opts___] := QuantumOperator[{"RandomUnitary", 2}, opts]

QuantumOperator[{"RandomUnitary", dimension_Integer}, order : (_ ? orderQ) : {1}, opts___] :=
    QuantumOperator[
           RandomVariate @ CircularUnitaryMatrixDistribution[dimension ^ Length[order]], order, dimension, Length[order], opts, "Label" -> None
    ]

QuantumOperator["RandomUnitary", order : (_ ? orderQ) : {1}, opts___] := With[{basis = QuantumBasis[opts]},
    QuantumOperator[
           RandomVariate @ CircularUnitaryMatrixDistribution[basis["Dimension"]], order, basis, "Label" -> None
    ]
]


QuantumOperator[{"Permutation", perm_Cycles}, opts___] := QuantumOperator[{"Permutation", 2, perm}, opts]

QuantumOperator[{"Permutation", dim_Integer, perm_Cycles}, opts___] := QuantumOperator[{"Permutation", Table[dim, PermutationMax[perm]], perm}, opts]

QuantumOperator[{"Permutation", dims_List, perm_Cycles}] := QuantumOperator[{"Permutation", dims, perm}, Range[Length[dims]]]

QuantumOperator[{"Permutation", dims_List, perm_Cycles}, args___] :=
    QuantumOperator[
        TensorTranspose[ArrayReshape[kroneckerProduct @@ identityMatrix /@ dims, Join[dims, dims]], perm],
        args,
        QuantumBasis[QuditBasis[Permute[dims, perm]], QuditBasis[dims], "Label" -> Superscript["\[Pi]", Row @ PermutationList[perm]]]
    ]

QuantumOperator["Uncurry", opts___] := QuantumOperator[{"Uncurry", {2, 2}}, opts]

QuantumOperator[{"Uncurry", dims_List}] := QuantumOperator[{"Uncurry", dims}, {1}, Range[Length[dims]]]

QuantumOperator[{"Uncurry", dims_List}, opts___] :=
    QuantumOperator[
        QuantumOperator[identityMatrix[Times @@ dims], QuantumBasis[QuditBasis[Times @@ dims], QuditBasis[dims]["Dual"]]],
        opts
    ]


QuantumOperator[name : "Curry" | {"Curry", ___}, opts___] := QuantumOperator[name /. "Curry" -> "Uncurry", opts]["ConjugateTranspose"]


QuantumOperator[name : "ZSpider" | "XSpider" | "Spider", opts___] := QuantumOperator[{name}, opts]

QuantumOperator[{"ZSpider", out_Integer : 1, in_Integer : 1, phase_ : 0}, opts___] := Module[{
    phases = If[ListQ[phase], phase, {0, phase}],
    dim, basis
},
    dim = Length[phases];
    basis = QuantumBasis[QuditBasis[{"PauliZ", dim}, out], QuditBasis[{"PauliZ", dim}, in], "Label" -> "ZSpider"];
    QuantumOperator[
        QuantumState[
            If[ basis["Dimension"] <= 1,
                {Exp[I Last[phases]]}[[;; basis["Dimension"]]],
                Flatten @ SparseArray[Thread[Transpose[Table[Range[dim], basis["Qudits"]]] -> Exp[I phases]], basis["Dimensions"]]
            ],
            basis
        ],
        Range[basis["OutputQudits"]], Range[basis["InputQudits"]],
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
        "ControlledU",
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

$upperCasesOperatorNames = AssociationThread[ToUpperCase @ $QuantumOperatorNames, $QuantumOperatorNames]

QuantumOperator[name_String, opts___] /; KeyExistsQ[$upperCasesOperatorNames, name] :=
    QuantumOperator[$upperCasesOperatorNames[name], opts]

QuantumOperator[{name_String, params___}, opts___] /; KeyExistsQ[$upperCasesOperatorNames, name] :=
    QuantumOperator[{$upperCasesOperatorNames[name], params}, opts]

