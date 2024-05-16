Package["Wolfram`QuantumFramework`"]

PackageScope["$QuantumOperatorNames"]
PackageScope["pauliZGate"]
PackageScope["controlledZGate"]
PackageScope["FromOperatorShorthand"]



$QuantumOperatorNames = {
    "Identity", "I", "Permutation", "Uncurry", "Curry",
    "Fourier", "InverseFourier",
    "XRotation", "YRotation", "ZRotation", "U", "Phase", "P", "RX", "RY", "RZ", "R",
    "Diagonal", "GlobalPhase",
    "PhaseShift", "FlipSign",
    "SUM", "RootNOT",
    "X", "Y", "Z", "PauliX", "PauliY", "PauliZ", "Shift", "ShiftPhase",
    "H", "Hadamard", "NOT",
    "0", "1",
    "SWAP", "RootSWAP", "CSWAP", "Fredkin", "Braid",
    "C", "Controlled", "C0", "Controlled0", "CX", "CY", "CZ", "CH", "CT", "CS", "CPHASE", "CNOT",
    "S", "T", "V",
    "Toffoli", "Deutsch",
    "RandomUnitary", "RandomHermitian",
    "Spider", "ZSpider", "XSpider", "WSpider",
    "Measure", "Encode", "Copy", "Decohere", "Marginal", "Discard",
    "Cup", "Cap", "Trace", "Reset", "Channel", "Measurement",
    "Switch",
    "Multiplexer",
    "WignerD", "JX", "JY", "JZ", "JX+", "JY+", "JZ+", "JX-", "JY-", "JZ-", "J+", "J-", "+", "-",
    "Double",
    "Hamiltonian", "Liouvillian"
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
FromOperatorShorthand[op_ ? QuantumFrameworkOperatorQ -> order_ ? autoOrderQ] := Head[op][op, order]

FromOperatorShorthand[qm_ ? QuantumMeasurementQ] := qm["QuantumOperator"]
FromOperatorShorthand[qm_ ? QuantumMeasurementQ -> order_ ? autoOrderQ] := QuantumMeasurementOperator[qm["QuantumOperator"], order]

FromOperatorShorthand[SuperDagger[arg_]] := FromOperatorShorthand[Unevaluated[arg]]["Dagger"]
FromOperatorShorthand[SuperDagger[arg_] -> rest_] := FromOperatorShorthand[Unevaluated[arg -> rest]]["Dagger"]

namePattern = {name_String, ___} | name_String[___] | name_String

FromOperatorShorthand[arg : namePattern] /; MemberQ[$QuantumStateNames, name] || StringMatchQ[name, ("0" | "1" | "+" | "-" | "L" | "R") ..] :=
    With[{s = QuantumState[arg]}, QuantumOperator[s, "Label" -> Ket[{s["Label"]}]]]

FromOperatorShorthand[arg : namePattern] /; MemberQ[$QuantumMeasurementOperatorNames, name] := QuantumMeasurementOperator[arg]
FromOperatorShorthand[target_ ? targetQ] := QuantumMeasurementOperator[target]
FromOperatorShorthand[target_ ? targetQ -> {arg_, opts___} | arg_] := QuantumMeasurementOperator[arg, target, opts]
FromOperatorShorthand[target_Integer ? Positive -> {arg_, opts___} | arg_] := QuantumMeasurementOperator[arg, {target}, opts]


FromOperatorShorthand[arg : namePattern] /; MemberQ[$QuantumChannelNames, name] := QuantumChannel[arg]

FromOperatorShorthand[arg : namePattern] /; MemberQ[$QuantumOperatorNames, name] := QuantumOperator[arg]

FromOperatorShorthand[arg : namePattern -> order : _Integer | _ ? orderQ] /; MemberQ[$QuantumMeasurementOperatorNames, name] :=
    With[{ops = QuantumMeasurementOperator[arg, {#}] & /@ Flatten[{order}]}, QuantumMeasurementOperator[Fold[#2[#1] &, ops], "Label" -> First[ops]["Label"]]]
FromOperatorShorthand[arg : namePattern -> order : _Integer | _ ? orderQ] /; MemberQ[$QuantumChannelNames, name] :=
    With[{ops = QuantumChannel[arg, {#}] & /@ Flatten[{order}]}, QuantumChannel[Fold[#2[#1] &, ops], "Label" -> First[ops]["Label"]]]
FromOperatorShorthand[arg : namePattern -> order : _Integer | _ ? orderQ] := With[{op = FromOperatorShorthand[Unevaluated[arg]]}, Head[op][op, Flatten[{order}]]]

FromOperatorShorthand[lhs_ -> order_ ? autoOrderQ] := QuantumOperator[FromOperatorShorthand[Unevaluated[lhs]], order]
FromOperatorShorthand[lhs_ -> n_Integer] := FromOperatorShorthand[Unevaluated[lhs -> {n}]]
FromOperatorShorthand[lhs_ -> n : _Integer | _ ? orderQ -> m : _Integer | _ ? orderQ] := QuantumOperator[lhs, {Flatten[{m}], Flatten[{n}]}]
FromOperatorShorthand[(lhs_ -> rhs_) -> label : Except[OptionsPattern[]]] := FromOperatorShorthand[Unevaluated[(lhs -> rhs) -> ("Label" -> label)]]
FromOperatorShorthand[(lhs_ -> rhs_) -> opts : OptionsPattern[]] := With[{op = FromOperatorShorthand[Unevaluated[lhs -> rhs]]}, Head[op][op, opts]]
FromOperatorShorthand[lhs_ -> opts : OptionsPattern[]] := FromOperatorShorthand[Unevaluated[(lhs -> {1}) -> {opts}]]
FromOperatorShorthand[lhs_ -> label : Except[OptionsPattern[]]] := FromOperatorShorthand[Unevaluated[(lhs -> {1}) -> ("Label" -> label)]]

FromOperatorShorthand[lhs_ -> rest_] := QuantumOperator[Unevaluated[lhs], Sequence @@ Developer`ToList[rest]]
FromOperatorShorthand[args_List] := Function[Null, FromOperatorShorthand[Unevaluated[#]], HoldFirst] /@ Unevaluated[args]
FromOperatorShorthand[arg_] := QuantumOperator[arg]



QuantumOperator[] := QuantumOperator["Identity"]

QuantumOperator[arg : _String | {_String, ___}, n_Integer ? Positive, opts___] := QuantumOperator[arg, Range[n], opts]

QuantumOperator[arg_, inputOrder : _ ? orderQ | Automatic -> outputOrder : _ ? orderQ | Automatic, opts___] := QuantumOperator[arg, {outputOrder, inputOrder}, opts]

QuantumOperator["Identity" | "I", opts___] := QuantumOperator["I", Automatic, opts] 

QuantumOperator["Identity" | "I", order : _ ? orderQ | Automatic, opts___] :=
    QuantumOperator[{"Identity", Table[2, If[order === Automatic, 1, Length[order]]]}, order, opts]

QuantumOperator["Identity" | "I", order : {outOrder : _ ? orderQ | Automatic, inOrder : _ ? orderQ | Automatic}, opts___] :=
    QuantumOperator[{"Identity", Table[2, If[outOrder === Automatic, 1, Length[outOrder]]], Table[2, If[inOrder === Automatic, 1, Length[inOrder]]]}, order, opts]

QuantumOperator[{"Identity" | "I", dims : {_Integer ? Positive ...}}, opts___] := QuantumOperator[
    QuantumOperator[QuantumState[SparseArrayFlatten @ identityMatrix[Times @@ dims], QuantumBasis[QuditBasis[dims], QuditBasis[dims], "Label" -> "I"]]],
    opts
]

QuantumOperator[{"Identity" | "I", outDims : {_Integer ? Positive ...}, inDims : {_Integer ? Positive ...}}, opts___] := QuantumOperator[
    QuantumOperator[QuantumState[SparseArrayFlatten @ identityMatrix[{Times @@ outDims, Times @@ inDims}], QuantumBasis[QuditBasis[outDims], QuditBasis[inDims], "Label" -> "I"]]],
    opts
]

QuantumOperator[{"Identity" | "I", qb_ ? QuditBasisQ}, opts___] := QuantumOperator[
    QuantumOperator[{"Identity", qb["FullDimensions"]}],
    opts,
    "Output" -> qb, "Input" -> qb["Dual"]
]

QuantumOperator[{"Identity" | "I", qb_ ? QuantumBasisQ}, opts___] :=
    QuantumOperator[{"Identity", qb["OutputDimensions"], qb["InputDimensions"]}, opts]

QuantumOperator[{"Identity" | "I", params__}, opts___] :=
    Enclose @ QuantumOperator[{"Identity", ConfirmBy[QuantumBasis[params], QuantumBasisQ]}, opts]

QuantumOperator[{"Identity" | "I", dim_Integer ? Positive}, opts___] :=
    Enclose @ QuantumOperator[{"Identity", {dim}}, opts]



QuantumOperator[name : "XRotation" | "YRotation" | "ZRotation" | "RX" | "RY" | "RZ", opts___] :=  QuantumOperator[{name, Pi / 2}, opts]

QuantumOperator[{"XRotation" | "RX", angle_, dimension : _Integer ? Positive : 2}, opts___] := QuantumOperator[
    FullSimplify @ QuantumOperator[
        Exp[- I angle / 2 QuantumOperator[{"PauliX", dimension}]],
        "Label" -> Subscript["R", "X"][angle]
    ],
    opts
]

QuantumOperator[{"YRotation" | "RY", angle_, dimension : _Integer ? Positive : 2}, opts___] := QuantumOperator[
    FullSimplify @ QuantumOperator[
        Exp[- I angle / 2 QuantumOperator[{"PauliY", dimension}]],
        "Label" -> Subscript["R", "Y"][angle]
    ],
    opts
]

QuantumOperator[{"ZRotation" | "RZ", angle_, dimension : _Integer ? Positive : 2}, opts___] := QuantumOperator[
    FullSimplify @ QuantumOperator[
        Exp[- I angle / 2 QuantumOperator[{"PauliZ", dimension}]],
        "Label" -> Subscript["R", "Z"][angle]
    ],
    opts
]

QuantumOperator[{"R", angle_, args__}, opts___] := Enclose @ Block[{ops = ConfirmBy[QuantumOperator[#], QuantumOperatorQ] & /@ {args}, op, orders},
    op = QuantumCircuitOperator[ops]["QuantumOperator", Method -> "Schrodinger"]["Sort"];
    orders = #["FullInputOrder"] & /@ ops;
    FullSimplify @ QuantumOperator[
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

QuantumOperator["FlipSign", opts___] := QuantumOperator[{"FlipSign", {1, 1, 1}, 2}, opts]

QuantumOperator[{"FlipSign", digits : {__Integer}, dim : _Integer ? Positive | Automatic : Automatic}, opts___] := With[{
    d = Replace[dim, Automatic :> Max[Max[digits] + 1, 2]],
    n = Length[digits]
},
    QuantumOperator[
        QuantumOperator[
            DiagonalMatrix[ReplacePart[ConstantArray[1, d ^ n], FromDigits[digits, d] + 1 -> -1]], d, 
            "Label" -> If[
                d == 2, 
                With[{index = PositionIndex[Most[digits]]}, 
                    Subscript["C", ToString[Last[digits]]][Lookup[index, 1, {}], 
                    Lookup[index, 0, {}]]
                ],
                "FlipSign"[Row[digits]]
            ]
        ],
        opts
    ] /; AllTrue[digits, Between[{0, d - 1}]]
]

QuantumOperator[{"FlipSign", s_String, args___}, opts___] := QuantumOperator[{"FlipSign", FromDigits /@ Characters[s], args}, opts]


QuantumOperator["S", opts___] := QuantumOperator[QuantumOperator[{"Phase", Pi / 2}, "Label" -> "S"], opts]

QuantumOperator["T", opts___] := QuantumOperator[QuantumOperator[{"Phase", Pi / 4}, "Label" -> "T"], opts]

QuantumOperator["V" | "SX", opts___] := QuantumOperator[QuantumOperator[Sqrt[QuantumOperator["X"]], "Label" -> "V"], opts]


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

QuantumOperator[{name : "C" | "Controlled" | "C0" | "Controlled0", qmo_ ? QuantumMeasurementOperatorQ, args___}, opts___] :=
    QuantumOperator[{name, qmo["SuperOperator"], args}, opts]

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


QuantumOperator[{name : "C" | "Controlled" | "C0" | "Controlled0", qo_ ? QuantumOperatorQ}, opts___] :=
    QuantumOperator[{name, qo, {First[Complement[Range[Max[qo["InputOrder"]] + 1], qo["InputOrder"]]]}}, opts]

QuantumOperator[{name : "C" | "Controlled" | "C0" | "Controlled0", qo_ ? QuantumOperatorQ, control___}, target_ ? orderQ, opts___] :=
    QuantumOperator[{name, QuantumOperator[qo, target], control}, opts]

QuantumOperator[{"C0" | "Controlled0", qo_ ? QuantumOperatorQ, control0 : _ ? orderQ | {}}, opts___] := QuantumOperator[{"Controlled", qo, {}, control0}, opts]


QuantumOperator[{"C" | "Controlled", qo_ ? QuantumOperatorQ /; qo["ControlOrder"] =!= {}, control1 : _ ? orderQ | {}, control0 : _ ? orderQ | {} : {}}, opts___] :=
    QuantumOperator[{"Controlled", qo["TargetOperator"],
        Complement[Union[qo["ControlOrder1"], control1], control0],
        Complement[Union[qo["ControlOrder0"], control0], control1]
    }, opts]

QuantumOperator[{"C" | "Controlled", qo_ ? QuantumOperatorQ, control1 : _ ? orderQ | {}, control0 : _ ? orderQ | {} : {}}, opts___] /; qo["VectorQ"] := Enclose @ With[{
    controls1 = Length[control1],
    controls0 = Length[control0],
    control = Join[control0, control1],
    op = qo["Sort"]
},
    (*ConfirmAssert[! IntersectingQ[qo["Order"], control], "Target and control qudits shouldn't intersect"];*)
    QuantumOperator[
        blockDiagonalMatrix[{
            identityMatrix[(2 ^ controls1 - 1) qo["MatrixNameDimensions"]],
            op["Matrix"],
            identityMatrix[(2 ^ controls0 - 1) 2 ^ controls1 qo["MatrixNameDimensions"]]
        }],
        With[{order = Join @@ NestWhile[
                Apply[
                    Block[{
                        lhs = Intersection[#1, #2], rhs},
                        rhs = Take[DeleteElements[Range[Min[#1, #2], Max[#1, #2] + Length[lhs]], #1], UpTo[Length[lhs]]];
                        {Join[#1, rhs], DeleteElements[#2, lhs]}
                    ] &
                ],
                {control, #},
                Apply[IntersectingQ]
            ] & /@ op["Order"]
        },
            order
        ],
        QuantumTensorProduct[
            QuantumBasis[QuditBasis[2, controls1], QuditBasis[2, controls1]],
            QuantumBasis[QuditBasis[2, controls0], QuditBasis[2, controls0]],
            op["Basis"]
        ],
        opts,
        "Label" -> Subscript["C", op["Label"]][control1, control0]
    ]
]

QuantumOperator[{name : "C" | "Controlled" | "C0" | "Controlled0", qo_ ? QuantumOperatorQ, args___}, opts___] /; qo["MatrixQ"] := Enclose @ Block[{
    cop = ConfirmBy[QuantumOperator[{name, qo["Bend"], args}], QuantumOperatorQ]["Sort"], control, target, targetDimensions
},
    control = cop["ControlOrder"];
    target = cop["TargetOrder"];
    ConfirmAssert[control =!= target =!= {}];
    targetDimensions = Take[cop["TargetDimensions"], Length[target] / 2];
    target = Thread[TakeDrop[target, Length[target] / 2]];
    QuantumOperator[
        QuantumCircuitOperator[{
            "Measure" -> control,
            MapThread["Uncurry"[#2] -> {First[#1]} -> #1 &, {target, targetDimensions}],
            cop,
            MapThread["Curry"[#2] -> #1 -> {First[#1]} &, {target, targetDimensions}],
            "Encode" -> control
        }],
        opts,
        "Label" -> ReplacePart[cop["Label"], {0, 2} -> qo["Label"]]
    ]["Undouble"] //
        QuantumOperator[#,
            QuantumBasis[
                "Output" -> QuantumTensorProduct @ ReplacePart[#["Output"]["Decompose"], Thread[(qo["OutputOrder"] /. #["OutputOrderQuditMapping"]) -> qo["Output"]["Decompose"]]],
                "Input" -> QuantumTensorProduct @ ReplacePart[#["Input"]["Decompose"], Thread[(qo["InputOrder"] /. #["InputOrderQuditMapping"]) -> qo["Input"]["Decompose"]]],
                #["Basis"]["Options"]
            ]
        ] &
]

(* QuantumOperator[{name : "C" | "Controlled" | "C0" | "Controlled0", qo_ ? QuantumOperatorQ, args___}, opts___] /; qo["MatrixQ"] := Enclose @ Block[{
    weights, vectors, ops
},
    {weights, vectors} = qo["State"]["Eigensystem"];
    ops = MapThread[Confirm[QuantumOperator[{name, QuantumOperator[QuantumState[Sqrt[#1] #2, qo["Basis"]], qo["Order"]], args}, opts]] &, {weights, vectors}];
    QuantumOperator[
        QuantumState[Total[Through[ops["DensityMatrix"]]], First[ops]["Basis"]],
        First[ops]["Order"]
    ]
] *)


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
    QuantumOperator[op, order, QuantumBasis[Join[Table @@@ FactorInteger[op["OutputDimension"]]], Sequence @@ op["Basis"]["Options"]]] /; op["OutputDimension"] == op["InputDimension"]
]


QuantumOperator["Fourier", opts___] := QuantumOperator[{"Fourier", 2}, opts]

QuantumOperator[{"Fourier", dimension : _Integer ? Positive}, order : (_ ? orderQ) : {1}, opts___] := QuantumOperator[
    QuantumOperator[
        FourierMatrix[dimension ^ Length[order]],
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
        ConjugateTranspose[FourierMatrix[dimension ^ Length[order]]],
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

QuantumOperator["SWAP", opts___] := QuantumOperator[{"SWAP"}, opts]

QuantumOperator[{"SWAP", dimension : _Integer ? Positive : 2}, opts___] :=
    QuantumOperator[QuantumOperator[{"Permutation", {dimension, dimension}, Cycles[{{1, 2}}]}, opts], "Label" -> "SWAP"]

QuantumOperator["RootSWAP", opts___] := QuantumOperator[{"RootSWAP"}, opts]

QuantumOperator[{"RootSWAP", dimension : _Integer ? Positive : 2}, opts___] :=
    QuantumOperator[Sqrt[QuantumOperator[{"SWAP", dimension}, opts]], "Label" -> "RootSWAP"]


QuantumOperator["Braid", opts___] := QuantumOperator[{"Braid"}, opts]

QuantumOperator[{"Braid", method_ : "Bell"}, opts___] := QuantumOperator[
    Switch[method, "Bell", QuditBasis["Bell"]["Matrix"], "SwapPhase" | _, ReplacePart[PermutationMatrix[{1, 3, 2, 4}], {4, 4} -> -1]],
    opts,
    "Label" -> "Braid"
]


QuantumOperator["SUM", opts___] := QuantumOperator[{"SUM", 2}, opts]

QuantumOperator[{"SUM", dimension : _Integer ? Positive}, opts___] := QuantumOperator[
    QuantumOperator[
        SparseArray[{output_, input_} :>
            With[{
                i = IntegerDigits[input - 1, dimension, 2],
                o = IntegerDigits[output - 1, dimension, 2]
            },
                1 /; i[[1]] == o[[1]] && o[[2]] == Mod[Total[i], dimension]
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

QuantumOperator[{"PauliX" | "X" | "Shift", dimension : _Integer ? Positive}, opts___] := QuantumOperator[
    QuantumOperator[pauliMatrix[1, dimension], dimension, "Label" -> "X"],
    opts
]

QuantumOperator[{"PauliY" | "Y", dimension : _Integer ? Positive}, opts___] := QuantumOperator[
    QuantumOperator[pauliMatrix[2, dimension], dimension, "Label" -> "Y"],
    opts
]

QuantumOperator[{"PauliZ" | "Z" | "ShiftPhase", dimension : _Integer ? Positive}, opts___] := QuantumOperator[
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


QuantumOperator["Hadamard" | "H", opts___]  := QuantumOperator[{"H"}, opts]

QuantumOperator[{"Hadamard" | "H", dim : _Integer ? NonNegative : 2}, order : _ ? orderQ : {1}, opts___] :=
    QuantumOperator[
        {"Fourier", dim} -> order,
        opts,
        "Label" -> If[Length[order] > 1, Superscript["H", CircleTimes[Length[order]]], "H"]
    ]


QuantumOperator["Toffoli", order : (_ ? orderQ) : {1, 2, 3}] :=
    QuantumOperator[{"Controlled", "NOT", Most[order]}, {Last[order]}]


QuantumOperator["CSWAP" | "Fredkin", opts___] := QuantumOperator[{"Controlled", "SWAP" -> {2, 3}}, opts]



QuantumOperator["RandomUnitary", order : {outputOrder_ ? orderQ, inputOrder_ ? orderQ}, opts___] := Enclose @
    QuantumOperator[{"RandomUnitary", ConfirmBy[QuantumBasis[QuditBasis[2, Length[outputOrder]], QuditBasis[2, Length[inputOrder]]], QuantumBasisQ]}, order, opts]


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
        order, qb, opts, "Label" -> None
    ]

QuantumOperator[{"RandomUnitary", args___}, order : (_ ? autoOrderQ) : {1}, opts___] := Enclose @
    QuantumOperator[{"RandomUnitary", ConfirmBy[QuantumBasis[args], QuantumBasisQ]}, order, opts]


QuantumOperator["RandomHermitian", order : (_ ? orderQ) : {1}, opts___] := Enclose @
    QuantumOperator[{"RandomHermitian", ConfirmBy[QuantumBasis[2, Length[order]], QuantumBasisQ]}, order, opts]

QuantumOperator[{"RandomHermitian", qb_ ? QuantumBasisQ}, order : (_ ? orderQ) : {1}, opts___] :=
    QuantumState["RandomMixed", QuantumBasis[qb, opts, "Label" -> None]]["Operator", order]

QuantumOperator[{"RandomHermitian", args___}, order : (_ ? orderQ) : {1}] := Enclose @
    QuantumOperator[{"RandomHermitian", ConfirmBy[QuantumBasis[args], QuantumBasisQ]}, order]


QuantumOperator["Permutation", opts___] := QuantumOperator[{"Permutation", 2, Cycles[{{1, 2}}]}, opts]

QuantumOperator["Permutation", order_ ? orderQ, opts___] := QuantumOperator[{"Permutation", ConstantArray[2, Length[order]]}, Sort[order] -> order, opts]

QuantumOperator["Permutation", {out_ ? orderQ, in_ ? orderQ} | (in_ ? orderQ -> out_ ? orderQ), opts___] /; Length[out] == Length[in] :=
    QuantumOperator[{"Permutation", ConstantArray[2, Length[out]]}, {out, in}, opts]

QuantumOperator[{"Permutation", perm_Cycles : Cycles[{}]}, opts___] := QuantumOperator[{"Permutation", 2, perm}, opts]

QuantumOperator[{"Permutation", dim : _Integer ? Positive, perm_Cycles : Cycles[{}]}, opts___] := QuantumOperator[{"Permutation", Table[dim, Max[PermutationMax[perm], 1]], perm}, opts]

QuantumOperator[{"Permutation", dims : {_Integer ? Positive..}, perm_Cycles : Cycles[{}]}, opts___] :=
    Enclose @ QuantumOperator[
        QuantumState[
            SparseArrayFlatten @ TensorTranspose[ArrayReshape[identityMatrix[Times @@ dims], Join[dims, dims]], perm],
            QuantumBasis[QuditBasis[ConfirmBy[Permute[dims, perm], ListQ]], QuditBasis[dims], "Label" -> "\[Pi]" @@ PermutationList[perm, Length[dims]]]
        ],
        opts
    ]

QuantumOperator[{"Permutation", dims : {_Integer ? Positive..} | Automatic : Automatic, perm_List}, opts___] :=
    QuantumOperator[{"Permutation", Replace[dims, Automatic :> ConstantArray[2, Length[perm]]], PermutationCycles[perm]}, opts]


QuantumOperator[{name : "Curry" | "Uncurry", args__ : 2}, opts___] := With[{basis = QuditBasis[args]}, QuantumOperator[{name, {basis, basis}}, opts]]

QuantumOperator[{name : "Curry" | "Uncurry", args_List}, opts___] := With[{bases = QuditBasis /@ args},
    QuantumOperator[
        QuantumState[
            SparseArrayFlatten @ identityMatrix[Times @@ Through[bases["Dimension"]]],
            If[ name === "Curry",
                QuantumBasis[QuditBasis[Tuples[Through[bases["Names"]]]], QuantumTensorProduct[bases]],
                QuantumBasis[QuantumTensorProduct[bases], QuditBasis[Tuples[Through[bases["Names"]]]]]
            ]
        ],
        opts,
        "Label" -> name
    ]
]


QuantumOperator[{name : "XSpider" | "YSpider" | "ZSpider", phase_ : 0},
    order : {outputOrder : _ ? orderQ, inputOrder : _ ? orderQ} | (inputOrder : _ ? orderQ -> outputOrder : _ ? orderQ), opts___] := QuantumOperator[{
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


QuantumOperator[name : "Spider" | "SimpleSpider", opts___] := QuantumOperator[{name, QuantumBasis[QuditBasis[2], QuditBasis[2]]}, opts]

QuantumOperator[{name : "Spider" | "SimpleSpider", args_, phase_ : 0}, opts___] := Enclose @ QuantumOperator[{name, Confirm @ QuantumBasis[args], phase}, opts]

QuantumOperator[{"Spider", basis_ ? QuantumBasisQ, phase_ : 0}, opts___] := Enclose @ Block[{
    phases, dims = Catenate[Table @@@ Catenate[FactorInteger[#] & /@ basis["Dimensions"]]], dim
},
    dim = Max[dims, 1];
    phases = Prepend[0] @ PadRight[Flatten[{phase}], dim - 1];
    QuantumOperator[
        Confirm @ QuantumState[
            If[ dim <= 1,
                {Exp[I First[phases, 0]]},
                SparseArrayFlatten @ SparseArray[Thread[Transpose[PadRight[Range[#], dim, #] & /@ dims] -> Exp[I phases]], dims]
            ],
            basis
        ],
        opts,
        "Label" -> "Spider"[phase]
    ]
]


QuantumOperator[{"SimpleSpider", basis_ ? QuantumBasisQ, phase_ : 0}, opts___] := Enclose @ Block[{
    phases, dims = basis["Dimensions"], dim
},
    dim = Max[dims, 1];
    phases = Prepend[0] @ PadRight[Flatten[{phase}], dim - 1];
    QuantumOperator[
        Confirm @ QuantumState[
            If[ dim <= 1,
                {Exp[I First[phases, 0]]},
                SparseArrayFlatten @ SparseArray[Thread[Transpose[PadRight[Range[#], dim, #] & /@ dims] -> Exp[I phases]], dims]
            ],
            basis
        ],
        opts,
        "Label" -> "Spider"[phase]
    ]
]

QuantumOperator[{"WSpider", n_Integer : 2, dim_Integer : 2}, opts___] := QuantumOperator[
    QuantumState[
        If[ dim <= 1,
            {1},
            SparseArrayFlatten @ SparseArray[Thread[
                Prepend[ConstantArray[1, n + 1]] @
                    Catenate @ Table[Prepend[i] @ ReplacePart[ConstantArray[1, n], j -> i], {i, 2, dim}, {j, n}] -> 1],
                Table[dim, n + 1]
            ]
        ],
        QuantumBasis[{dim}, Table[dim, n]]
    ],
    opts,
    "Label" -> "WSpider"
]

$Spider = "XSpider" | "YSpider" | "ZSpider" | "Spider" | "WSpider"

QuantumOperator[name : $Spider, opts___] := QuantumOperator[{name}, opts]

QuantumOperator[{name : $Spider, args___}, order : _ ? orderQ, opts___] :=
    QuantumOperator[{name, args}, {order, order}, opts]

QuantumOperator[{name : $Spider, args___}, opts : PatternSequence[] | PatternSequence[Except[_ ? autoOrderQ], ___]] :=
    QuantumOperator[{name, args}, {{1}, {1}}, opts]

QuantumOperator[name : "Measure" | "Encode" | "Copy" | "Decohere" | "Marginal" | "Discard" | "Trace" | "Cap" | "Cup", opts___] := QuantumOperator[{name}, opts]

QuantumOperator[{name : "Measure" | "Encode" | "Marginal" | "Discard" | "Trace", args___}, order : _ ? orderQ, opts___] /; Length[order] > 1 := QuantumTensorProduct[QuantumOperator[{name, args}, {#}, opts] & /@ order]

QuantumOperator[{"Measure", args__ : 2}, opts___] := With[{decohere = QuantumOperator["Decohere"[args], {1, 2} -> {1}]}, QuantumOperator[decohere @ QuantumOperator["Uncurry"[decohere["OutputDimension"]], {1} -> {1, 2}], opts, "Label" -> "Measure"]]

QuantumOperator[{"Encode", args__ : 2}, opts___] := With[{copy = QuantumOperator["Copy"[args], {1} -> {1, 2}]}, QuantumOperator[QuantumOperator["Curry"[copy["InputDimension"]], {1, 2} -> {1}] @ copy, opts, "Label" -> "Encode"]]

QuantumOperator[{"Copy", args__ : 2}, opts___] := With[{basis = QuditBasis[args]}, QuantumOperator[QuantumOperator[{"SimpleSpider", QuantumBasis[QuantumTensorProduct[basis, basis["Conjugate"]], basis]}, {1} -> {1, 2}], opts, "Label" -> "Copy"]]

QuantumOperator[{"Decohere", args__ : 2}, opts___] := With[{basis = QuditBasis[args]}, QuantumOperator[QuantumOperator[{"SimpleSpider", QuantumBasis[basis, QuantumTensorProduct[basis, basis["Conjugate"]]]}, {1, 2} -> {1}], opts, "Label" -> "Decohere"]]

QuantumOperator[{"Marginal", args__ : 4}, opts___] := With[{basis = QuditBasis[args]}, QuantumOperator[Sqrt[basis["Dimension"]] QuantumState["UniformSuperposition", basis]["Dagger"], opts, "Label" -> "Marginal"]]

QuantumOperator[{"Discard", args__ : 4}, opts___] := QuantumOperator[QuantumOperator[{"Spider", QuantumBasis[QuditBasis[1], QuditBasis[args]]}, {1} -> {}], opts, "Label" -> "Discard" ]

QuantumOperator[{"Trace", args__ : 2}, opts___] := With[{basis = QuditBasis[args]}, QuantumOperator[QuantumState[IdentityMatrix[basis["Dimension"]], basis]["Dagger"], opts, "Label" -> "Trace"]]

QuantumOperator[{"Reset", args___ : "0"}, opts___] := With[{state = QuantumState[args]},
    QuantumOperator[QuantumOperator[state] @ QuantumOperator["Trace"[state["Dimension"]]], opts, "Label" -> If[state["Label"] === None, None, "Reset"[state["Label"]]]]
]

QuantumOperator[{"Measurement", args__ : "I"}, opts___] := QuantumOperator[QuantumMeasurementOperator[args]["DiscardExtraQudits"], opts]

QuantumOperator[{"Channel", args__ : "BitFlip"}, opts___] := QuantumOperator[QuantumChannel[args]["DiscardExtraQudits"], opts]

QuantumOperator[{"Cup", args__ : 2}, order : _ ? orderQ : {1, 2}, opts___] /; Length[order] == 2 := With[{basis = QuditBasis[args]},
    QuantumOperator["SimpleSpider"[QuantumBasis[QuantumTensorProduct[basis, basis["Conjugate"]], QuditBasis[]]], {order, {}}, opts, "Label" -> "Cup"]
]

QuantumOperator[{"Cap", args__ : 2}, order : _ ? orderQ : {1, 2}, opts___] /; Length[order] == 2 := With[{basis = QuditBasis[args]},
    QuantumOperator["SimpleSpider"[QuantumBasis[QuditBasis[], QuantumTensorProduct[basis, basis["Conjugate"]]]], {{}, order}, opts, "Label" -> "Cap"]
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
    a["InputDimension"] == a["OutputDimension"] == b["InputDimension"] == b["OutputDimension"] && Length[order] == 2 := With[{q = Max[order] + 1},
QuantumPartialTrace[
	QuantumOperator[
        QuantumOperator[{"Controlled0", "SWAP"}, Append[order, q]] @
        QuantumOperator[b, order[[{2}]], order[[{2}]]] @
        QuantumOperator[a, {q}, {q}] @
        QuantumOperator["CSWAP", Append[order, q]],
        "Label" -> "\[ScriptCapitalS]"[a["Label"], b["Label"]]
    ],
	{q}
]]


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

jUp[j_] := Table[Sqrt[(j - m1) (j + m1 + 1)] KroneckerDelta[m1 + 1, m2], {m2, -j, j}, {m1, -j, j}]

jDown[j_] := Table[Sqrt[(j + m1) (j - m1 + 1)] KroneckerDelta[m1 - 1, m2], {m2, -j, j}, {m1, -j, j}]

jX[j_] := 1 / 2 (jUp[j] + jDown[j])

jY[j_] := 1 / (2 I) (jDown[j] - jUp[j])

jZ[j_] := DiagonalMatrix[Table[m, {m, j, -j, -1}]]

QuantumOperator[{"WignerD", j_ : 1 / 2, {a_, b_, c_}}, opts___] :=  QuantumOperator[QuantumOperator[wignerD[j, {a, b, c}], 2 j + 1], opts, "Label" -> "WignerD"[a, b, c]]

QuantumOperator[{"WignerD", j_ : 1 / 2, b_ : 0}, opts___] := QuantumOperator[QuantumOperator[wignerD[j, b], 2 j + 1], opts, "Label" -> "WignerD"[b]]

QuantumOperator[{"JX" | "AngularMomentumX", j_ : 1 / 2}, opts___] := QuantumOperator[QuantumOperator[QuantumOperator[jX[j], 2 j + 1], "JX"[j]], opts, "Label" -> "JX"]

QuantumOperator[{"JY" | "AngularMomentumY", j_ : 1 / 2}, opts___] := QuantumOperator[QuantumOperator[QuantumOperator[jY[j], 2 j + 1], "JY"[j]], opts, "Label" -> "JY"]

QuantumOperator[{"JZ" | "AngularMomentumZ", j_ : 1 / 2}, opts___] := QuantumOperator[QuantumOperator[QuantumOperator[jZ[j], 2 j + 1], "JZ"[j]], opts, "Label" -> "JZ"]

QuantumOperator[{name : "JX+" | "JY+" | "JZ+" | "JI+" | "J+", j_ : 1 / 2}, opts___] := QuantumOperator[QuantumOperator[jUp[j], StringDrop[name, -1][j]], opts, "Label" -> name]

QuantumOperator[{name : "JX-" | "JY-" | "JZ-" | "JI-" | "J-", j_ : 1 / 2}, opts___] := QuantumOperator[QuantumOperator[jDown[j], StringDrop[name, -1][j]], opts, "Label" -> name]

QuantumOperator[{"+" | "I+", args___}, opts___] := With[{qb = QuantumBasis[args]}, QuantumOperator[QuantumOperator[jUp[(qb["Dimension"] - 1) / 2], qb], opts, "Label" -> "+"]]

QuantumOperator[{"-" | "I-", args___}, opts___] := With[{qb = QuantumBasis[args]}, QuantumOperator[QuantumOperator[jDown[(qb["Dimension"] - 1) / 2], qb], opts, "Label" -> "-"]]


QuantumOperator[{"Double", args___}, opts___] := QuantumOperator[args, opts]["Double"]


HamiltonianMixedOperator[h_QuantumOperator] := Block[{d = h["Dimension"], H},
    H = QuantumOperator[h, {1}, d];
    QuantumOperator[QuantumState[ArrayReshape[Transpose[(H - QuantumOperator[Transpose[H], {2}])["Tensor"], 2 <-> 3], {d, d}], h["Basis"]], h["Order"]]
]

LindbladMixedOperator[l_QuantumOperator] := Block[{d = l["Dimension"], L, LL},
    L = QuantumOperator[l, {1}, d];
    LL = L["Dagger"] @ L;
    QuantumOperator[
        QuantumState[
            ArrayReshape[Transpose[(L["Dagger"] @ QuantumOperator[Transpose[L], {2}] - (LL + QuantumOperator[Transpose[LL], {2}]) / 2)["Tensor"], 2 <-> 3], {d, d}],
            l["Basis"]
        ],
        l["Order"]
    ]
]

QuantumOperator[{"Liouvillian", H : _QuantumOperator | None : None, Ls : {___QuantumOperator} : {}, gammas_List : {}}, opts___] := Enclose[
	ConfirmAssert[SameQ @@ Join[If[H === None, {}, {H["OutputDimension"], H["InputDimension"]}], Through[Ls["OutputDimension"]], Through[Ls["InputDimension"]]]];
    QuantumOperator[
        If[H === None, 0, HamiltonianMixedOperator[H] / I] + PadRight[gammas, Length[Ls], 1] . (LindbladMixedOperator /@ Ls),
        opts, "Label" -> "Liouvillian"
    ]
]

QuantumOperator[{"Hamiltonian", args___}, opts___] := QuantumOperator[I QuantumOperator["Liouvillian"[args]], opts, "Label" -> "Hamiltonian"]


QuantumOperator[chain_String, opts___] := With[{chars = Characters[chain]},
    QuantumOperator[QuantumTensorProduct[MapIndexed[QuantumOperator, chars]], opts] /;
        ContainsOnly[chars, {"I", "X", "Y", "Z", "H", "S", "T", "V", "P"}]
]

QuantumOperator[name_String[args___], opts___] := QuantumOperator[{name, args}, opts]

$upperCasesOperatorNames := AssociationThread[ToUpperCase @ $QuantumOperatorNames, $QuantumOperatorNames]

QuantumOperator[name_String, opts___] /; ToUpperCase[name] =!= name && KeyExistsQ[$upperCasesOperatorNames, name] :=
    QuantumOperator[$upperCasesOperatorNames[name], opts]

QuantumOperator[{name_String, params___}, opts___] /; ToUpperCase[name] =!= name && KeyExistsQ[$upperCasesOperatorNames, name] :=
    QuantumOperator[{$upperCasesOperatorNames[name], params}, opts]

QuantumOperator[name_String, opts___] /; MemberQ[$QuantumOperatorNames, name] :=
    QuantumOperator[{name}, opts]

QuantumOperator[rule : _Rule, opts___] := QuantumOperator[FromOperatorShorthand[Unevaluated[rule]], opts]

QuantumOperator[f_Symbol[args___], opts___] /; MemberQ[Attributes[f], NumericFunction] := QuantumOperator[FromOperatorShorthand[Unevaluated[f[args]]], opts]

QuantumOperator[ops : {Except[_QuantumOperator], ___}, opts___] /; AllTrue[ops, MatchQ[_Rule | _Integer | _String | ({name_, ___} | name_[___] /; MemberQ[$QuantumOperatorNames, name]) | _QuantumOperator]] :=
    Enclose @ QuantumOperator[
        QuantumCircuitOperator[Flatten[ConfirmBy[FromOperatorShorthand[#], QuantumOperatorQ] & /@ ops]]["QuantumOperator", Method -> "Schrodinger"],
        opts
    ]

QuantumOperator[SuperDagger[arg_], opts___] := QuantumOperator[arg, opts]["Dagger"]

