Package["Wolfram`QuantumFramework`"]

PackageScope["$QuantumStateNames"]



$QuantumStateNames = {
    "0", "Zero", "1", "One",
    "Plus", "Minus", "Left", "Right",
    "PsiPlus", "PsiMinus", "PhiPlus", "PhiMinus",
    "BasisState", "Register",
    "UniformSuperposition",
    "UniformMixture",
    "RandomPure", "RandomMixed",
    "GHZ", "Bell",
    "W",
    "Werner",
    "Graph",
    "BlochVector"
}

(*QuantumState[name_ ? nameQ, args : PatternSequence[] | Except[PatternSequence[_Integer ? Positive, ___]]] := QuantumState[name, 2, args]*)

QuantumState[name_ ? nameQ, basisName : Except[Alternatives @@ $QuantumBasisPictures, _ ? nameQ]] :=
    QuantumState[QuantumState[name], QuantumBasis[basisName]]

QuantumState[] := QuantumState["0"]

QuantumState[""] := QuantumState[{}, QuantumBasis[1]]

QuantumState["Zero", args___] := QuantumState["0", args]

QuantumState["One", args___] := QuantumState["1", args]

QuantumState[s_String /; StringMatchQ[s, DigitCharacter..], dim : (_Integer ? Positive) : 2, args___] := With[{
    digits = Clip[Interpreter[DelimitedSequence["Digit", ""]] @ s, {0, dim - 1}]
},
    QuantumState[{"BasisState", digits}, dim, "Label" -> s, args]
]

QuantumState["Plus", args___] := QuantumState[Normalize @ {1, 1}, "Label" -> "+", args]

QuantumState["Minus", args___] := QuantumState[Normalize @ {-1, 1}, "Label" -> "-", args]

QuantumState["Left", args___] := QuantumState[Normalize @ {I, 1}, "Label" -> "L", args]

QuantumState["Right", args___] := QuantumState[Normalize @ {-I, 1}, "Label" -> "R", args]

QuantumState["PhiPlus", args___] := QuantumState[Normalize @ {1, 0, 0, 1}, "Label" -> "\*SubscriptBox[\[CapitalPhi], \(+\)]", args]

QuantumState["PhiMinus", args___] := QuantumState[Normalize @ {1, 0, 0, -1}, "Label" -> "\*SubscriptBox[\[CapitalPhi], \(-\)]", args]

QuantumState["PsiPlus", args___] := QuantumState[Normalize @ {0, 1, 1, 0}, "Label" -> "\*SubscriptBox[\[CapitalPsi], \(+\)]", args]

QuantumState["PsiMinus", args___] := QuantumState[Normalize @ {0, 1, -1, 0}, "Label" -> "\*SubscriptBox[\[CapitalPsi], \(-\)]", args]

QuantumState[{name : "Plus" | "Minus" | "Left" | "Right" | "PsiPlus" | "PsiMinus" | "PhiPlus" | "PhiMinus", n_Integer ? Positive}, args___] :=
    QuantumTensorProduct @ Table[QuantumState[name, args], n]



QuantumState[{"BasisState", basisElement_List}, args___] := QuantumState[{"BasisState", basisElement}, 2, args]

QuantumState[{"BasisState", basisElement_List}, dimension : (_Integer ? Positive) : 2, args___] := QuantumState[
    With[{elementPosition = FromDigits[basisElement, dimension] + 1, basisSize = Length[basisElement]},
        SparseArray[{elementPosition} -> 1, {dimension ^ basisSize}]
    ],
    dimension,
    args
]


QuantumState[{"Register", subsystemCount_Integer}, args___] := QuantumState[{"Register", subsystemCount, 0}, args]

QuantumState[{"Register", subsystemCount_Integer, state_Integer ? NonNegative}, dimension : (_Integer ? Positive) : 2, args___] :=
    QuantumState[SparseArray[{{state + 1} -> 1}, {dimension ^ subsystemCount}], dimension, args]


QuantumState["UniformSuperposition", args___] := QuantumState[{"UniformSuperposition", 1}, args]

QuantumState[{"UniformSuperposition", subsystemCount_Integer}, dimension : (_Integer ? Positive) : 2, args___] :=
    QuantumState[ConstantArray[1, dimension ^ subsystemCount], dimension, args]["Normalized"]


QuantumState["UniformMixture", args___] := QuantumState[{"UniformMixture", 1}, args]

QuantumState[{"UniformMixture", subsystemCount_Integer}, dimension : (_Integer ? Positive) : 2, args___] :=
    QuantumState[identityMatrix[dimension ^ subsystemCount] / (dimension ^ subsystemCount), dimension, args]


QuantumState[{"RandomPure", subsystemCount_Integer}, dimension : (_Integer ? Positive) : 2, args___] :=
    QuantumState[RandomComplex[{-1 - I, 1 + I}, dimension ^ subsystemCount], dimension, args]

QuantumState["RandomPure", args : PatternSequence[Except[_ ? QuantumBasisQ], ___]] :=  QuantumState["RandomPure", QuantumBasis[args]]

QuantumState["RandomPure", qb_ ? QuantumBasisQ] :=
    QuantumState[RandomComplex[{-1 - I, 1 + I}, qb["Dimension"]], qb]

QuantumState["RandomPure"] := QuantumState["RandomPure", QuantumBasis[]]


QuantumState[{"RandomMixed", subsystemCount_Integer}, dimension : (_Integer ? Positive) : 2, args___] :=
    With[{m = RandomComplex[{-1 - I, 1 + I}, Table[dimension ^ subsystemCount, 2]]},
        QuantumState[m . ConjugateTranspose[m], dimension, args]
    ]

QuantumState["RandomMixed", args : PatternSequence[Except[_ ? QuantumBasisQ], ___]] :=  QuantumState["RandomMixed", QuantumBasis[args]]

QuantumState["RandomMixed", qb_ ? QuantumBasisQ] :=
    With[{m = RandomComplex[{-1 - I, 1 + I}, Table[qb["Dimension"], 2]]},
        QuantumState[m . ConjugateTranspose[m], qb]
    ]

QuantumState["RandomMixed"] := QuantumState["RandomMixed", QuantumBasis[]]


QuantumState["GHZ", args___] := QuantumState[{"GHZ", 3}, args]

QuantumState[{"GHZ", subsystemCount_Integer}, dimension : (_Integer ? Positive) : 2, args___] :=
    QuantumState[SparseArray[{{1} -> 1 / Sqrt[2], {dimension ^ subsystemCount} -> 1 / Sqrt[2]}, {dimension ^ subsystemCount}], dimension, args]

QuantumState["Bell", args___] := QuantumState[{"GHZ", 2}, args]


QuantumState["W", args___] := QuantumState[{"W", 3}, args]

QuantumState[{"W", subsystemCount_Integer}, dimension : (_Integer ? Positive) : 2, args___] :=
    QuantumState[SparseArray[{element_} /; IntegerQ[Log[dimension, element - 1]] -> 1 / Sqrt[subsystemCount], {dimension ^ subsystemCount}], dimension, args]



wernerState[p_, qb_QuditBasis] /; qb["Qudits"] == 2 :=
    Module[{dim = qb["Dimension"], d = First[qb["Dimensions"]], fAB, sym, as},
        fAB = QuantumOperator[{"Permutation", qb["Dimensions"], Cycles[{{1, 2}}]}]["Matrix"];
        sym = IdentityMatrix[dim] + fAB;
        as = IdentityMatrix[dim] - fAB;
        QuantumState[p sym 1 / (dim + d) + 1 / (dim - d) (1 - p) as // FullSimplify, qb]
    ]

QuantumState["Werner", args___] := QuantumState[{"Werner", .5, 2}, args]

QuantumState[{"Werner", p_, param_ : 2}, args___] := QuantumState[{"Werner", p, QuditBasis[param]}, args]

QuantumState[{"Werner", p_, qb_ ? QuditBasisQ}, args___] := With[{
    basis = QuditBasis[qb, 2]
},
    QuantumState[wernerState[p, basis], args]
]


QuantumState[{"Graph", graph_ ? GraphQ}, args___] := Module[{
    indexGraph, quditCount, entanglements
},
    indexGraph = IndexGraph[graph];
    quditCount = VertexCount[indexGraph];
    entanglements = OperatorApplied[Take][2] @* List @@@ EdgeList[indexGraph];

    QuantumState[
        Fold[
            QuantumOperator["CZ", #2] @ #1 &,
            QuantumState[{"UniformSuperposition", quditCount}],
            entanglements
        ],
        args
    ]
]


QuantumState[{"BlochVector", r_ /; VectorQ[r] && Length[r] == 3}] :=
    QuantumState[1 / 2 (identityMatrix[2] + r . Table[PauliMatrix[i], {i, 3}])]

