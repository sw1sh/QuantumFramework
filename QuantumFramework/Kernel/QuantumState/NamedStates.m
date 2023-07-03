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
    "GHZ", "Bell", "Dicke",
    "W",
    "Werner",
    "Graph",
    "BlochVector"
}

(*QuantumState[name_ ? nameQ, args : PatternSequence[] | Except[PatternSequence[_Integer ? Positive, ___]]] := QuantumState[name, 2, args]*)

(* QuantumState[name_ ? nameQ, basisName : Except[Alternatives @@ $QuantumBasisPictures, _ ? nameQ]] :=
    QuantumState[QuantumState[name], QuantumBasis[basisName]] *)

QuantumState[] := QuantumState["0"]

QuantumState[""] := QuantumState[{}, QuantumBasis[1]]

QuantumState["Zero", args___] := QuantumState["0", args]

QuantumState["One", args___] := QuantumState["1", args]

QuantumState[s_String /; StringMatchQ[s, DigitCharacter..], dim : (_Integer ? Positive) : 2, args___] := With[{
    digits = Clip[Interpreter[DelimitedSequence["Digit", ""]] @ s, {0, dim - 1}]
},
    QuantumState[{"BasisState", digits}, dim, "Label" -> s, args]
]

QuantumState["Plus" | "+", args___] := QuantumState[Normalize @ {1, 1}, args, "Label" -> "+"]

QuantumState["Minus" | "-", args___] := QuantumState[Normalize @ {-1, 1}, args, "Label" -> "-"]

QuantumState["Left" | "L", args___] := QuantumState[Normalize @ {I, 1}, args, "Label" -> "L"]

QuantumState["Right" | "R", args___] := QuantumState[Normalize @ {-I, 1}, args, "Label" -> "R"]

QuantumState[s_String /; StringMatchQ[s, ("0" | "1" | "+" | "-" | "L" | "R") ..], args___] :=
    QuantumState[QuantumTensorProduct[QuantumState /@ Characters[s]], args]


QuantumState["PhiPlus", args___] := QuantumState[Normalize @ {1, 0, 0, 1}, args, "Label" -> "\*SubscriptBox[\[CapitalPhi], \(+\)]"]

QuantumState["PhiMinus", args___] := QuantumState[Normalize @ {1, 0, 0, -1}, args, "Label" -> "\*SubscriptBox[\[CapitalPhi], \(-\)]"]

QuantumState["PsiPlus", args___] := QuantumState[Normalize @ {0, 1, 1, 0}, args, "Label" -> "\*SubscriptBox[\[CapitalPsi], \(+\)]"]

QuantumState["PsiMinus", args___] := QuantumState[Normalize @ {0, 1, -1, 0}, args, "Label" -> "\*SubscriptBox[\[CapitalPsi], \(-\)]"]

QuantumState[{name : "Plus" | "Minus" | "Left" | "Right" | "PsiPlus" | "PsiMinus" | "PhiPlus" | "PhiMinus", n_Integer ? Positive}, args___] :=
    QuantumTensorProduct @ Table[QuantumState[name, args], n]



QuantumState[{"BasisState", basisElement_List : {1}}, args___] := QuantumState[{"BasisState", basisElement}, 2, args]

QuantumState[{"BasisState", basisElement_List : {1}}, dimension : (_Integer ? Positive) : 2, args___] := QuantumState[
    With[{elementPosition = FromDigits[basisElement, dimension] + 1, basisSize = Length[basisElement]},
        SparseArray[{elementPosition} -> 1, {dimension ^ basisSize}]
    ],
    dimension,
    args
]


QuantumState[{"Register", subsystemCount : _Integer ? Positive, state : _Integer ? NonNegative : 0}, dimension : (_Integer ? Positive), args___] :=
    QuantumState[SparseArray[{{state + 1} -> 1}, {dimension ^ subsystemCount}], Table[dimension, Max[subsystemCount, 1]], "Label" -> state, args]

QuantumState[{"Register", subsystemCount: _Integer ? Positive, state : _Integer ? NonNegative : 0}, args___] :=
    QuantumState[SparseArray[{{state + 1} -> 1}, {2 ^ subsystemCount}], args, "Label" -> state]

QuantumState[{"Register", 0, ___}, args___] := QuantumState[1, 1, args]

QuantumState[{"Register", dims : {__Integer ? Positive}, state : _Integer ? NonNegative : 0}, args___] :=
    QuantumState[SparseArray[{{state + 1} -> 1}, {Times @@ dims}], dims, args, "Label" -> state]

QuantumState[{"Register", basisArgs_, state : _Integer ? NonNegative : 0}, args___] := With[{qb = QuantumBasis[basisArgs]},
    QuantumState[SparseArray[{{state + 1} -> 1}, qb["Dimension"]], qb, args, "Label" -> state]
]


QuantumState["UniformSuperposition", args___] := With[{basis = QuantumBasis[args]},
    QuantumState[ConstantArray[1, basis["Dimension"]], basis]["Normalized"]
]

QuantumState[{"UniformSuperposition", subsystemCount_Integer}, args___] := With[{basis = QuantumBasis[args]},
    QuantumState[ConstantArray[1, basis["Dimension"] ^ subsystemCount], basis]["Normalized"]
]

QuantumState[{"UniformSuperposition", subsystemCount_Integer}, dimension : (_Integer ? Positive) : 2, args___] :=
    QuantumState[ConstantArray[1, dimension ^ subsystemCount], Table[dimension, subsystemCount], args]["Normalized"]


QuantumState["UniformMixture", args___] := QuantumState[{"UniformMixture", 1}, args]

QuantumState[{"UniformMixture", subsystemCount_Integer}, dimension : (_Integer ? Positive) : 2, args___] :=
    QuantumState[identityMatrix[dimension ^ subsystemCount] / (dimension ^ subsystemCount), dimension, args]


QuantumState[{"RandomPure", subsystemCount_Integer}, dimension : (_Integer ? Positive) : 2, args___] :=
    QuantumState["RandomPure", dimension, subsystemCount, args]

QuantumState["RandomPure", args : PatternSequence[Except[_ ? QuantumBasisQ], ___]] :=  QuantumState["RandomPure", QuantumBasis[args]]

QuantumState["RandomPure", qb_ ? QuantumBasisQ] :=
    QuantumState[QuantumOperator[{"RandomUnitary", qb}, Range[qb["OutputQudits"]]][], "Label" -> None]

QuantumState["RandomPure"] := QuantumState["RandomPure", QuantumBasis[]]


QuantumState[{"RandomMixed", subsystemCount_Integer}, dimension : (_Integer ? Positive) : 2, args___] :=
    With[{m = RandomComplex[{-1 - I, 1 + I}, Table[dimension ^ subsystemCount, 2]]},
        QuantumState[m . ConjugateTranspose[m], dimension, args]["Normalized"]
    ]

QuantumState["RandomMixed", args : PatternSequence[Except[_ ? QuantumBasisQ], ___]] :=  QuantumState["RandomMixed", QuantumBasis[args]]

QuantumState["RandomMixed", qb_ ? QuantumBasisQ] :=
    With[{m = RandomComplex[{-1 - I, 1 + I}, Table[qb["Dimension"], 2]]},
        QuantumState[m . ConjugateTranspose[m], qb]["Normalized"]
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


QuantumState["Graph", args___] := QuantumState[{"Graph", RandomGraph[{4, 4}]}, args]

QuantumState[{"Graph", graph : _ ? GraphQ}, args___] := Module[{
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

QuantumState["BlochVector", args___] := QuantumState[{"BlochVector", {0, 0, 1}}, args]

QuantumState[{"BlochVector", r_ /; VectorQ[r] && Length[r] == 3}, args___] :=
    QuantumState[1 / 2 (identityMatrix[2] + r . Table[PauliMatrix[i], {i, 3}]), args]

QuantumState[{"Dicke", n_Integer ? Positive, k_Integer}, args___] /; n >= k := QuantumState[{"Dicke", {n - k, k}}]

QuantumState[{"Dicke", k_}, args___] /; VectorQ[k, IntegerQ[#] && NonNegative[#] & ] := Block[{
    n = Total[k], dim = Length[k], s
},
    s = Table[QuantumState[{"Register", {dim}, i}], {i, 0, dim - 1}];
    Total[QuantumTensorProduct /@ Permutations @ Catenate[ConstantArray[#1, #2] & @@@ Transpose[{s, k}]]]["Normalized"]
]


QuantumState[SuperDagger[arg_], opts___] := QuantumState[arg, opts]["Dagger"]

