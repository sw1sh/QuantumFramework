Package["Wolfram`QuantumFramework`"]

PackageScope["$QuantumStateNames"]



$QuantumStateNames = {
    "0", "Zero", "1", "One",
    "Plus", "Minus", "Left", "Right",
    "PsiPlus", "PsiMinus", "PhiPlus", "PhiMinus",
    "BasisState", "Register",
    "UniformSuperposition",
    "UniformMixture",
    "RandomPure",
    "GHZ",
    "W",
    "Werner",
    "Graph"
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

QuantumState["Left", args___] := QuantumState[Normalize @ {1, I}, "Label" -> "L", args]

QuantumState["Right", args___] := QuantumState[Normalize @ {1, -I}, "Label" -> "R", args]

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
    QuantumState[ConstantArray[1, dimension ^ subsystemCount], dimension, args]


QuantumState["UniformMixture", args___] := QuantumState[{"UniformMixture", 1}, args]

QuantumState[{"UniformMixture", subsystemCount_Integer}, dimension : (_Integer ? Positive) : 2, args___] :=
    QuantumState[IdentityMatrix[dimension ^ subsystemCount], dimension, args]


QuantumState[{"RandomPure", subsystemCount_Integer}, dimension : (_Integer ? Positive) : 2, args___] :=
    QuantumState[RandomComplex[{-1 - I, 1 + I}, dimension ^ subsystemCount], dimension, args]

QuantumState["RandomPure", args : PatternSequence[Except[_ ? QuantumBasisQ], ___]] :=  QuantumState["RandomPure", QuantumBasis[args]]

QuantumState["RandomPure", qb_ ? QuantumBasisQ] :=
    QuantumState[Flatten @ RandomComplex[{-1 - I, 1 + I}, qb["Dimensions"]], qb]


QuantumState["GHZ", args___] := QuantumState[{"GHZ", 3}, args]

QuantumState[{"GHZ", subsystemCount_Integer}, dimension : (_Integer ? Positive) : 2, args___] :=
    QuantumState[SparseArray[{{1} -> 1, {dimension ^ subsystemCount} -> 1}, {dimension ^ subsystemCount}], dimension, args]


QuantumState["W", args___] := QuantumState[{"W", 3}, args]

QuantumState[{"W", subsystemCount_Integer}, dimension : (_Integer ? Positive) : 2, args___] :=
    QuantumState[SparseArray[{element_} /; IntegerQ[Log[dimension, element - 1]] -> 1, {dimension ^ subsystemCount}], dimension, args]


QuantumState["Werner", args___] := QuantumState[{"Werner", 0}, args]

QuantumState[{"Werner", relativeWeight_}, args___] := Module[{
    phiMinus, phiMinusDensityMatrix, densityMatrix
},
    phiMinus = {1, 0, 0, -1};
    phiMinusDensityMatrix = SparseArray[ConjugateTranspose[{phiMinus}]] . SparseArray[{phiMinus}];
    densityMatrix = (relativeWeight phiMinusDensityMatrix) + ((1 - relativeWeight) / 4) IdentityMatrix[4];
    QuantumState[densityMatrix, args]
]


QuantumState[{"Graph", graph_ ? GraphQ}, args___] := Module[{
    indexGraph, quditCount, uniformSuperposition, entanglements
},
    indexGraph = IndexGraph[graph];
    quditCount = VertexCount[indexGraph];
    uniformSuperposition = ConstantArray[1, 2 ^ quditCount];
    entanglements = OperatorApplied[Take][2] @* List @@@ EdgeList[indexGraph];

    QuantumState[
        Fold[
            QuantumOperator["CZ", #2][{"OrderedMatrix", quditCount}] . #1 &,
            uniformSuperposition,
            entanglements
        ],
        args
    ]
]


QuantumState[{"BlochVector", r_ /; VectorQ[r] && Length[r] == 3}] :=
    QuantumState[1 / 2 (IdentityMatrix[2] + r . Table[PauliMatrix[i], {i, 3}])]

