Package["QuantumFramework`"]



QuantumState[name_ ? nameQ, args : PatternSequence[] | Except[PatternSequence[_Integer ? Positive, ___]]] := QuantumState[name, 2, args]


QuantumState["Plus", args___] := QuantumState[Normalize @ {1, 1}, args]

QuantumState["Minus", args___] := QuantumState[Normalize @ {1, -1}, args]

QuantumState["Left", args___] := QuantumState[Normalize @ {1, I}, args]

QuantumState["Right", args___] := QuantumState[Normalize @ {1, -I}, args]

QuantumState["PsiPlus", args___] := QuantumState[Normalize @ {1, 0, 0, 1}, args]

QuantumState["PsiMinus", args___] := QuantumState[Normalize @ {1, 0, 0, -1}, args]

QuantumState["PhiPlus", args___] := QuantumState[Normalize @ {0, 1, 1, 0}, args]

QuantumState["PhiMinus", args___] := QuantumState[Normalize @ {0, 1, -1, 0}, args]

QuantumState[{name : "Plus" | "Minus" | "Left" | "Right" | "PsiPlus" | "PsiMinus" | "PhiPlus" | "PhiMinus", n_Integer ? Positive}, args___] :=
    QuantumTensorProduct @ Table[QuantumState[name, args], n]



QuantumState[{"BasisState", basisElement_List}, args___] := QuantumState[{"BasisState", basisElement}, 2, args]

QuantumState[{"BasisState", basisElement_List}, dimension_Integer ? Positive, args___] := QuantumState[
    With[{elementPosition = FromDigits[basisElement, dimension] + 1, basisSize = Length[basisElement]},
        SparseArray[{elementPosition} -> 1, {dimension ^ basisSize}]
    ],
    dimension,
    args
]


QuantumState[{"Register", subsystemCount_Integer}, args___] := QuantumState[{"Register", subsystemCount, 0}, args]

QuantumState[{"Register", subsystemCount_Integer, state_Integer ? NonNegative}, dimension_Integer ? Positive, args___] :=
    QuantumState[SparseArray[{{state + 1} -> 1}, {dimension ^ subsystemCount}], dimension, args]


QuantumState["UniformSuperposition", args___] := QuantumState[{"UniformSuperposition", 1}, args]

QuantumState[{"UniformSuperposition", subsystemCount_Integer}, dimension_Integer ? Positive, args___] :=
    QuantumState[ConstantArray[1, dimension ^ subsystemCount], dimension, args]


QuantumState["UniformMixture", args___] := QuantumState[{"UniformMixture", 1}, args]

QuantumState[{"UniformMixture", subsystemCount_Integer}, dimension_Integer ? Positive, args___] :=
    QuantumState[IdentityMatrix[dimension ^ subsystemCount], dimension, args]


QuantumState["RandomPure", args___] :=  QuantumState[{"RandomPure", 1}, args]

QuantumState[{"RandomPure", subsystemCount_Integer}, dimension_Integer ? Positive, args___] :=
    QuantumState[RandomComplex[{-1 - I, 1 + I}, dimension ^ subsystemCount], dimension, args]


QuantumState["GHZ", args___] := QuantumState[{"GHZ", 3}, args]

QuantumState[{"GHZ", subsystemCount_Integer}, dimension_Integer ? Positive, args___] :=
    QuantumState[SparseArray[{{1} -> 1, {dimension ^ subsystemCount} -> 1}, {dimension ^ subsystemCount}], dimension, args]


QuantumState["W", args___] := QuantumState[{"W", 3}, args]

QuantumState[{"W", subsystemCount_Integer}, dimension_Integer ? Positive, args___] :=
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
            OrderedMatrixRepresentation[controlledZGate, quditCount, #2] . #1 &,
            uniformSuperposition,
            entanglements
        ],
        args
    ]
]

