Package["QuantumFramework`"]



QuantumDiscreteState[name_ ? nameQ, args : PatternSequence[] | Except[PatternSequence[_Integer ? Positive, ___]]] := QuantumDiscreteState[name, 2, args]


QuantumDiscreteState["Plus", args___] := QuantumDiscreteState[{1, 1}, args]

QuantumDiscreteState["Minus", args___] := QuantumDiscreteState[{1, -1}, args]

QuantumDiscreteState["Left", args___] := QuantumDiscreteState[{1, I}, args]

QuantumDiscreteState["Right", args___] := QuantumDiscreteState[{1, -I}, args]

QuantumDiscreteState["PsiPlus", args___] := QuantumDiscreteState[{1, 0, 0, 1}, args]

QuantumDiscreteState["PsiMinus", args___] := QuantumDiscreteState[{1, 0, 0, -1}, args]

QuantumDiscreteState["PhiPlus", args___] := QuantumDiscreteState[{0, 1, 1, 0}, args]

QuantumDiscreteState["PhiMinus", args___] := QuantumDiscreteState[{0, 1, -1, 0}, args]


QuantumDiscreteState[{"BasisState", basisElement_List}, args___] := QuantumDiscreteState[{"BasisState", basisElement}, 2, args]

QuantumDiscreteState[{"BasisState", basisElement_List}, dimension_Integer ? Positive, args___] := QuantumDiscreteState[
    With[{elementPosition = FromDigits[basisElement, dimension] + 1, basisSize = Length[basisElement]},
        SparseArray[{elementPosition} -> 1, {dimension ^ basisSize}]
    ],
    dimension,
    args
]


QuantumDiscreteState[{"Register", subsystemCount_Integer}, args___] := QuantumDiscreteState[{"Register", subsystemCount, 0}, args]

QuantumDiscreteState[{"Register", subsystemCount_Integer, state_Integer ? NonNegative}, dimension_Integer ? Positive, args___] :=
    QuantumDiscreteState[SparseArray[{{state + 1} -> 1}, {dimension ^ subsystemCount}], dimension, args]


QuantumDiscreteState["UniformSuperposition", args___] := QuantumDiscreteState[{"UniformSuperposition", 1}, args]

QuantumDiscreteState[{"UniformSuperposition", subsystemCount_Integer}, dimension_Integer ? Positive, args___] :=
    QuantumDiscreteState[ConstantArray[1, dimension ^ subsystemCount], dimension, args]


QuantumDiscreteState["UniformMixture", args___] := QuantumDiscreteState[{"UniformMixture", 1}, args]

QuantumDiscreteState[{"UniformMixture", subsystemCount_Integer}, dimension_Integer ? Positive, args___] :=
    QuantumDiscreteState[IdentityMatrix[dimension ^ subsystemCount], dimension, args]


QuantumDiscreteState["RandomPure", args___] :=  QuantumDiscreteState[{"RandomPure", 1}, args]

QuantumDiscreteState[{"RandomPure", subsystemCount_Integer}, dimension_Integer ? Positive, args___] :=
    QuantumDiscreteState[RandomComplex[{-1 - I, 1 + I}, dimension ^ subsystemCount], dimension, args]


QuantumDiscreteState["GHZ", args___] := QuantumDiscreteState[{"GHZ", 3}, args]

QuantumDiscreteState[{"GHZ", subsystemCount_Integer}, dimension_Integer ? Positive, args___] :=
    QuantumDiscreteState[SparseArray[{{1} -> 1, {dimension ^ subsystemCount} -> 1}, {dimension ^ subsystemCount}], dimension, args]


QuantumDiscreteState["W", args___] := QuantumDiscreteState[{"W", 3}, args]

QuantumDiscreteState[{"W", subsystemCount_Integer}, dimension_Integer ? Positive, args___] :=
    QuantumDiscreteState[SparseArray[{element_} /; IntegerQ[Log[dimension, element - 1]] -> 1, {dimension ^ subsystemCount}], dimension, args]


QuantumDiscreteState["Werner", args___] := QuantumDiscreteState[{"Werner", 0}, args]

QuantumDiscreteState[{"Werner", relativeWeight_}, args___] := Module[{
    phiMinus, phiMinusDensityMatrix, densityMatrix
},
    phiMinus = {1, 0, 0, -1};
    phiMinusDensityMatrix = SparseArray[ConjugateTranspose[{phiMinus}]] . SparseArray[{phiMinus}];
    densityMatrix = (relativeWeight phiMinusDensityMatrix) + ((1 - relativeWeight) / 4) IdentityMatrix[4];
    QuantumDiscreteState[densityMatrix, args]
]


QuantumDiscreteState[{"Graph", graph_ ? GraphQ}, args___] := Module[{
    indexGraph, quditCount, uniformSuperposition, entanglements
},
    indexGraph = IndexGraph[graph];
    quditCount = VertexCount[indexGraph];
    uniformSuperposition = ConstantArray[1, 2 ^ quditCount];
    entanglements = OperatorApplied[Take][2] @* List @@@ EdgeList[indexGraph];

    QuantumDiscreteState[
        Fold[
            OrderedMatrixRepresentation[controlledZGate, quditCount, #2] . #1 &,
            uniformSuperposition,
            entanglements
        ],
        args
    ]
]

