Package["QuantumFramework`"]

PackageScope["basisElementNameLength"]
PackageScope["normalBasisElementName"]
PackageScope["symbolicTensorQ"]
PackageScope["basisMultiplicity"]
PackageScope["nameQ"]
PackageScope["propQ"]
PackageScope["stateQ"]
PackageScope["operatorQ"]
PackageScope["kroneckerProduct"]
PackageScope["OrderedMatrixRepresentation"]


basisElementNameLength[name : _TensorProduct | _CircleTimes | _List] := Length @ name

basisElementNameLength[_] := 1


normalBasisElementName[name : _TensorProduct | _CircleTimes | _List] := List @@ name

normalBasisElementName[name_] := {name}


symbolicTensorQ[a_] := MatchQ[a, _Symbol] || TensorQ[a] && AnyTrue[Level[a, {-1}], MatchQ[_Symbol]]


basisMultiplicity[dim_, size_] := Quiet[Ceiling @ Log[size, dim] /. 0 | Indeterminate -> 1, Divide::indet]


nameQ[name_] := MatchQ[name, _String | {_String, ___}]

propQ[prop_] := MatchQ[prop, _String | {_String, ___}]

stateQ[state_] := !nameQ[state] && VectorQ[state] && Length[state] > 0 || SquareMatrixQ[state]

operatorQ[op_] := SquareMatrixQ[op]

orderQ[order_] := VectorQ[order, IntegerQ]


(* Matrix tools *)

kroneckerProduct[arg_] := arg

kroneckerProduct[args___] := KroneckerProduct[args]


OrderedMatrixRepresentation[matrix_ ? SquareMatrixQ, quditCount_Integer ? Positive, order_ ? orderQ] := Enclose @ Module[{
    dimension, arity,
    subsystems, passiveSubsystems, ordering,
    passiveMatrixRepresentations, matrixShape,
    newMatrixRepresentation, tensorProductLevels
},
    dimension = ConfirmBy[Length[matrix] ^ (1 / Length[order]), IntegerQ];

    If[ Length[order] == 1,

        kroneckerProduct @@ ReplacePart[Table[IdentityMatrix[dimension], quditCount], order -> matrix],

        arity = Length[order];
        subsystems = Range[quditCount];
        passiveSubsystems = Complement[subsystems, order];
        ordering = Join[order, passiveSubsystems];
        passiveMatrixRepresentations = Table[IdentityMatrix[dimension],  Length[passiveSubsystems]];
        matrixShape = ConstantArray[dimension, 2 quditCount];
        newMatrixRepresentation = kroneckerProduct @@ Join[{matrix}, passiveMatrixRepresentations];
        newMatrixRepresentation = ArrayReshape[newMatrixRepresentation, matrixShape];
        tensorProductLevels = InversePermutation[Ordering[ordering]];
        tensorProductLevels = Join[tensorProductLevels, tensorProductLevels + quditCount];
        newMatrixRepresentation = Transpose[newMatrixRepresentation, tensorProductLevels];
        ArrayReshape[
            Flatten[newMatrixRepresentation],
            {dimension ^ quditCount, dimension ^ quditCount}
        ]

    ]
]

