Package["QuantumFramework`"]

PackageScope["$BasisNameZero"]
PackageScope["$BasisNameIdentity"]
PackageScope["basisElementNameLength"]
PackageScope["simplifyBasisElementName"]
PackageScope["normalBasisElementName"]
PackageScope["prettyBasisElementName"]
PackageScope["basisElementQudits"]
PackageScope["basisElementNamesDimensions"]
PackageScope["symbolicTensorQ"]
PackageScope["basisMultiplicity"]
PackageScope["nameQ"]
PackageScope["propQ"]
PackageScope["stateQ"]
PackageScope["orderQ"]
PackageScope["measurementReprQ"]
PackageScope["normalizeMatrix"]
PackageScope["tensorToVector"]
PackageScope["identityMatrix"]
PackageScope["kroneckerProduct"]
PackageScope["OrderedMatrixRepresentation"]



(* basis names *)

$BasisNameZero = CircleTimes[]

$BasisNameIdentity = \[FormalCapitalI]

BasisNameHeadQ = MatchQ[_TensorProduct | _CircleTimes | _List];


basisElementNameLength[name_ ? BasisNameHeadQ] := Length @ name

basisElementNameLength[_] := 1


simplifyBasisElementName[name_] := With[{
    noIdentities = DeleteCases[name, $BasisNameIdentity]
},
    If[ basisElementNameLength[noIdentities] == 1 && BasisNameHeadQ[noIdentities],
        First[noIdentities, $BasisNameIdentity],
        noIdentities
    ]
]


normalBasisElementName[name_ ? BasisNameHeadQ] := List @@ name

normalBasisElementName[name_] := {name}


prettyBasisElementName[name_] := simplifyBasisElementName[CircleTimes @@ normalBasisElementName[name]]


basisElementQudits[name_] := basisElementNameLength @ DeleteCases[$BasisNameIdentity] @ normalBasisElementName @ name


basisElementNamesDimensions[names_] := CountDistinct /@ Transpose[normalBasisElementName /@ names]


(* *)

symbolicTensorQ[a_] := MatchQ[a, _Symbol] || TensorQ[a] && AnyTrue[Level[a, {-1}], MatchQ[_Symbol]]


basisMultiplicity[dim_, size_] := Quiet[Ceiling @ Log[size, dim] /. 0 | Indeterminate -> 1, Divide::indet]


(* test functions *)

nameQ[name_] := MatchQ[name, _String | {_String, ___}]

propQ[prop_] := MatchQ[prop, _String | {_String, ___} | (_String -> _)]

stateQ[state_] := !nameQ[state] && VectorQ[state] && Length[state] > 0 || SquareMatrixQ[state]

orderQ[order_] := VectorQ[order, IntegerQ]

measurementReprQ[state_] := TensorQ[state] && MemberQ[{2, 3}, TensorRank[state]]


(* Matrix tools *)

tensorToVector[t_ ? TensorQ] := Flatten[t]

(* scalar *)
tensorToVector[t_] := {t}


identityMatrix[0] := {{}}

identityMatrix[n_] := IdentityMatrix[n]


normalizeMatrix[matrix_] := matrix / Tr[matrix] (*SparseArray[{i_, i_} -> 1 / Tr[matrix], Dimensions[matrix], 1]*)


kroneckerProduct[ts___] := Fold[If[ArrayQ[#1] && ArrayQ[#2], KroneckerProduct[##], Times[##]] &, {ts}]


OrderedMatrixRepresentation[matrix_ ? MatrixQ, quditCount_Integer ? Positive, order_ ? orderQ] := Enclose @ Module[{
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
        passiveMatrixRepresentations = Table[IdentityMatrix[dimension], Length[passiveSubsystems]];
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

