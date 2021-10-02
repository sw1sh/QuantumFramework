Package["QuantumFramework`"]

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
PackageScope["projector"]

PackageScope["toggleSwap"]
PackageScope["toggleShift"]




(* *)

symbolicTensorQ[a_] := MatchQ[a, _Symbol] || TensorQ[a] && AnyTrue[Level[a, {-1}], MatchQ[_Symbol]]


basisMultiplicity[dim_, size_] := Quiet[Ceiling @ Log[size, dim] /. 0 | Indeterminate -> 1, Divide::indet]


(* test functions *)

nameQ[name_] := MatchQ[name, _String | {_String, ___}]

propQ[prop_] := MatchQ[prop, _String | {_String, ___} | (_String -> _)]

stateQ[state_] := !nameQ[state] && VectorQ[state] && Length[state] > 0 || SquareMatrixQ[state]

orderQ[order_] := VectorQ[order, IntegerQ] && DuplicateFreeQ[order]

measurementReprQ[state_] := TensorQ[state] && MemberQ[{2, 3}, TensorRank[state]]


(* Matrix tools *)

tensorToVector[t_ ? TensorQ] := Flatten[t]

(* scalar *)
tensorToVector[t_] := {t}


identityMatrix[0] := {{}}

identityMatrix[n_] := IdentityMatrix[n]


normalizeMatrix[matrix_] := Enclose[ConfirmQuiet[matrix / Tr[matrix], Power::infy]] (*SparseArray[{i_, i_} -> 1 / Tr[matrix], Dimensions[matrix], 1]*)


kroneckerProduct[ts___] := Fold[If[ArrayQ[#1] && ArrayQ[#2], KroneckerProduct[##], Times[##]] &, {ts}]


projector[v_] := KroneckerProduct[v, Conjugate[v]]


(* helpers *)

toggleSwap[xs : {_Integer...}, n_Integer] := MapIndexed[(#1 > n) != (First[#2] > n) &, xs]

toggleShift[xs : {_Integer...}, n_Integer] := n - Subtract @@ Total /@ TakeDrop[Boole @ toggleSwap[xs, n], n]

