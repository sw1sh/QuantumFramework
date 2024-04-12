Package["Wolfram`QuantumFramework`"]

PackageScope[BlockEncode]
PackageScope[PCPhase]
PackageScope[QSP2QSVTangles]
PackageScope[PauliDecompose]
PackageScope[QSVT]



BlockEncode[A_ ? MatrixQ] := With[{
	Adg = ConjugateTranspose[A]
},
	Join[
		Join[A, MatrixPower[IdentityMatrix[Dimensions[A][[1]]] - A . Adg, 1 / 2], 2],
		Join[MatrixPower[IdentityMatrix[Dimensions[A][[2]]] - Adg . A, 1 / 2], - Adg, 2]
	]
]

PCPhase[phi_, dim_Integer ? Positive, n_Integer ? Positive] :=
	DiagonalMatrix @ Join[
		ConstantArray[Exp[I phi], dim],
		ConstantArray[Exp[- I phi], Max[0, 2 ^ n - dim]]
	]

QSP2QSVTangles[angles_] := Block[{newAngles = angles},
	newAngles[[1]] += 3 Pi / 4;
	newAngles[[-1]] -= Pi / 4;
	newAngles[[2 ;; -2]] += Pi / 2;
	newAngles
]

PauliDecompose[qo_QuantumOperator] /; qo["OutputDimensions"] == qo["OutputDimensions"] && MatchQ[qo["OutputDimensions"], {2 ..}] := Block[{
	q = qo["InputQudits"], matrix, coeffs
},
	matrix = 1 / 2 ^ q ArrayReshape[kroneckerProduct @@@ Tuples[QuditBasis["Pauli"]["Elements"], q], 4 ^ q {1, 1}];
	coeffs = SparseArray[matrix . qo["StateVector"]];
	{Chop @ coeffs["ExplicitValues"], ArrayReshape[#, Sqrt[Times @@ Dimensions[#]] {1, 1}] & /@ Extract[Inverse @ Transpose @ matrix, coeffs["ExplicitPositions"]]}
]

PauliDecompose[mat_ ? SquareMatrixQ] := PauliDecompose[QuantumOperator[mat]]


QSVT[A_ ? MatrixQ, angles_ ? VectorQ, OptionsPattern[{"QSP" -> False}]] := Enclose @ Block[{\[CapitalPi], U, qspQ = TrueQ[OptionValue["QSP"]]},
	\[CapitalPi] = With[{n = ConfirmBy[Log2[2 Length[A]], IntegerQ]}, MapIndexed[QuantumOperator[PCPhase[#1, Dimensions[A][[Mod[#2[[1]], 2] + 1]], n], "Label" -> "\[Phi]"[#]] &, If[qspQ, QSP2QSVTangles[angles], angles]]];
	U = QuantumOperator[BlockEncode[A], "Label" -> "BlockEncode"];
	QuantumCircuitOperator[If[qspQ, Prepend["GlobalPhase"[Pi / 2 (4 - Mod[Length[angles] - 1, 4])]], Identity] @ Riffle[\[CapitalPi], {U, SuperDagger[U]}]]
]

