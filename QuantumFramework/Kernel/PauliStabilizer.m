Package["Wolfram`QuantumFramework`"]

PackageExport[PauliStabilizer]
PackageScope[PauliStabilizerApply]
PackageScope[$PauliStabilizerNames]



$PauliStabilizerNames = {"5QubitCode", "5QubitCode1", "9QubitCode", "9QubitCode1", "Random"}


PauliTableauQ[t_] := ArrayQ[t, 3, MatchQ[0 | 1]] && MatchQ[Dimensions[t], {2, n_, m_}]
PauliStabilizerQ[PauliStabilizer[KeyValuePattern[{"Signs" -> signs : {(-1 | 1) ...}, "Tableau" -> tableau_ ? PauliTableauQ}] /; Length[signs] == Dimensions[tableau][[3]]]] := True
PauliStabilizerQ[_] := False


_PauliStabilizer["Properties"] = {"Qubits", "Generators", "Matrix", "Phase", "TableauForm", "State", "Operator", "Circuit"}


(* constructors *)

PauliStabilizerTableau[v_ ? ArrayQ, n_Integer ? Positive] :=
	Transpose[
		Partition[#, 2] & /@ Take[#, n] & @ ReverseSort @ ResourceFunction["RowSpace"][
			With[{pauli = Tuples[Range[0, 3], n]},
				Map[
					(Conjugate[v] . (kroneckerProduct @@ PauliMatrix /@ #) . v) *
						Catenate[Replace[#, {0 -> {0, 0}, 1 -> {1, 0}, 2 -> {1, 1}, 3 -> {0, 1}}, {1}]] &,
					pauli
				]
			],
			"Basis"
		],
		{3, 2, 1}
	]

PauliStabilizer[qs_QuantumState] := PauliStabilizer @ PauliStabilizerTableau[qs["Computational"]["StateVector"], qs["Qudits"]]
(* PauliStabilizer[qs_QuantumState] := PauliStabilizer[QuantumOperator[Transpose @ qs["Eigenvectors"]]] *)

PauliStabilizer[data : KeyValuePattern[{"Phase" -> phase_}]] := PauliStabilizer[<|KeyDrop[data, "Phase"], "Signs" -> 1 - 2 phase|>]
PauliStabilizer[data : KeyValuePattern[{"Matrix" -> mat_}]] := PauliStabilizer[<|KeyDrop[data, "Matrix"], "Tableau" -> Transpose[ArrayReshape[mat, {Length[mat], 2, Length[mat] / 2}], {3, 1, 2}]|>]
PauliStabilizer[data : KeyValuePattern[{"Tableau" -> t_}]] /; ! KeyExistsQ[data, "Signs"] := PauliStabilizer[<|"Signs" -> ConstantArray[1, Dimensions[t][[3]]], "Tableau" -> t|>]

firstIndex[row_] := Replace[FirstPosition[Normal[row], x_ /; Abs[x] == 1, {1}, Heads -> False], {i_} :> i - 1]
bitvector[x_, n_] := Reverse @ IntegerDigits[x, 2, n]

PauliRow[mat_ ? MatrixQ, n_Integer ? Positive] := Enclose @ Block[{xint, xbits, zbits, entries, positivePhase, phase, coef},
	xint = Confirm @ firstIndex[mat[[1]]];
	xbits = bitvector[xint, n];
	entries = MapIndexed[
		With[{i = #2[[1]] - 1, index = Confirm @ firstIndex[#1]},
			ConfirmAssert[index == BitXor[xint, i] && MemberQ[{1, -1, I, -I}, #1[[index + 1]]]];
			#1[[index + 1]]
		] &,
		mat
	];

	zbits = Confirm @ Replace[entries[[2 ^ # + 1]] / entries[[1]], {1 -> 0, -1 -> 1, _ -> Missing[]}] & /@ Range[0, n - 1];
	positivePhase = (-I) ^ Inner[BitAnd, xbits, zbits];
	phase = Confirm @ Which[positivePhase == entries[[1]], 0, positivePhase == - entries[[1]], 1, True, Missing[]];
	coef = ((-1) ^ phase) positivePhase;
	ConfirmAssert[entries == Flatten[coef kroneckerProduct @@ Replace[Reverse[zbits], {1 -> {1, -1}, 0 -> {1, 1}}, {1}]]];

	(* Reverse bit convention *)
	Join[Reverse[xbits], Reverse[zbits], {phase}]
]

QuantumOperatorTableau[qo_QuantumOperator] /; qo["InputQudits"] == qo["OutputQudits"] := Enclose @ With[{n = qo["InputQudits"]},
	Join[
		Confirm @ PauliRow[(qo @ QuantumOperator["X" -> #] @ qo["Dagger"])["Matrix"], n] & /@ Sort[qo["InputOrder"]],
		Confirm @ PauliRow[(qo @ QuantumOperator["Z" -> #] @ qo["Dagger"])["Matrix"], n] & /@ Sort[qo["InputOrder"]]
	]
]
FromFullTableau[t_] := PauliStabilizer[<|"Signs" -> 1 - 2 t[[All, -1]], "Tableau" -> Transpose[ArrayReshape[t[[All, ;; -2]], {Length[t], 2, Length[t] / 2}], {3, 1, 2}]|>]


PauliStabilizer[qo_QuantumOperator, n : _Integer : 1] /; Sort[qo["OutputOrder"]] == Sort[qo["InputOrder"]] := With[{max = Max[n, qo["InputOrder"]]},
	Enclose @ FromFullTableau[
		Confirm @ QuantumOperatorTableau[qo["Sort"]["Computational"]]
	]["PadRight", max]["Permute", Join[Sort[qo["InputOrder"]], Complement[Range[max], qo["InputOrder"]]]]
]

PauliStabilizer[qco_QuantumCircuitOperator] := Fold[Collect[#1 /. ps_PauliStabilizer :> #2[ps], _PauliStabilizer, Simplify] &,
	PauliStabilizer[qco["Max"]],
	Replace[PauliStabilizer[#, qco["Max"]], _ ? FailureQ :> #] & /@ qco["NormalOperators"]
]

PauliStabilizer[basis_ /; ArrayQ[basis, 3, MatchQ[0 | 1 | -1]] && MatchQ[Dimensions[basis], {2, n_, m_}]] :=
	Enclose @ PauliStabilizer[Confirm @ Replace[Union @@ #, {{0, 1} | {1} -> 1, {-1, 0} | {-1} -> -1, _ -> $Failed}] & /@ Transpose[basis, {3, 2, 1}], Abs[basis]]
PauliStabilizer[tableau_ ? PauliTableauQ] := PauliStabilizer[1, tableau]
PauliStabilizer[sign : -1 | 1, tableau_ ? PauliTableauQ] := PauliStabilizer[{sign}, tableau]
PauliStabilizer[signs : {(-1 | 1) ...}, tableau_ ? PauliTableauQ] := With[{padSigns = PadRight[signs, Dimensions[tableau][[3]], 1]},
	PauliStabilizer[
		<|"Signs" -> Join[padSigns, padSigns], "Tableau" -> MapThread[Join[##, 2] &, {Reverse[tableau], tableau}]|>
	]
]

$PauliString = Repeated["-" | "+", {0, 1}] ~~ ("I" | "X" | "Y" | "Z") ...

PauliStabilizer[signedPauliStrings : {__String}] /; AllTrue[signedPauliStrings, StringMatchQ[$PauliString]] :=
	PauliStabilizer @@
		MapAt[Transpose[PadRight[#, Automatic, {0, 0}], {3, 2, 1}] &, 2] @ Thread @ Replace[
			Characters[signedPauliStrings], {sign :  "-" | "+" : 1, paulis___} :> {
				Replace[sign, {"-" -> -1, "+" -> 1}],
				Replace[{paulis}, {"I" -> {0, 0}, "X" -> {1, 0}, "Y" -> {1, 1}, "Z" -> {0, 1}}, {1}]
			},
			{1}
		]

PauliStabilizer[stabString : {__String}, destabStrings : {__String}] /; AllTrue[Join[stabString, destabStrings], StringMatchQ[$PauliString]] :=
	PauliStabilizer[<|"Signs" -> #1, "Tableau" -> #2|>] & @@
		MapAt[Transpose[PadRight[#, Automatic, {0, 0}], {3, 2, 1}] &, 2] @ Thread @ Replace[
			Characters[Join[destabStrings, stabString]], {sign : "-" | "+" : 1, paulis___} :> {
				Replace[sign, {"-" -> -1, "+" -> 1}],
				Replace[{paulis}, {"I" -> {0, 0}, "X" -> {1, 0}, "Y" -> {1, 1}, "Z" -> {0, 1}}, {1}]
			},
			{1}
		]

PauliStabilizer[q_Integer ? NonNegative] := PauliStabilizer[{ConstantArray[0, {Max[q, 1], q}], identityMatrix[q]}]

PauliStabilizer["5QubitCode"] := PauliStabilizer[{"XZZXI", "IXZZX", "XIXZZ", "ZXIXZ", "XXXXX"}]
PauliStabilizer["5QubitCode1"] := PauliStabilizer[{"XZZXI", "IXZZX", "XIXZZ", "ZXIXZ", "-XXXXX"}]

PauliStabilizer["9QubitCode"] := PauliStabilizer[{"ZZIIIIIII", "IZZIIIIII", "IIIZZIIII", "IIIIZZIII", "IIIIIIZZI", "IIIIIIIZZ", "XXXXXXIII", "IIIXXXXXX", "XXXXXXXXX"}]
PauliStabilizer["9QubitCode1"] := PauliStabilizer[{"ZZIIIIIII", "IZZIIIIII", "IIIZZIIII", "IIIIZZIII", "IIIIIIZZI", "IIIIIIIZZ", "XXXXXXIII", "IIIXXXXXX", "-XXXXXXXXX"}]

PauliStabilizer[] := PauliStabilizer[1]

PauliStabilizer[shortcut : _String | (_String -> _ ? orderQ | _Integer) | _List] := PauliStabilizer[QuantumCircuitOperator[shortcut]]


(* random *)

SampleMallows[n_] := Block[{h = ConstantArray[0, n], perm = ConstantArray[0, n], indices = Range[n]},
	Do[
		Block[{m = n - i, eps, r, index, k},
			eps = 4 ^ (- m);
			r = RandomReal[];
			index = - Ceiling[Log2[r + (1 - r) eps]];
			h[[i + 1]] = Boole[index < m];
			k = If[index < m, index, 2 m - index - 1];
			perm[[i + 1]] = indices[[k + 1]];
			indices = Drop[indices, {Mod[k, Length[indices]] + 1}]
		],
		{i, 0, n - 1}];
	{h, perm}
]

fillTril[n_, symmetric_ : False] := If[symmetric, SymmetrizedArray[# + UpperTriangularize[Transpose[#], 1]], LowerTriangularMatrix[# + IdentityMatrix[n]]] & @
	LowerTriangularize[RandomInteger[1, {n, n}], If[symmetric, 0, -1]]

RandomClifford[n_] := Block[{h, perm, gamma1, delta1, gamma2, delta2, zero, prod1, prod2, inv1, inv2, table1, table2, table, indices},
	{h, perm} = SampleMallows[n];
	gamma1 = fillTril[n, True];
	gamma2 = fillTril[n, True];
	delta1 = fillTril[n];
	delta2 = fillTril[n];
	zero = ConstantArray[0, {n, n}];
	prod1 = Mod[gamma1 . delta1, 2];
	prod2 = Mod[gamma2 . delta2, 2];
	inv1 = Transpose[Inverse[delta1]];
	inv2 = Transpose[Inverse[delta2]];
	table1 = Join[Join[delta1, zero, 2], Join[prod1, inv1, 2]];
	table2 = Join[Join[delta2, zero, 2], Join[prod2, inv2, 2]];
	table = table2[[Join[perm, perm + n]]];
	indices = Pick[Range[n], h, 1];
	table[[Join[indices, indices + n]]] = table[[Join[indices + n, indices]]];
	PauliStabilizer[<|"Matrix" -> Mod[table1 . table, 2], "Phase" -> RandomInteger[1, 2n]|>]
]

PauliStabilizer["Random", n : _Integer ? Positive : 5] := RandomClifford[n]


(* properties & methods *)

PauliStabilizer[assoc_Association][prop_String] /; KeyExistsQ[assoc, prop] := assoc[prop]

ps_PauliStabilizer["Qudits" | "Qubits"] := Dimensions[ps["Tableau"]][[2]]
ps_PauliStabilizer["GeneratorCount"] := Dimensions[ps["Tableau"]][[3]] / 2

ps_PauliStabilizer["StabilizerSigns"] := Drop[ps["Signs"], ps["GeneratorCount"]]
ps_PauliStabilizer["StabilizerTableau" | "Stabilizer"] := Map[Drop[#, ps["GeneratorCount"]] &, ps["Tableau"], {2}]
ps_PauliStabilizer["StabilizerX"] := ps["Stabilizer"][[1]]
ps_PauliStabilizer["StabilizerZ"] := ps["Stabilizer"][[2]]

ps_PauliStabilizer["DestabilizerSigns"] := Take[ps["Signs"], ps["GeneratorCount"]]
ps_PauliStabilizer["DestabilizerTableau" | "Destabilizer"] := Map[Take[#, ps["GeneratorCount"]] &, ps["Tableau"], {2}]
ps_PauliStabilizer["DestabilizerX"] := ps["Destabilizer"][[1]]
ps_PauliStabilizer["DestabilizerZ"] := ps["Destabilizer"][[2]]

ps_PauliStabilizer["Phase"] := (1 - ps["Signs"]) / 2
ps_PauliStabilizer["X"] := ps["Tableau"][[1]]
ps_PauliStabilizer["Z"] := ps["Tableau"][[2]]

ps_PauliStabilizer["Matrix"] := ArrayReshape[Transpose[ps["Tableau"], {2, 3, 1}], 2 {ps["GeneratorCount"], ps["Qubits"]}]
ps_PauliStabilizer["p"] := With[{n = ps["Qudits"]}, Diagonal[ps["Matrix"] . PadLeft[identityMatrix[n], {-2 n, 2 n}] . Transpose[ps["Matrix"]]]]

ps_PauliStabilizer["TableauPhase"] := Join[ps["Matrix"], ArrayReshape[ps["Phase"], {2 ps["GeneratorCount"], 1}], 2]

ps_PauliStabilizer["H", j_Integer] := With[{t = ps["Tableau"]},
	PauliStabilizer[<|
		"Signs" -> MapIndexed[If[t[[1, j, #2[[1]]]] == t[[2, j, #2[[1]]]] == 1, - #, #] &, ps["Signs"]],
		"Tableau" -> ReplacePart[t, Thread[{{1, j}, {2, j}} -> Extract[t, {{2, j}, {1, j}}]]]
	|>]
]
ps_PauliStabilizer["S", j_Integer] := With[{t = ps["Tableau"]},
	PauliStabilizer[<|
		"Signs" -> MapIndexed[If[t[[1, j, #2[[1]]]] == t[[2, j, #2[[1]]]] == 1, - #, #] &, ps["Signs"]],
		"Tableau" -> ReplacePart[t, {2, j} -> MapThread[BitXor, Extract[ps["Tableau"], {{1, j}, {2, j}}]]]
	|>]
]
ps_PauliStabilizer[SuperDagger["S"], j_Integer] := With[{t = ps["Tableau"]},
	PauliStabilizer[<|
		"Signs" -> MapIndexed[If[t[[1, j, #2[[1]]]] == 1 - t[[2, j, #2[[1]]]] == 1, - #, #] &, ps["Signs"]],
		"Tableau" -> ReplacePart[t, {2, j} -> MapThread[BitXor, Extract[ps["Tableau"], {{1, j}, {2, j}}]]]
	|>]
]
ps_PauliStabilizer["CNOT" | "CX", j_Integer, k_Integer] := With[{t = ps["Tableau"]},
	PauliStabilizer[<|
		"Signs" -> MapIndexed[If[t[[1, j, #2[[1]]]] == t[[2, k, #2[[1]]]] == 1 && t[[1, k, #2[[1]]]] == t[[2, j, #2[[1]]]], - #, #] &, ps["Signs"]],
		"Tableau" -> ReplacePart[t, {
			{1, k} -> MapThread[BitXor, Extract[t, {{1, j}, {1, k}}]],
			{2, j} -> MapThread[BitXor, Extract[t, {{2, j}, {2, k}}]]
		}]
	|>]
]
ps_PauliStabilizer["X", j_Integer] := PauliStabilizer[<|"Phase" -> BitXor[ps["Phase"], ps["Z"][[j]]], "Tableau" -> ps["Tableau"]|>]
ps_PauliStabilizer["Y", j_Integer] := PauliStabilizer[<|"Phase" -> BitXor[ps["Phase"], ps["X"][[j]], ps["Z"][[j]]], "Tableau" -> ps["Tableau"]|>]
ps_PauliStabilizer["Z", j_Integer] := PauliStabilizer[<|"Phase" -> BitXor[ps["Phase"], ps["X"][[j]]], "Tableau" -> ps["Tableau"]|>]
ps_PauliStabilizer["CZ", j_Integer, k_Integer] := ps["H", k]["CNOT", j, k]["H", k]
ps_PauliStabilizer["SWAP", j_Integer, k_Integer] := ps["PermuteQudits", Cycles[{{j, k}}]]

ps_PauliStabilizer["Permute", perm_] := With[{n = ps["Qudits"]},
	PauliStabilizer[<|
		"Signs" -> Catenate[Permute[#, perm] & /@ Partition[ps["Signs"], n]],
		"Tableau" -> Map[Map[Catenate[Permute[#, perm] & /@ Partition[#, n]] &, Permute[#, perm]] &, ps["Tableau"]]
	|>]
]

ps_PauliStabilizer["PermuteQudits", perm_] := With[{n = ps["Qudits"]},
	PauliStabilizer[<|
		"Signs" -> ps["Signs"],
		"Tableau" -> Map[Permute[#, perm] &, ps["Tableau"]]
	|>]
]

ps_PauliStabilizer["Dagger" | "Inverse"] := Block[{mat},
	PauliStabilizer @ <|
		"Phase" -> BitXor[
			ps["Phase"],
			ps[PauliStabilizer[<|
				"Matrix" -> (mat = Inverse[ps["Matrix"], Modulus -> 2]),
				"Signs" -> ps["Signs"]
			|>]]["Phase"]
		],
		"Matrix" -> mat
	|>
]

ps_PauliStabilizer["PadRight", n_] := QuantumTensorProduct[ps, PauliStabilizer[Max[n - ps["Qubits"], 0]]]
ps_PauliStabilizer["PadLeft", n_] := QuantumTensorProduct[PauliStabilizer[Max[n - ps["Qubits"], 0]], ps]


g[x1_, z1_, x2_, z2_] := Which[x1 == z1 == 0, 0, x1 == z1 == 1, z2 - x2, x1 == 1 && z1 == 0, z2 (2 x2 - 1), True, x2 (1 - 2 z2)]

rowsum[{r_, t_}, h_Integer, i_Integer] :=
	{
		ReplacePart[r, h -> 1 - 2 Boole[Mod[2 - r[[h]] - r[[i]] + Sum[g[t[[1, j, i]], t[[2, j, i]], t[[1, j, h]], t[[2, j, h]]], {j, 1, Dimensions[t][[2]]}], 4] == 2]],
		(tx |-> SubsetMap[BitXor[tx[[All, i]], tx[[All, h]]] &, tx, {All, h}]) /@ t
	}

ps_PauliStabilizer["RowSum", h_Integer, i_Integer] := PauliStabilizer[<|"Signs" -> #1, "Tableau" -> #2|> & @@ rowsum[{ps["Signs"], ps["Tableau"]}, h, i]]


ps_PauliStabilizer["Measure" | "M", a_Integer] := Enclose @ Block[{r = ps["Signs"], t = ps["Tableau"], n = ps["GeneratorCount"], pos},
	ConfirmAssert[1 <= a <= n];
	pos = Lookup[PositionIndex[t[[1, a]]], 1, {}];
	Association @ With[{p = SelectFirst[pos, GreaterThan[n]]},
		If[ MissingQ[p],
			(* deterministic *)
			{(1 - Last[#1]) / 2 -> PauliStabilizer[<|"Signs" -> Most[#1], "Tableau" -> t|>]} & @@
				Fold[rowsum[#1, 2 n + 1, #2 + n] &, {Append[r, 1], PadRight[t, Dimensions[t] + {0, 0, 1}]}, Select[pos, LessEqualThan[n]]],

			(* non-deterministic *)
			Block[{r2, t2},
				{r2, t2} = Fold[rowsum[#1, #2, p] &, {r, t}, Select[pos, GreaterThan[p]]];
				t2[[All, All, p - n]] = t2[[All, All, p]];
				t2[[All, All, p]] = 0;
				t2[[2, a, p]] = 1;
				# -> PauliStabilizer[<|
					"Signs" -> ReplacePart[r2, p -> 1 - 2 #],
					"Tableau" -> t2
				|>] & /@ {0, 1}
			]
		]
	]
]
ps_PauliStabilizer["Measure" | "M", qudits : {___Integer}] := Enclose @ If[qudits === {}, <|{} -> ps|>,
	Join @@ KeyValueMap[{k, v} |-> KeyMap[Prepend[k]] @ Confirm @ v["M", Rest[qudits]], Confirm @ ps["M", First[qudits]]]
]

ps_PauliStabilizer["V", j_Integer] := PauliStabilizer[{"-Y"}, {"X"}]["PadLeft", j] @ ps
ps_PauliStabilizer[SuperDagger["V"], j_Integer] := PauliStabilizer[{"Y"}, {"X"}]["PadLeft", j] @ ps

ps_PauliStabilizer["P"[phase_], j_Integer] := With[{c = Exp[I phase / 2]},
	(1 + c) / 2 ps + (1 - c) / 2 ps["Z", j]
]
ps_PauliStabilizer["T", j_Integer] := ps["P"[Pi / 2], j]
ps_PauliStabilizer[SuperDagger["T"], j_Integer] := ps["P"[- Pi / 2], j]

ps_PauliStabilizer[op_ -> order_] := ps[op, Sequence @@ Flatten[{order}]]

ps_PauliStabilizer["Measure" | "M", qudits___Integer] := ps["M", {qudits}]
ps_PauliStabilizer[qudits__Integer] := ps["M", {qudits}]
ps_PauliStabilizer[qudits : {___Integer}] := ps["M", qudits]
ps_PauliStabilizer[] := ps["M", Range[ps["Qudits"]]]


ps_PauliStabilizer["State" | "QuantumSttate"] := QuantumState @ Normalize @ Total @ NullSpace[
	Total @ MapThread[(IdentityMatrix[Length[#2]] - #1 #2) &, {
		ps["StabilizerSigns"],
		kroneckerProduct @@@
			Map[PauliMatrix, Replace[Transpose[ps["StabilizerTableau"], {3, 2, 1}], {{0, 0} -> 0, {1, 0} -> 1, {1, 1} -> 2, {0, 1} -> 3}, {2}], {2}]
		}
	]
]

ps_PauliStabilizer["Circuit" | "QuantumCircuit" | "QuantumCircuitOperator"] := Block[{
	clifford = ps, n = ps["Qudits"], gates = {},
	destabX, destabZ, stabX, stabZ, destabP, stabP,
	append, setQubitX1, setRowX0, setRowZ0
},
	destabX[q_] := Thread[clifford["DestabilizerX"][[All, q]] == 1];
	destabZ[q_] := Thread[clifford["DestabilizerZ"][[All, q]] == 1];
	stabX[q_] := Thread[clifford["StabilizerX"][[All, q]] == 1];
	stabZ[q_] := Thread[clifford["StabilizerZ"][[All, q]] == 1];
	destabP[q_] := clifford["Phase"][[q]] == 1;
	stabP[q_] := clifford["Phase"][[n + q]] == 1;
	append[gate_] := (clifford = clifford[gate]; AppendTo[gates, gate]);
	setQubitX1[q_] := Catch @ With[{x := destabX[q], z := destabZ[q]},
		If[x[[q]], Throw[0]];
		Do[If[x[[i]], append["SWAP" -> {i, q}]; Throw[0]], {i, q + 1, n}];
		Do[If[z[[i]], append["H" -> i]; If[i != q, append["SWAP" -> {i, q}]]; Throw[0]], {i, q, n}]
	];
	setRowX0[q_] := With[{x := destabX[q], z := destabZ[q]},
		Do[If[x[[i]], append["CNOT" -> {q, i}]], {i, q + 1, n}];
		If[Or @@ z[[q ;;]], If[! z[[q]], append["S" -> q]]; Do[If[z[[i]], append["CNOT" -> {i, q}]], {i, q + 1, n}]; append["S" -> q]]
	];
	setRowZ0[q_] := With[{x := stabX[q], z := stabZ[q]},
		If[Or @@ z[[q + 1 ;;]], Do[If[z[[i]], append["CNOT" -> {i, q}]], {i, q + 1, n}]];
		If[Or @@ x[[q ;;]], append["H" -> q]; Do[If[x[[i]], append["CNOT" -> {q, i}]], {i, q + 1, n}]; If[z[[q]], append["S" -> q]]; append["H" -> q]]
	];
	Do[setQubitX1[q]; setRowX0[q]; setRowZ0[q], {q, n}];
	Do[If[destabP[q], append["Z" -> q]]; If[stabP[q], append["X" -> q]], {q, n}];
	gates = FixedPoint[SequenceReplace[{g_, g_} -> Nothing], gates];
	QuantumCircuitOperator[gates]["Dagger"]
]

ps_PauliStabilizer["Operator" | "QuantumOperator"] := QuantumOperator["I", Range[ps["Qudits"]]] @ ps["Circuit"]["QuantumOperator"]


(* quantum composition *)

phaseLookup := phaseLookup = SparseArray[
	Join[
		Thread[{{1, 2, 2, 1}, {2, 1, 2, 2}, {2, 2, 1, 2}} -> -1],
		Thread[{{1, 2, 2, 2}, {2, 1, 1, 2}, {2, 2, 2, 1}} -> 1]
	],
	{2, 2, 2, 2}
]


left_PauliStabilizer[right_PauliStabilizer] := Enclose @ Block[{n = Max[left["Qudits"], right["Qudits"]], first, second, ifacts, x1, z1, p},
	second = right["PadRight", n];
	first = left["PadRight", n];
	{x1, z1} = Transpose /@ first["Tableau"];
	ifacts = Total[BitAnd[second["X"], second["Z"]]] + Map[
		With[{x1s = Pick[x1, #, 1], z1s = Pick[z1, #, 1]},
			Total @ Extract[phaseLookup,
				Thread[Flatten[#] + 1 & /@ {Rest[x1s], Rest[z1s], Most @ FoldList[BitXor, x1s], Most @ FoldList[BitXor, z1s]}]
			]
		] &,
		second["Matrix"]
	];
	p = Quotient[Mod[ifacts, 4], 2];
	PauliStabilizer[<|
		"Phase" -> Mod[second["Matrix"] . first["Phase"] + second["Phase"] + p, 2],
		"Matrix" -> Mod[second["Matrix"] . first["Matrix"], 2]
	|>]
]

QuantumTensorProduct[a_PauliStabilizer, b_PauliStabilizer] :=
	PauliStabilizer[<|
		"Signs" -> Join[a["DestabilizerSigns"], b["DestabilizerSigns"], a["StabilizerSigns"], b["StabilizerSigns"]],
		"Tableau" -> {
			Join[
				blockDiagonalMatrix[{a["DestabilizerX"], b["DestabilizerX"]}],
				blockDiagonalMatrix[{a["StabilizerX"], b["StabilizerX"]}],
				2
			],
			Join[
				blockDiagonalMatrix[{a["DestabilizerZ"], b["DestabilizerZ"]}],
				blockDiagonalMatrix[{a["StabilizerZ"], b["StabilizerZ"]}],
				2
			]
		}
	|>
	]

PauliStabilizerApply[qco_QuantumCircuitOperator, qs : _QuantumState | _PauliStabilizer : Automatic] := Fold[
	#1[Replace[#2, {"C", gate : "NOT" | "X" | "Z" -> t_, c_, _} :> "C" <> gate -> Join[c, t]]] &,
	Replace[qs, {Automatic :> PauliStabilizer[qco["Arity"]], s_QuantumState :> PauliStabilizer[s]}],
	QuantumShortcut[qco]
]

qo_QuantumOperator[ps_PauliStabilizer] ^:= PauliStabilizerApply[QuantumCircuitOperator[qo], ps]
qmo_QuantumMeasurementOperator[ps_PauliStabilizer] ^:= PauliStabilizerApply[QuantumCircuitOperator[qmo], ps]
QuantumState[ps_PauliStabilizer] ^:= ps["State"]


(* forms *)

PauliForm[ps_ ? PauliStabilizerQ, n : _Integer ? Positive | Infinity : Infinity, stab_ : True] := If[stab,
	PauliForm[Take[ps["StabilizerSigns"], UpTo[n]], Map[Take[#, UpTo[n]] &, ps["StabilizerTableau"], {2}]],
	PauliForm[Take[ps["DestabilizerSigns"], UpTo[n]], Map[Take[#, UpTo[n]] &, ps["DestabilizerTableau"], {2}]]
]
PauliForm[signs_, tableau_] := If[MatchQ[Dimensions[tableau], {2, n_, m_} /; 0 < m < n], Append["\[Ellipsis]"], Identity] @ MapThread[
	StringJoin,
	{
		Replace[signs, {1 -> "", -1 -> "-"}, {1}],
		StringJoin @@@ Replace[Transpose[tableau, {3, 2, 1}], {{0, 0} -> "I", {1, 0} -> "X", {1, 1} -> "Y", {0, 1} -> "Z"}, {2}]
	}
]

TableauForm[ps_ ? PauliStabilizerQ, n : _Integer ? Positive | Infinity : Infinity] := TableauForm[Take[ps["StabilizerSigns"], UpTo[n]], Map[Take[#, UpTo[n]] &, ps["StabilizerTableau"], {2}]]

TableauForm[signs_, tableau_, stab_ : True] := With[{
	short = stab && MatchQ[Dimensions[tableau], {2, n_, m_} /; 0 < m < n],
	r = PadRight[signs, Max[2, Length[signs]], 1],
	t = PadRight[tableau, MapAt[Max[#, If[stab, 1, 2]] &, Dimensions[tableau], -1], " "]
},
	Row[{
		Column[Replace[r, {1 -> "", -1 -> "-"}, {1}]],
		MatrixForm[{{
			Grid[Transpose[Join @@ t], Dividers -> {{Dimensions[t][[2]] + 1 -> True}, If[stab, False, {Length[r] / 2 + 1 -> True}]}]
		}, 	If[short, {"\[VerticalEllipsis]"}, Nothing]}]
	}]
]


ps_PauliStabilizer["PauliForm" | "Generators" | "Stabilizers", n_ : Infinity] := PauliForm[ps, n]
ps_PauliStabilizer["Destabilizers", n_ : Infinity] := PauliForm[ps, n, False]
ps_PauliStabilizer["PauliStrings", n_ : Infinity] := Join[ps["Stabilizers", n], ps["Destabilizers", n]]
ps_PauliStabilizer["PauliSymbols", n_ : Infinity] := If[StringStartsQ[#, "-"], - StringDrop[#, 1], #] & /@ Join[ps["Stabilizers", n], ps["Destabilizers", n]]

ps_PauliStabilizer["StabilizerTableauForm", n_ : Infinity] := TableauForm[ps, n]
ps_PauliStabilizer["TableauForm", n_ : Infinity] := TableauForm[Take[ps["Signs"], UpTo[n]], Map[Take[#, UpTo[n]] &, ps["Tableau"], {2}], False]


(* formatting *)

MakeBoxes[ps_PauliStabilizer ? PauliStabilizerQ, TraditionalForm] ^:= With[{boxes = ToBoxes[ps["State"], TraditionalForm]},
	InterpretationBox[boxes, ps]
]

MakeBoxes[ps_PauliStabilizer ? PauliStabilizerQ, form_] ^:= With[{n = ps["GeneratorCount"]},
	BoxForm`ArrangeSummaryBox["PauliStabilizer",
		ps,
		Framed["\[ScriptCapitalS]"],
		If[n < 32, {{BoxForm`SummaryItem[{PauliForm[ps, 5]}]}, If[n <= 5, {BoxForm`SummaryItem[{TableauForm[ps, 5]}]}, Nothing]}, {{BoxForm`SummaryItem[{"Qubits: ", ps["Qudits"]}]}, {BoxForm`SummaryItem[{"Generators: ", n}]}}],
		If[n < 32, {{BoxForm`SummaryItem[{"Destabilizers: ", PauliForm[ps, 5, False]}]}, {BoxForm`SummaryItem[{"Tableau: ", ps["TableauForm"]}]}}, {{}}],
		form
	]
]

