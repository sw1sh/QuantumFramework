Package["Wolfram`QuantumFramework`"]

PackageExport[PauliStabilizer]
PackageScope[PauliStabilizerApply]
PackageScope[$PauliStabilizerNames]



$PauliStabilizerNames = {"5QubitCode", "5QubitCode1", "9QubitCode", "9QubitCode1"}


PauliTableauQ[t_] := ArrayQ[t, 3, MatchQ[0 | 1]] && MatchQ[Dimensions[t], {2, n_, m_}]
PauliStabilizerQ[PauliStabilizer[KeyValuePattern[{"Signs" -> signs : {(-1 | 1) ...}, "Tableau" -> tableau_ ? PauliTableauQ}] /; Length[signs] == Dimensions[tableau][[3]]]] := True
PauliStabilizerQ[_] := False


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
		Confirm @ PauliRow[(qo @ QuantumOperator["X" -> #] @ qo["Dagger"])["Matrix"], n] & /@ Reverse[qo["InputOrder"]],
		Confirm @ PauliRow[(qo @ QuantumOperator["Z" -> #] @ qo["Dagger"])["Matrix"], n] & /@ Reverse[qo["InputOrder"]]
	]
]
FromFullTableau[t_] := PauliStabilizer[<|"Signs" -> 1 - 2 t[[All, -1]], "Tableau" -> Transpose[ArrayReshape[t[[All, ;; -2]], {Length[t], 2, Length[t] / 2}], {3, 1, 2}]|>]


PauliStabilizer[qo_QuantumOperator, n : _Integer : 1] /; qo["OutputOrder"] == qo["InputOrder"] := With[{max = Max[n, qo["InputOrder"]]},
	Enclose @ FromFullTableau[
		Confirm @ QuantumOperatorTableau[qo["Sort"]["Computational"]]
	]["PadRight", max]["Permute", Join[Sort[qo["InputOrder"]], Complement[Range[max], qo["InputOrder"]]]]
]

PauliStabilizer[qco_QuantumCircuitOperator] := Fold[ReverseApplied[Construct], PauliStabilizer[#, qco["Max"]] & /@ qco["NormalOperators"]]

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

ps_PauliStabilizer["H", j_Integer] := With[{t = ps["Tableau"]},
	PauliStabilizer[<|
		"Signs" -> MapIndexed[If[t[[1, j, #2[[1]]]] == t[[2, j, #2[[1]]]] == 1, - #, #] &, ps["Signs"]],
		"Tableau" -> ReplacePart[t, Thread[{{1, j}, {2, j}} -> Extract[t, {{2, j}, {1, j}}]]]
	|>]
]
ps_PauliStabilizer["P" | "S", j_Integer] := With[{t = ps["Tableau"]},
	PauliStabilizer[<|
		"Signs" -> MapIndexed[If[t[[1, j, #2[[1]]]] == t[[2, j, #2[[1]]]] == 1, - #, #] &, ps["Signs"]],
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

ps_PauliStabilizer["Permute", perm_] := PauliStabilizer[<|
	"Signs" -> ps["Signs"],
	"Tableau" -> (Permute[#, perm] & /@ ps["Tableau"])
|>]

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

ps_PauliStabilizer[op_String -> order_] := ps[op, Sequence @@ Flatten[{order}]]

ps_PauliStabilizer["Measure" | "M", qudits___Integer] := ps["M", {qudits}]
ps_PauliStabilizer[qudits__Integer] := ps["M", {qudits}]
ps_PauliStabilizer[qudits : {___Integer}] := ps["M", qudits]
ps_PauliStabilizer[] := ps["M", Range[ps["Qudits"]]]


ps_PauliStabilizer["State"] := QuantumState @ Normalize @ Total @ NullSpace[
	Total @ MapThread[(IdentityMatrix[Length[#2]] - #1 #2) &, {
		ps["StabilizerSigns"],
		kroneckerProduct @@@
			Map[PauliMatrix, Replace[Transpose[ps["StabilizerTableau"], {3, 2, 1}], {{0, 0} -> 0, {1, 0} -> 1, {1, 1} -> 2, {0, 1} -> 3}, {2}], {2}]
		}
	]
]


(* quantum composition *)

phaseLookup := phaseLookup = SparseArray[
	Join[
		Thread[{{1, 2, 2, 1}, {2, 1, 2, 2}, {2, 2, 1, 2}} -> -1],
		Thread[{{1, 2, 2, 2}, {2, 1, 1, 2}, {2, 2, 2, 1}} -> 1]
	],
	{2, 2, 2, 2}
]

(*
		ifacts = np.sum(second.x & second.z, axis=1, dtype=int)

        x1, z1 = first.x.astype(np.uint8), first.z.astype(np.uint8)
        lookup = cls._compose_lookup()

        # The loop is over 2*n_qubits entries, and the entire loop is cubic in the number of qubits.
        for k, row2 in enumerate(second.symplectic_matrix):
            x1_select = x1[row2]
            z1_select = z1[row2]
            x1_accum = np.logical_xor.accumulate(x1_select, axis=0).astype(np.uint8)
            z1_accum = np.logical_xor.accumulate(z1_select, axis=0).astype(np.uint8)
            indexer = (x1_select[1:], z1_select[1:], x1_accum[:-1], z1_accum[:-1])
            ifacts[k] += np.sum(lookup[indexer])
        p = np.mod(ifacts, 4) // 2

        phase = (
            (np.matmul(second.symplectic_matrix, first.phase, dtype=int) + second.phase + p) % 2
        ).astype(bool)
*)

left_PauliStabilizer[right_PauliStabilizer] := Enclose @ Block[{n = Max[left["Qudits"], right["Qudits"]], first, second, ifacts, x1, z1, p},
	second = left["PadRight", n];
	first = right["PadRight", n];
	{x1, z1} = Transpose /@ first["Tableau"];
	ifacts = Total[BitAnd[second["X"], second["Z"]]] + Map[
		With[{x1s = Pick[x1, #, 1], z1s = Pick[z1, #, 1]},
			Total @ Extract[phaseLookup,
				Join @@@ Tuples[Position[Flatten[#], 1, {1}, Heads -> False] & /@ {Rest[x1s], Rest[z1s], Most @ FoldList[BitXor, x1s], Most @ FoldList[BitXor, z1s]}]
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

ps_PauliStabilizer["SymplecticMatrix"] := With[{n = ps["Qudits"]},
	ps["Matrix"] . Reverse[BlockDiagonalMatrix[{- IdentityMatrix[n], IdentityMatrix[n]}]]
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
	#1[Replace[#2, {"C", "NOT" | "X" -> t_, c_, _} :> "CNOT" -> Join[c, t]]] &,
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
ps_PauliStabilizer["Destabilizers", n_ : Infinity] := PauliForm[ps, n, ]
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

