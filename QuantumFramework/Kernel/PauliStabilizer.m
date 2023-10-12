Package["Wolfram`QuantumFramework`"]

PackageExport[PauliStabilizer]



PauliTableauQ[t_] := ArrayQ[t, 3, MatchQ[0 | 1]] && MatchQ[Dimensions[t], {2, n_, m_}]
PauliStabilizerQ[PauliStabilizer[KeyValuePattern[{"Signs" -> signs : {(-1 | 1) ...}, "Tableau" -> tableau_ ? PauliTableauQ}] /; Length[signs] == Dimensions[tableau][[3]]]] := True
PauliStabilizerQ[_] := False

PauliStabilizer[s_QuantumState] := PauliStabilizer @ With[{n = s["Qudits"]},
	Transpose[
		Partition[#, 2] & /@ ReverseSort @ ResourceFunction["RowSpace"][
			With[{pauli = Tuples[Range[0, 3], n], v = s["Computational"]["StateVector"]},
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

PauliStabilizer[assoc_Association][prop_String] /; KeyExistsQ[assoc, prop] := assoc[prop]

ps_PauliStabilizer["Qudits"] := Dimensions[ps["Tableau"]][[2]]
ps_PauliStabilizer["Generators"] := Dimensions[ps["Tableau"]][[3]] / 2

ps_PauliStabilizer["HalfSigns"] := Drop[ps["Signs"], ps["Generators"]]
ps_PauliStabilizer["HalfTableau"] := Map[Drop[#, ps["Generators"]] &, ps["Tableau"], {2}]

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
ps_PauliStabilizer["CNOT", j_Integer, k_Integer] := With[{t = ps["Tableau"]},
	PauliStabilizer[<|
		"Signs" -> MapIndexed[If[t[[1, j, #2[[1]]]] == t[[2, k, #2[[1]]]] == 1 && t[[1, k, #2[[1]]]] == t[[2, j, #2[[1]]]], - #, #] &, ps["Signs"]],
		"Tableau" -> ReplacePart[t, {
			{1, k} -> MapThread[BitXor, Extract[t, {{1, j}, {1, k}}]],
			{2, j} -> MapThread[BitXor, Extract[t, {{2, j}, {2, k}}]]
		}]
	|>]
]


g[x1_, z1_, x2_, z2_] := Which[x1 == z1 == 0, 0, x1 == z1 == 1, z2 - x2, x1 == 1 && z1 == 0, z2 (2 x2 - 1), True, x2 (1 - 2 z2)]

rowsum[{r_, t_}, h_Integer, i_Integer] :=
	{
		ReplacePart[r, h -> 1 - 2 Boole[Mod[2 - r[[h]] - r[[i]] + Sum[g[t[[1, j, i]], t[[2, j, i]], t[[1, j, h]], t[[2, j, h]]], {j, 1, Dimensions[t][[2]]}], 4] == 2]],
		(tx |-> SubsetMap[BitXor[tx[[All, i]], tx[[All, h]]] &, tx, {All, h}]) /@ t
	}

ps_PauliStabilizer["RowSum", h_Integer, i_Integer] := PauliStabilizer[<|"Signs" -> #1, "Tableau" -> #2|> & @@ rowsum[{ps["Signs"], ps["Tableau"]}, h, i]]


ps_PauliStabilizer["Measure" | "M", a_Integer] := Block[{r = ps["Signs"], t = ps["Tableau"], n = ps["Generators"], pos},
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
];
ps_PauliStabilizer[op_String -> order_] := ps[op, Sequence @@ Flatten[{order}]]

ps_PauliStabilizer["State"] := QuantumState @ Normalize @ Total @ NullSpace[
	Total @ MapThread[(IdentityMatrix[Length[#2]] - #1 #2) &, {
		ps["HalfSigns"],
		kroneckerProduct @@@
			Map[PauliMatrix, Replace[Transpose[ps["HalfTableau"], {3, 2, 1}], {{0, 0} -> 0, {1, 0} -> 1, {1, 1} -> 2, {0, 1} -> 3}, {2}], {2}]
		}
	]
]

PauliForm[ps_ ? PauliStabilizerQ] := PauliForm[ps["HalfSigns"], ps["HalfTableau"]]
PauliForm[signs_, tableau_] := MapThread[
	StringJoin,
	{
		Replace[signs, {1 -> "", -1 -> "-"}, {1}],
		StringJoin @@@ Replace[Transpose[tableau, {3, 2, 1}], {{0, 0} -> "I", {1, 0} -> "X", {1, 1} -> "Y", {0, 1} -> "Z"}, {2}]
	}
]

TableauForm[ps_ ? PauliStabilizerQ] := TableauForm[ps["HalfSigns"], ps["HalfTableau"]]
TableauForm[signs_, tableau_, half_ : True] := Row[{Column[Replace[signs, {1 -> "", -1 -> "-"}, {1}]], MatrixForm[{{Grid[Transpose[Join @@ tableau], Dividers -> {{Dimensions[tableau][[2]] + 1 -> True}, If[half, False, {Length[signs] / 2 + 1 -> True}]}]}}]}]

MakeBoxes[ps_PauliStabilizer ? PauliStabilizerQ, form_] ^:= BoxForm`ArrangeSummaryBox["PauliStabilizer",
    ps,
    Framed["\[ScriptCapitalS]"],
    {{BoxForm`SummaryItem[{PauliForm[ps]}]}, {BoxForm`SummaryItem[{TableauForm[ps]}]}},
    {{BoxForm`SummaryItem[{TableauForm[ps["Signs"], ps["Tableau"], False]}]}},
    form
]

