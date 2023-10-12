Package["Wolfram`QuantumFramework`"]

PackageExport[PauliStabilizer]



PauliTableauQ[t_] := ArrayQ[t, 3, MatchQ[0 | 1]] && MatchQ[Dimensions[t], {2, n_, m_}]
PauliStabilizerQ[PauliStabilizer[KeyValuePattern[{"Signs" -> signs : {(-1 | 1) ...}, "Tableau" -> tableau_ ? PauliTableauQ}] /; Length[signs] == Dimensions[tableau][[3]]]] := True
PauliStabilizerQ[_] := False

PauliStabilizer[\[Psi]_QuantumState] := With[{n = \[Psi]["Qudits"]},
	PauliStabilizer @ Transpose[
		Partition[#, 2] & /@ ResourceFunction["RowSpace"][
			With[{pauli = Tuples[Range[0, 3], n], v = \[Psi]["StateVector"]},
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
PauliStabilizer[basis_ /; ArrayQ[basis, 3, MatchQ[0 | 1 | -1]] && MatchQ[Dimensions[basis], {2, n_, m_}]] := PauliStabilizer[1 - 2 Mod[Count[#, -1, {2}], 2] & /@ Transpose[basis, {3, 2, 1}], Abs[basis]]
PauliStabilizer[tableau_ ? PauliTableauQ] := PauliStabilizer[1, tableau]
PauliStabilizer[sign : -1 | 1, tableau_ ? PauliTableauQ] := PauliStabilizer[{1}, tableau]
PauliStabilizer[signs : {(-1 | 1) ...}, tableau_ ? PauliTableauQ] := PauliStabilizer[<|"Signs" -> PadRight[signs, Dimensions[tableau][[3]], 1], "Tableau" -> tableau|>]

PauliStabilizer[assoc_Association][prop_String] /; KeyExistsQ[assoc, prop] := assoc[prop]

ps_PauliStabilizer["H", j_Integer] := With[{t = ps["Tableau"]},
	PauliStabilizer[
		MapIndexed[If[t[[1, j, #2[[1]]]] == t[[2, j, #2[[1]]]] == 1, - #, #] &, ps["Signs"]],
		ReplacePart[ps["Tableau"], Thread[{{1, j}, {2, j}} -> Extract[ps["Tableau"], {{2, j}, {1, j}}]]]
	]
]
ps_PauliStabilizer["P" | "S", j_Integer] := With[{t = ps["Tableau"]},
	PauliStabilizer[
		MapIndexed[If[t[[1, j, #2[[1]]]] == t[[2, j, #2[[1]]]] == 1, - #, #] &, ps["Signs"]],
		ReplacePart[t, {2, j} -> MapThread[BitXor, Extract[ps["Tableau"], {{1, j}, {2, j}}]]]
	]
]
ps_PauliStabilizer["CNOT", j_Integer, k_Integer] := With[{t = ps["Tableau"]},
	PauliStabilizer[
		MapIndexed[If[t[[1, j, #2[[1]]]] == t[[2, k, #2[[1]]]] == 1 && t[[1, k, #2[[1]]]] == t[[2, j, #2[[1]]]], - #, #] &, ps["Signs"]],
		ReplacePart[t, {
			{1, k} -> MapThread[BitXor, Extract[t, {{1, j}, {1, k}}]],
			{2, j} -> MapThread[BitXor, Extract[t, {{2, j}, {2, k}}]]
		}]
	]
]

ps_PauliStabilizer["State"] := QuantumState @ Normalize @ Total @ NullSpace[
	Total @ MapThread[(IdentityMatrix[Length[#2]] - #1 #2) &, {
		ps["Signs"],
		kroneckerProduct @@@
			Map[PauliMatrix, Replace[Transpose[ps["Tableau"], {3, 2, 1}], {{0, 0} -> 0, {1, 0} -> 1, {1, 1} -> 2, {0, 1} -> 3}, {2}], {2}]
		}
	]
]

PauliForm[ps_ ? PauliStabilizerQ] := PauliForm[ps["Signs"], ps["Tableau"]]
PauliForm[signs_, tableau_] := MapThread[
	StringJoin,
	{
		Replace[signs, {1 -> "", -1 -> "-"}, {1}],
		StringJoin @@@ Replace[Transpose[tableau, {3, 2, 1}], {{0, 0} -> "I", {1, 0} -> "X", {1, 1} -> "Y", {0, 1} -> "Z"}, {2}]
	}
]

TableauForm[ps_ ? PauliStabilizerQ] := TableauForm[ps["Signs"], ps["Tableau"]]
TableauForm[signs_, tableau_] := Row[{Column[Replace[signs, {1 -> "", -1 -> "-"}, {1}]], MatrixForm[{{Grid[{Grid /@ Transpose /@ tableau}, Dividers -> {{{False, True}}, False}]}}]}]

MakeBoxes[ps_PauliStabilizer ? PauliStabilizerQ, form_] ^:= BoxForm`ArrangeSummaryBox["PauliStabilizer",
    ps,
    Framed["\[ScriptCapitalS]"],
    {{BoxForm`SummaryItem[{PauliForm[ps]}]}, {BoxForm`SummaryItem[{TableauForm[ps]}]}},
    {{}},
    form
]

