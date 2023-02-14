Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumLabelName"]

PackageScope["simplifyLabel"]



simplifyLabel[op_QuantumOperator] := QuantumOperator[op, "Label" -> simplifyLabel[op["Label"]]]

simplifyLabel[l_] := Replace[l, {
    SuperDagger[label : None | "X" | "Y" | "Z" | "I" | "NOT" | "H" | "SWAP"] :> label,
    SuperDagger[Superscript[label_, p_CircleTimes]] :> Superscript[simplifyLabel[SuperDagger[label]], p],
    SuperDagger[t_CircleTimes] :> simplifyLabel @* SuperDagger /@ t,
    SuperDagger[c_Composition] :> simplifyLabel @* SuperDagger /@ Reverse[c],
    SuperDagger[Subscript["C", x_][rest__]] :> Subscript["C", simplifyLabel[SuperDagger[x]]][rest],
    SuperDagger[Subscript["R", args__][a_]] :> Subscript["R", args][- a],
    SuperDagger[(r : Subscript["R", _] | "P")[angle_]] :> r[- angle],
    SuperDagger["PhaseShift"[n_] | n_Integer] :> "PhaseShift"[-n],
    SuperDagger["U2"[a_, b_]] :> "U2"[Pi - a, Pi - b],
    SuperDagger["\[Pi]"[args__]] :> "\[Pi]"[args],
    SuperDagger[SuperDagger[label_]] :> label
}]


QuantumLabelName[qo_QuantumOperator] := QuantumLabelName[qo["Label"], qo["TargetOrder"]]

QuantumLabelName[qmo_QuantumMeasurementOperator] := qmo["InputOrder"]

QuantumLabelName[qc_QuantumCircuitOperator] := QuantumLabelName /@ qc["Operators"]

QuantumLabelName[label_, order_ : {}] := Replace[label, {
	Subscript["C", subLabel_Composition][c0_, c1_] :> ({"C", QuantumLabelName[#] -> order, c0, c1} & /@ Reverse[List @@ subLabel]),
	Subscript["C", subLabel_][c0_, c1_] :> {"C", QuantumLabelName[subLabel, order], c0, c1},
	HoldPattern[Composition[subLabels___]] :> Reverse[QuantumLabelName /@ {subLabels}] -> order,
	Superscript[subLabel_, CircleTimes[n_Integer]] /; n == Length[order] :> Thread[ConstantArray[QuantumLabelName[subLabel], n] -> order],
	Superscript[subLabel_, CircleTimes[n_Integer]] :> QuantumLabelName[subLabel, order],
	Subscript["R", subLabel_Composition][angle_] :> ({"R", Sow[Chop @ angle, #], QuantumLabelName[#]} -> order & /@ Reverse[List @@ subLabel]),
	Subscript["R", subLabel_][angle_] :> {"R", Sow[Chop @ angle, subLabel], QuantumLabelName[subLabel, order]},
	"\[Pi]"[perm__] :> {"Permutation", PermutationCycles[{perm}]},
	name_ :> If[order === {}, name, name -> order]
}]

