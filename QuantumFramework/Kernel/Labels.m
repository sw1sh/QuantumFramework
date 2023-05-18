Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumShortcut"]

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
    SuperDagger["U2"[a_, b_]] :> "U2"[Pi - b, Pi - a],
    SuperDagger["U"[a_, b_, c_]] :> "U"[- a, - b, - c],
    SuperDagger["\[Pi]"[args__]] :> "\[Pi]"[args],
    SuperDagger[SuperDagger[label_]] :> label
}]


QuantumShortcut[qo_QuantumOperator] := Replace[
    QuantumShortcut[qo["Label"], First @ qo["Dimensions"], qo["TargetOrder"]],
    {
        _Missing /; qo["Dimensions"] === {2, 2} && MatrixQ[qo["Matrix"], NumericQ] :> QuantumShortcut[qo["ZYZ"]],
        _Missing :> qo["Matrix"]
    }
]

QuantumShortcut[qmo_QuantumMeasurementOperator] := qmo["InputOrder"]

QuantumShortcut[qc_QuantumCircuitOperator] := QuantumShortcut /@ qc["Operators"]

QuantumShortcut[qc_QuantumChannel] := qc

QuantumShortcut[label_, dim_ : 2, order_ : {}] := With[{nameOrder = If[order === {}, Identity, # -> order &]},
    Replace[label, {
        Subscript["C", subLabel_Composition][c0_, c1_] :> ({"C", nameOrder @ QuantumShortcut[#], c0, c1} & /@ Reverse[List @@ subLabel]),
        Subscript["C", subLabel_][c0_, c1_] :> {"C", QuantumShortcut[subLabel, dim, order], c0, c1},
        HoldPattern[Composition[subLabels___]] :> nameOrder @ Reverse[QuantumShortcut /@ {subLabels}],
        Superscript[subLabel_, CircleTimes[n_Integer]] /; n == Length[order] :> Thread[ConstantArray[QuantumShortcut[subLabel], n] -> order],
        Superscript[subLabel_, CircleTimes[n_Integer]] :> QuantumShortcut[subLabel, dim, order],
        CircleTimes[subLabels___] /; Length[{subLabels}] == Length[order] :> MapThread[QuantumShortcut[#1, dim, {#2}] &, {{subLabels}, order}],
        Subscript["R", subLabel_Composition][angle_] :> (nameOrder @ {"R", Sow[Chop @ angle, #], QuantumShortcut[#]}& /@ Reverse[List @@ subLabel]),
        Subscript["R", subLabel_][angle_] :> {"R", Sow[Chop @ angle, subLabel], QuantumShortcut[subLabel, dim, order]},
        "\[Pi]"[perm__] :> nameOrder @ {"Permutation", PermutationCycles[{perm}]},
        OverHat[x_] :> nameOrder @ {"Diagonal", x},
        (subLabel : "P" | "PhaseShift" | "U2" | "U")[params___] :> nameOrder @ {subLabel, params},
        subLabel : "X" | "Y" | "Z" | "I" | "NOT" :> nameOrder @ If[dim === 2, subLabel, {subLabel, dim}],
        SuperDagger[subLabel_] :> nameOrder @ SuperDagger[QuantumShortcut[subLabel, dim, order]],
        name_ :> If[MemberQ[$QuantumOperatorNames, name], Identity, Missing] @ nameOrder[name]
    }]
]

QuantumShortcut = QuantumShortcut

