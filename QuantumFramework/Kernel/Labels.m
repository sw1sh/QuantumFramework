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
        _Missing :> Labeled[qo["Matrix"], qo["Label"]]
    }
]

QuantumShortcut[qmo_QuantumMeasurementOperator] := {qmo["InputOrder"]}

QuantumShortcut[qc_QuantumCircuitOperator] := Catenate[QuantumShortcut /@ qc["Operators"]]

QuantumShortcut[qc_QuantumChannel] := {qc}

QuantumShortcut[label_, dim_ : 2, order_ : {}] := Enclose[Confirm @ With[{nameOrder = If[order === {}, Identity, # -> order &]},
    Replace[label, {
        HoldPattern[Composition[subLabels___]] :> Catenate[Reverse[Confirm @ QuantumShortcut[#, dim, order] & /@ {subLabels}]],
        Subscript["C", subLabel_][{}, {}] :> QuantumShortcut[subLabel, dim, order],
        Subscript["C", subLabel_][controls__] :> ({"C", #, controls} & /@ Confirm @ QuantumShortcut[subLabel, dim, Complement[order, Flatten[{controls}]]]),
        Superscript[subLabel_, CircleTimes[n_Integer]] /; n == Length[order] :> Thread[ConstantArray[Confirm @ QuantumShortcut[subLabel], n] -> order, List, 1],
        Superscript[subLabel_, CircleTimes[n_Integer]] :> QuantumShortcut[subLabel, dim, order],
        CircleTimes[subLabels___] /; Length[{subLabels}] == Length[order] :> Catenate @ MapThread[Confirm @ QuantumShortcut[#1, dim, {#2}] &, {{subLabels}, order}],
        Subscript["R", subLabel_][angle_] :> ({"R", Sow[Chop @ angle], subLabel} & /@ Confirm @ QuantumShortcut[subLabel, dim, order]),
        "\[Pi]"[perm__] :> {nameOrder @ {"Permutation", PermutationCycles[{perm}]}},
        OverHat[x_] :> {nameOrder @ {"Diagonal", x}},
        (subLabel : "P" | "PhaseShift" | "U2" | "U")[params___] :> {nameOrder @ {subLabel, params}},
        subLabel : "X" | "Y" | "Z" | "I" | "NOT" :> {nameOrder @ If[dim === 2, subLabel, {subLabel, dim}]},
        Times[x_ ? NumericQ, subLabel_] :> QuantumShortcut[Composition[OverHat[x], subLabel], dim, order],
        SuperDagger[subLabel_] :> nameOrder @* SuperDagger /@ Confirm @ QuantumShortcut[subLabel, dim, order],
        name_ /; MemberQ[$QuantumOperatorNames, name] :> {nameOrder[name]},
        barrier_ ? BarrierQ :> {barrier},
        _ :> Missing[label]
    }]
],
    Missing[label] &
]

QuantumShortcut = QuantumShortcut

