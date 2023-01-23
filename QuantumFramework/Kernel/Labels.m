Package["Wolfram`QuantumFramework`"]

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

