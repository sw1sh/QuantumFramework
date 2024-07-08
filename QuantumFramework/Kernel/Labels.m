Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumShortcut"]

PackageScope["simplifyLabel"]
PackageScope["expandLabel"]
PackageScope["collectLabel"]
PackageScope["labelList"]
PackageScope["sortLabel"]



simplifyLabel[x : _QuantumBasis | _QuantumState] :=
    Head[x][x, "Label" -> simplifyLabel[x["Label"], x["OutputDimensions"], x["InputDimensions"]]]

simplifyLabel[x : _QuantumOperator | _QuantumMeasurementOperator | _QuantumMeasurement | _QuantumChannel] :=
    Head[x][x, "Label" -> simplifyLabel[x["Label"], x["OutputDimensions"], x["InputDimensions"], x["Order"]]]

simplifyLabel[qc : _QuantumCircuitOperator] :=
    QuantumCircuitOperator[qc, "Label" -> simplifyLabel[qc["Label"], qc["OutputDimensions"], qc["InputDimensions"], qc["Order"]], FilterRules[qc["Options"], Except["Label"]]]

conj = SuperDagger | SuperStar

(* TODO: dimensions and order dependence *)
simplifyLabel[l_, out_ : None, in_ : None, order_ : None] := Replace[l, {
    _CircleTimes :> Map[simplifyLabel, l],
    conj[label : None | "0" | "1" | "X" | "I" | "NOT" | "SWAP" | "Cap" | "Cup" | "Measurement"[_] | "Channel"[_]] :> label,
    conj[label : "Z" | "H"] /; out == in == {2} :> label,
    SuperStar[label: "S" | "T"] :> SuperDagger[label],
    (h : conj)[Superscript[label_, p_CircleTimes]] :> Superscript[simplifyLabel[h[label]], p],
    (h : conj)[t_CircleTimes] :> simplifyLabel @* h /@ t,
    SuperDagger[c_Composition] :> simplifyLabel @* SuperDagger /@ Reverse[c],
    SuperStar[c_Composition] :> simplifyLabel @* SuperStar /@ c,
    (h : conj)[Subscript["C", x_][rest__]] :> Subscript["C", simplifyLabel[h[x]]][rest],
    conj[Subscript["R", args__][a_]] :> Subscript["R", args][- a],
    conj[(r : Subscript["R", _] | "P")[angle_]] :> r[- angle],
    conj["PhaseShift"[n_]] :> "PhaseShift"[-n],
    conj[n_Integer] :> -n,
    SuperDagger["U2"[a_, b_]] :> "U2"[Pi - b, Pi - a],
    SuperStar["U2"[a_, b_]] :> "U2"[- a, - b],
    SuperDagger["U"[a_, b_, c_]] :> "U"[- a, - c, - b],
    SuperStar["U"[a_, b_, c_]] :> "U"[a, - b, - c],
    conj["\[Pi]"[args__]] :> "\[Pi]"[args],
    SuperDagger[Ket[x_]] :> Bra[x],
    SuperDagger[Bra[x_]] :> Ket[x],
    conj[(name : "XSpider" | "YSpider" | "ZSpider" | "Spider")[phase_]] :> name[-phase],
    conj["WSpider"] :> "WSpider",
    conj["Curry"] :> "Uncurry",
    conj["Uncurry"] :> "Curry",
    conj[qn_QuditName] :> simplifyLabel[qn["Dual"]],
    (h : conj)[(h : conj)[label_]] :> simplifyLabel[label, out, in, order]
}]

expandLabel[label_CircleTimes] := Flatten[expandLabel /@ label, Infinity, CircleTimes]
expandLabel[Superscript[subLabel_, CircleTimes[n_Integer]]] := CircleTimes @@ ConstantArray[expandLabel[subLabel], n]
expandLabel[label_] := label

collectLabel[label_] := FixedPoint[
    Replace[{
        CircleTimes[x_] :> x,
        ct_CircleTimes :> CircleTimes @@ SequenceReplace[List @@ ct, xs : {Repeated[x_, {2, Infinity}]} :> Superscript[x, CircleTimes[Length[xs]]]]
    }],
    label
]

labelList[label_] := Replace[
    expandLabel[label], {
        ct_CircleTimes :> Catenate[labelList /@ List @@ ct],
        c_Composition :>  Catenate[labelList /@ List @@ c],
        l_ :> {l}
    }
]

sortLabel[label_, order_] := collectLabel @ With[{subLabels = labelList[label]},
    If[Length[subLabels] == Length[order], CircleTimes @@ subLabels[[Ordering[order]]], label]
]


Options[QuantumShortcut] = {"Decompose" -> True}

QuantumShortcut[qo_QuantumOperator, OptionsPattern[]] := Replace[
    QuantumShortcut[qo["Label"], First[qo["Dimensions"], 1], qo["Order"]],
    {
        {"Reset" -> order_} :> {{"Reset", qo[]} -> order},
        _Missing /; TrueQ[OptionValue["Decompose"]] && qo["VectorQ"] && qo["Dimensions"] === {2, 2} && MatrixQ[qo["Matrix"], NumericQ] && MatchQ[qo["Order"], {{_}, {_}}] :> QuantumShortcut[qo["ZYZ"]],
        _Missing :> {Labeled[qo["Matrix"] -> qo["Order"], qo["Label"]]}
    }
]

QuantumShortcut[qmo_QuantumMeasurementOperator, OptionsPattern[]] :=
    {If[MatchQ[qmo["Label"], None | "I" | "I"[___]], "M", {"M", qmo["Label"]}] -> If[AllTrue[qmo["TargetDimensions"], # == 2 &], qmo["InputOrder"], qmo["InputOrder"] -> {"I"[qmo["TargetDimensions"]], "Label" -> None}]}

QuantumShortcut[qc_QuantumCircuitOperator, opts : OptionsPattern[]] := Catenate[QuantumShortcut[#, opts] & /@ qc["Operators"]]

QuantumShortcut[qc_QuantumChannel, OptionsPattern[]] := {qc}

QuantumShortcut[label_, dim_Integer : 2, order : _ ? orderQ, opts : OptionsPattern[]] := QuantumShortcut[label, dim, {order, order}, opts]

QuantumShortcut[label_, dim_Integer : 2, order : {outputOrder : _ ? orderQ, inputOrder : _ ? orderQ} : {}, opts : OptionsPattern[]] := Enclose[Confirm @ With[{
    nameOrder = Which[order === {}, Identity, outputOrder === inputOrder, # -> inputOrder &, True, # -> order &]
},
    Replace[label, {
        HoldPattern[Composition[subLabels___]] :> Catenate[Reverse[Confirm @ QuantumShortcut[#, dim, order, opts] & /@ {subLabels}]],
        Subscript["C", subLabel_][{}, {}] :> QuantumShortcut[subLabel, dim, order, opts],
        Subscript["C", subLabel_][controls__] :> ({"C", #, controls} & /@ Confirm @ QuantumShortcut[subLabel, dim, Complement[Join @@ order, Flatten[{controls}]], opts]),
        Superscript[subLabel_, CircleTimes[n_Integer]] /; n == Length[inputOrder] :> Catenate @ MapThread[Thread[#1 -> {#2}, List, 1] &, {ConstantArray[Confirm @ QuantumShortcut[subLabel, opts], n], inputOrder}],
        Superscript[subLabel_, _CircleTimes] :> QuantumShortcut[subLabel, dim, order, opts],
        ct : CircleTimes[___, Superscript[_, CircleTimes[_Integer]], ___] :> QuantumShortcut[Replace[ct, Superscript[subLabel_, CircleTimes[n_Integer]] :> Splice[ConstantArray[subLabel, n], CircleTimes], {1}], dim, order, opts],
        CircleTimes[subLabels___] /; Length[{subLabels}] == Length[inputOrder] :>
            Catenate @ MapThread[Confirm @ QuantumShortcut[#1, dim, {#2}, opts] &, {{subLabels}, inputOrder}],
        Subscript["R", subLabel_][angle_] :> {{"R", Sow[Chop @ angle], Confirm @ QuantumShortcut[subLabel, dim, order, opts]}},
        "\[Pi]"[perm__] :> {nameOrder @ {"Permutation", PermutationCycles[{perm}]}},
        OverHat[x_] :> {nameOrder @ {"Diagonal", x}},
        (subLabel : "P" | "PhaseShift" | "U2" | "U" | "ZSpider" | "XSpider" | "Spider")[params___] :> {nameOrder @ {subLabel, params}},
        (subLabel : "GlobalPhase")[params___] :> {{subLabel, params}},
        (subLabel : "X" | "Y" | "Z" | "I" | "NOT" | "Cap" | "Cup") | (subLabel : "I")[_] :> {nameOrder @ If[dim === 2, subLabel, {subLabel, dim}]},
        Times[x_ ? NumericQ, subLabel_] :> QuantumShortcut[Composition[OverHat[x], subLabel], dim, order, opts],
        (h : SuperDagger | SuperStar)[subLabel_] :> MapAt[h, Confirm @ QuantumShortcut[subLabel, dim, order, opts], {All, 1}],
        "Reset"[_] :> {nameOrder["Reset"]},
        name_ /; MemberQ[$QuantumOperatorNames, name] :> {nameOrder[name]},
        barrier_ ? BarrierQ :> {barrier},
        _ :> Missing[label]
    }]
],
    Missing[label] &
]

