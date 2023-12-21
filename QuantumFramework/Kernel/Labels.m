Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumShortcut"]

PackageScope["simplifyLabel"]
PackageScope["expandLabel"]
PackageScope["collectLabel"]
PackageScope["labelList"]
PackageScope["sortLabel"]



simplifyLabel[x : _QuantumBasis | _QuantumState | _QuantumOperator] := QuantumOperator[x, "Label" -> simplifyLabel[x["Label"], x["OutputDimensions"], x["InputDimensions"], x["Order"]]]

conj = SuperDagger | SuperStar

(* TODO: dimensions and order dependence *)
simplifyLabel[l_, out_ : None, in_ : None, order_ : None] := Replace[l, {
    _CircleTimes :> Map[simplifyLabel, l],
    conj[label : None | "X" | "I" | "NOT" | "SWAP" | "Cap" | "Cup"] :> label,
    conj[label : "Y" | "Z" | "H"] /; out == in == {2} :> label,
    SuperStar[label: "S" | "T"] :> SuperDagger[label],
    (h : conj)[Superscript[label_, p_CircleTimes]] :> Superscript[simplifyLabel[h[label]], p],
    (h : conj)[t_CircleTimes] :> simplifyLabel @* h /@ t,
    SuperDagger[c_Composition] :> simplifyLabel @* SuperDagger /@ Reverse[c],
    SuperStar[c_Composition] :> simplifyLabel @* SuperStar /@ c,
    (h : conj)[Subscript["C", x_][rest__]] :> Subscript["C", simplifyLabel[h[x]]][rest],
    conj[Subscript["R", args__][a_]] :> Subscript["R", args][- a],
    conj[(r : Subscript["R", _] | "P")[angle_]] :> r[- angle],
    conj["PhaseShift"[n_] | n_Integer] :> "PhaseShift"[-n],
    SuperDagger["U2"[a_, b_]] :> "U2"[Pi - b, Pi - a],
    SuperStar["U2"[a_, b_]] :> "U2"[- a, - b],
    SuperDagger["U"[a_, b_, c_]] :> "U"[- a, - c, - b],
    SuperStar["U"[a_, b_, c_]] :> "U"[a, - b, - c],
    conj["\[Pi]"[args__]] :> "\[Pi]"[args],
    SuperDagger[Ket[x_]] :> Bra[x],
    SuperDagger[Bra[x_]] :> Ket[x],
    conj[(name : "XSpider" | "YSpider" | "ZSpider" | "Spider")[phase_]] :> name[-phase],
    conj["WSpider"] :> "WSpider",
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

QuantumShortcut[qo_QuantumOperator] := Replace[
    QuantumShortcut[qo["Label"], First[qo["Dimensions"], 1], qo["Order"]],
    {
        _Missing /; qo["Dimensions"] === {2, 2} && MatrixQ[qo["Matrix"], NumericQ] && MatchQ[qo["Order"], {{_}, {_}}] :> QuantumShortcut[qo["ZYZ"]],
        _Missing :> {Labeled[qo["Matrix"] -> qo["Order"], qo["Label"]]}
    }
]

QuantumShortcut[qmo_QuantumMeasurementOperator] := {qmo["InputOrder"]}

QuantumShortcut[qc_QuantumCircuitOperator] := Catenate[QuantumShortcut /@ qc["Operators"]]

QuantumShortcut[qc_QuantumChannel] := {qc}

QuantumShortcut[label_, dim_ : 2, order : _ ? orderQ] := QuantumShortcut[label, dim, {order, order}]

QuantumShortcut[label_, dim_ : 2, order : {outputOrder : _ ? orderQ, inputOrder : _ ? orderQ} : {}] := Enclose[Confirm @ With[{
    nameOrder = Which[order === {}, Identity, outputOrder === inputOrder, # -> inputOrder &, True, # -> order &]
},
    Replace[label, {
        HoldPattern[Composition[subLabels___]] :> Catenate[Reverse[Confirm @ QuantumShortcut[#, dim, order] & /@ {subLabels}]],
        Subscript["C", subLabel_][{}, {}] :> QuantumShortcut[subLabel, dim, order],
        Subscript["C", subLabel_][controls__] :> ({"C", #, controls} & /@ Confirm @ QuantumShortcut[subLabel, dim, Complement[Join @@ order, Flatten[{controls}]]]),
        Superscript[subLabel_, CircleTimes[n_Integer]] /; n == Length[inputOrder] :> Catenate @ MapThread[Thread[#1 -> {#2}, List, 1] &, {ConstantArray[Confirm @ QuantumShortcut[subLabel], n], inputOrder}],
        Superscript[subLabel_, _CircleTimes] :> QuantumShortcut[subLabel, dim, order],
        ct : CircleTimes[___, Superscript[_, CircleTimes[_Integer]], ___] :> QuantumShortcut[Replace[ct, Superscript[subLabel_, CircleTimes[n_Integer]] :> Splice[ConstantArray[subLabel, n], CircleTimes], {1}], dim, order],
        CircleTimes[subLabels___] /; Length[{subLabels}] == Length[inputOrder] :>
            Catenate @ MapThread[Confirm @ QuantumShortcut[#1, dim, {#2}] &, {{subLabels}, inputOrder}],
        Subscript["R", subLabel_][angle_] :> {{"R", Sow[Chop @ angle], Confirm @ QuantumShortcut[subLabel, dim, order]}},
        "\[Pi]"[perm__] :> {nameOrder @ {"Permutation", PermutationCycles[{perm}]}},
        OverHat[x_] :> {nameOrder @ {"Diagonal", x}},
        (subLabel : "P" | "PhaseShift" | "U2" | "U" | "ZSpider" | "XSpider" | "Spider")[params___] :> {nameOrder @ {subLabel, params}},
        (subLabel : "GlobalPhase")[params___] :> {{subLabel, params}},
        subLabel : "X" | "Y" | "Z" | "I" | "NOT" | "Cap" | "Cup" :> {nameOrder @ If[dim === 2, subLabel, {subLabel, dim}]},
        Times[x_ ? NumericQ, subLabel_] :> QuantumShortcut[Composition[OverHat[x], subLabel], dim, order],
        (h : SuperDagger | SuperStar)[subLabel_] :> MapAt[h, Confirm @ QuantumShortcut[subLabel, dim, order], {All, 1}],
        name_ /; MemberQ[$QuantumOperatorNames, name] :> {nameOrder[name]},
        barrier_ ? BarrierQ :> {barrier},
        _ :> Missing[label]
    }]
],
    Missing[label] &
]

