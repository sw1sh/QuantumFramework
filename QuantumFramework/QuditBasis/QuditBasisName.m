Package["QuantumFramework`"]

PackageExport["QuditBasisName"]



$QuditZero = CircleTimes[]

$QuditIdentity = \[FormalCapitalI]


nameHeadQ[name_] := MatchQ[name, _TensorProduct | _CircleTimes | _List]


nameLength[name_ ? nameHeadQ] := Length @ name

nameLength[_] := 1


simplifyName[name_] := With[{
    noIdentities = DeleteCases[name, $QuditIdentity]
},
    If[ nameHeadQ[noIdentities],
        If[ nameLength[noIdentities] == 1,
            First[noIdentities, $QuditIdentity],
            If[ nameLength[noIdentities] == 0 && nameLength[name] > 0,
                $QuditIdentity,
                noIdentities
            ]
        ]
    ]
]

Options[QuditBasisName] = {"Dual" -> False}

QuditBasisName[QuditBasisName[names__, opts : OptionsPattern[QuditBasisName]], newOpts : OptionsPattern[]] :=
    QuditBasisName[names, Sequence @@ DeleteDuplicatesBy[{newOpts, opts}, First]]

QuditBasisName /: Normal[QuditBasisName[name_ ? nameHeadQ, OptionsPattern[QuditBasisName]]] := List @@ name

QuditBasisName /: Normal[QuditBasisName[names__, OptionsPattern[QuditBasisName]]] := {names}


QuditBasisName[] := QuditBasisName[$QuditIdentity]

QuditBasisName[{}] := QuditBasisName[$QuditZero]

(*QuditBasisName[names_List, opts : OptionsPattern[]] := QuditBasisName[##, opts] & @@ QuditBasisName /@ names*)

QuditBasisName[name_, OptionsPattern[QuditBasisName]]["Name"] := name

QuditBasisName[names__, OptionsPattern[QuditBasisName]]["Name"] := {names}

qbn_QuditBasisName["DualQ"] := TrueQ[Lookup[Options[qbn], "Dual", False]]

qbn_QuditBasisName["Dual"] := QuditBasisName[qbn, "Dual" -> Not @ qbn["DualQ"]]


qbn_QuditBasisName["Qudits"] := nameLength @ DeleteCases[$QuditIdentity] @ Normal @ qbn



qbn_QuditBasisName["Pretty"] := simplifyName[CircleTimes @@ Normal[qbn]]


QuantumTensorProduct[qbn1_QuditBasisName, qbn2_QuditBasisName] := Which[
    qbn1["Name"] === $QuditZero || qbn2["Name"] === $QuditZero,
    QuditBasisName[$QuditZero],
    qbn1["Name"] === $QuditIdentity,
    qbn2,
    qbn2["Name"] === $QuditIdentity,
    qbn1,
    qbn1["DualQ"] === qbn2["DualQ"],
    QuditBasisName[{qbn1["Name"], qbn2["Name"]}],
    True,
    QuditBasisName[qbn1, qbn2]
]


Format[qbn : QuditBasisName[name_, OptionsPattern[]]] :=
    Switch[name, $QuditZero, "\[EmptySet]", $QuditIdentity, "\[ScriptOne]", _, If[qbn["DualQ"], Bra, Ket] @@ Normal[qbn]]

Format[qbn : QuditBasisName[names__, OptionsPattern[]]] := Row @ If[qbn["DualQ"], #["Dual"] & /@ {names}, {names}]
