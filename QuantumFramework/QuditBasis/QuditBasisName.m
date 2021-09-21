Package["QuantumFramework`"]

PackageExport["QuditBasisName"]



QuditBasisName["Properties"] = {"Name", "DualQ", "Dual", "Qudits"}

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

QuditBasisName[] := QuditBasisName[$QuditIdentity]

QuditBasisName[{}] := QuditBasisName[$QuditZero]


_QuditBasisName["Properties"] := QuditBasisName["Properties"]

QuditBasisName[name_, OptionsPattern[QuditBasisName]]["Name"] := name

QuditBasisName[names__, OptionsPattern[QuditBasisName]]["Name"] := {names}

qbn_QuditBasisName["DualQ"] := TrueQ[Lookup[Options[qbn], "Dual", False]]

qbn_QuditBasisName["Dual"] := QuditBasisName[qbn, "Dual" -> Not @ qbn["DualQ"]]

qbn_QuditBasisName["Qudits"] := Length @ DeleteCases[QuditBasisName[$QuditIdentity, ___]] @ Normal @ qbn

qbn_QuditBasisName["Pretty"] := simplifyName[CircleTimes @@ Normal[qbn]]


splitQuditBasisName[QuditBasisName[name_ ? nameHeadQ, args___]] :=
    Catenate[If[nameLength[#] > 1, splitQuditBasisName, List] @ QuditBasisName[#, args] & /@ (List @@ name)]

splitQuditBasisName[qbn : QuditBasisName[_, OptionsPattern[]]] := {qbn}

splitQuditBasisName[qbn : QuditBasisName[names___, OptionsPattern[]]] :=
    Catenate[splitQuditBasisName @* If[qbn["DualQ"], QuditBasisName[#]["Dual"] &, QuditBasisName] /@ {names}]

QuditBasisName /: Normal[qbn_QuditBasisName] := splitQuditBasisName @ qbn


groupQuditBasisName[qbn_QuditBasisName] := QuditBasisName[##, "Dual" -> qbn["DualQ"]] & @@
    SequenceReplace[Normal[qbn],
        qbns : {Repeated[_QuditBasisName, {2, Infinity}]} /; Equal @@ (#["DualQ"] & /@ qbns) :>
            QuditBasisName[Flatten[#["Name"] & /@ qbns], "Dual" -> First[qbns]["DualQ"]]
]

qbn_QuditBasisName[{"Permute", perm_Cycles}] := groupQuditBasisName @ QuditBasisName @ Permute[splitQuditBasisName[qbn], perm]


qbn_QuditBasisName["Group"] := groupQuditBasisName @ qbn


QuantumTensorProduct[qbn1_QuditBasisName, qbn2_QuditBasisName] := (*groupQuditBasisName @ QuditBasisName @ splitQuditBasisName @ *)Which[
    qbn1["Name"] === $QuditZero || qbn2["Name"] === $QuditZero,
    QuditBasisName[$QuditZero],
    qbn1["Name"] === $QuditIdentity,
    qbn2,
    qbn2["Name"] === $QuditIdentity,
    qbn1,
    qbn1["DualQ"] === qbn2["DualQ"],
    QuditBasisName[Flatten @ {qbn1["Name"], qbn2["Name"]}],
    True,
    QuditBasisName[qbn1, qbn2]
]


QuditBasisName /: MakeBoxes[qbn : QuditBasisName[name_, OptionsPattern[]], format_] :=
    Switch[name, $QuditZero, "\[EmptySet]", $QuditIdentity, "\[ScriptOne]", _,
        TemplateBox[{RowBox[Riffle[ToBoxes[#, format] & /@ {##}, "\[InvisibleSpace]"]]}, If[qbn["DualQ"], "Bra", "Ket"]] & @@ If[qbn["Qudits"] > 1, name, {name}]]

QuditBasisName /: MakeBoxes[qbn : QuditBasisName[names__, OptionsPattern[]], format_] :=
    ToBoxes[Row @ If[qbn["DualQ"], Map[#["Dual"] &], Identity] @ {names}, format]

