Package["Wolfram`QuantumFramework`"]

PackageExport["QuditName"]

PackageScope["$QuditZero"]
PackageScope["$QuditIdentity"]


QuditName["Properties"] = {"Name", "DualQ", "Dual", "Qudits"}

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

Options[QuditName] = {"Dual" -> False}

QuditName[QuditName[names__, opts : OptionsPattern[QuditName]], newOpts : OptionsPattern[]] :=
    QuditName[names, Sequence @@ DeleteDuplicatesBy[{newOpts, opts}, First]]

QuditName[] := QuditName[$QuditIdentity]

QuditName[{}] := QuditName[$QuditZero]

QuditName[name_] := QuditName[name, Sequence @@ Options[QuditName]]

_QuditName["Properties"] := QuditName["Properties"]

QuditName[name_, OptionsPattern[QuditName]]["Name"] := name

QuditName[names__, OptionsPattern[QuditName]]["Name"] := {names}

qbn_QuditName["DualQ"] := TrueQ[Lookup[Options[qbn], "Dual", False]]

qbn_QuditName["Dual"] := QuditName[qbn, "Dual" -> Not @ qbn["DualQ"]]

QuditName[$QuditIdentity, opts : OptionsPattern[QuditName]] /; TrueQ[Lookup[{opts}, "Dual", False]] := QuditName[]

qbn_QuditName["Qudits"] := Length @ DeleteCases[QuditName[$QuditIdentity, ___]] @ Normal @ qbn

qbn_QuditName["Pretty"] := simplifyName[CircleTimes @@ Normal[qbn]]


splitQuditName[qbn : QuditName[name_ ? nameHeadQ, ___]] :=
    Catenate[If[nameLength[#] > 1, splitQuditName, List] @ If[qbn["DualQ"], QuditName[#]["Dual"], QuditName[#]] & /@ (List @@ name)]

splitQuditName[qbn : QuditName[_, OptionsPattern[]]] := {qbn}

splitQuditName[qbn : QuditName[names___, OptionsPattern[]]] :=
    Catenate[splitQuditName @* If[qbn["DualQ"], QuditName[#]["Dual"] &, QuditName] /@ {names}]

QuditName /: Normal[qbn_QuditName] := splitQuditName @ qbn


groupQuditName[qbn_QuditName] := QuditName @@
    SequenceReplace[Normal[qbn],
        qbns : {Repeated[_QuditName, {2, Infinity}]} /; Equal @@ (#["DualQ"] & /@ qbns) :>
            QuditName[Flatten[#["Name"] & /@ qbns], "Dual" -> First[qbns]["DualQ"]]
]

qbn_QuditName["Group"] := groupQuditName @ qbn

qbn_QuditName[{"Permute", perm_Cycles, outputs_Integer : 0}] :=
    (QuditName @@ MapThread[If[#2, #1["Dual"], #1] &, {Permute[Normal[qbn], perm], toggleSwap[PermutationList[perm, qbn["Qudits"]], outputs]}])["Group"]

qbn_QuditName["Take", arg_] := (QuditName @@ Take[Normal[qbn], arg])["Group"]

qbn_QuditName["Drop", arg_] := (QuditName @@ Drop[Normal[qbn], arg])["Group"]

qbn_QuditName["Delete", arg_] := (QuditName @@ Delete[Normal[qbn], arg])["Group"]


QuantumTensorProduct[qbn1_QuditName, qbn2_QuditName] := Which[
    qbn1["Name"] === $QuditZero || qbn2["Name"] === $QuditZero,
    QuditName[$QuditZero],
    qbn1["Name"] === $QuditIdentity,
    qbn2,
    qbn2["Name"] === $QuditIdentity,
    qbn1,
    qbn1["DualQ"] === qbn2["DualQ"],
    QuditName[Flatten @ {qbn1["Name"], qbn2["Name"]}, "Dual" -> qbn1["DualQ"]],
    True,
    QuditName[qbn1, qbn2]
]["Group"]


QuditName /: MakeBoxes[qbn : QuditName[name_, OptionsPattern[]], format_] := With[{
    boxes = Switch[name, $QuditZero, "\[EmptySet]", $QuditIdentity, "\[ScriptOne]", _,
            If[ FreeQ[qbn["Name"], _QuditName],
                TemplateBox[{RowBox[Riffle[ToBoxes[#, format] & /@ {##}, "\[InvisibleSpace]"]]}, If[qbn["DualQ"], "Bra", "Ket"]],
                RowBox[ToBoxes[#, format] & /@ {##}]
            ] & @@ If[qbn["Qudits"] > 1, name, {name}]]
},
    InterpretationBox[boxes, qbn]
]

QuditName /: MakeBoxes[qbn : QuditName[names__, OptionsPattern[]], format_] :=
    ToBoxes[Interpretation[Row @ If[qbn["DualQ"], Map[#["Dual"] &], Identity] @ {names}, qbn], format]

