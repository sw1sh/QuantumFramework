Package["Wolfram`QuantumFramework`"]

PackageExport["QuditName"]

PackageScope["QuditNameQ"]
PackageScope["$QuditZero"]
PackageScope["$QuditIdentity"]


quditNameQ[qn_QuditName] := MatchQ[Unevaluated[qn], HoldPattern[QuditName[_, OptionsPattern[]] | QuditName[___ ? quditNameQ, OptionsPattern[]]]]

quditNameQ[___] := False

QuditNameQ[qn_QuditName] := System`Private`HoldValidQ[qn]

QuditNameQ[___] := False

qn_QuditName /; quditNameQ[Unevaluated[qn]] && ! System`Private`HoldValidQ[qn] := System`Private`HoldSetValid[qn]


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

QuditName[_[name_] ? nameHeadQ, opts___] := QuditName[name, opts]

QuditName[name_] := QuditName[Replace[name /. (Ket | Bra)[x_] :> x, x_ ? nameHeadQ :> List @@ x], Sequence @@ Options[QuditName]]

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

qbn_QuditName["Permute", perm_Cycles, outputs_Integer : 0] :=
    (QuditName @@ MapThread[If[#2, #1["Dual"], #1] &, {Permute[Normal[qbn], perm], toggleSwap[PermutationList[perm, qbn["Qudits"]], outputs]}])["Group"]

qbn_QuditName["Take", arg_] := (QuditName @@ Take[Normal[qbn], arg])["Group"]

qbn_QuditName["Drop", arg_] := (QuditName @@ Drop[Normal[qbn], arg])["Group"]

qbn_QuditName["Delete", arg_] := (QuditName @@ Delete[Normal[qbn], arg])["Group"]

isIdentity[name_] := MatchQ[name, $QuditIdentity | Subscript[$QuditIdentity, _]]

QuantumTensorProduct[qbn1_QuditName, qbn2_QuditName] := Which[
    qbn1["Name"] === $QuditZero || qbn2["Name"] === $QuditZero,
    QuditName[$QuditZero],
    isIdentity @ qbn1["Name"],
    qbn2,
    isIdentity @ qbn2["Name"],
    qbn1,
    qbn1["DualQ"] === qbn2["DualQ"],
    QuditName[Flatten @ {qbn1["Name"], qbn2["Name"]}, "Dual" -> qbn1["DualQ"]],
    True,
    QuditName[qbn1, qbn2]
]["Group"]


QuditName /: MakeBoxes[qbn : QuditName[name_, OptionsPattern[]] /; QuditNameQ[qbn], format_] := With[{
    boxes = Block[{BoxForm`UseTextFormattingQ = False},
        StyleBox[
            Switch[name, {}, "\[EmptySet]", $QuditIdentity, "\[ScriptOne]", _,
                If[ FreeQ[qbn["Name"], _QuditName],
                    TemplateBox[{RowBox[Riffle[ToBoxes[#, format] & /@ {##}, "\[InvisibleSpace]"]]}, If[qbn["DualQ"], "Bra", "Ket"]],
                    RowBox[ToBoxes[#, format] & /@ {##}]
                ] & @@ If[qbn["Qudits"] > 1, name, {name}]
            ],
            FontWeight -> "Plain"
        ]
    ]
},
    InterpretationBox[boxes, qbn]
]

QuditName /: MakeBoxes[qbn : QuditName[names__QuditName, OptionsPattern[]] /; QuditNameQ[qbn], format_] := With[{
    boxes = RowBox @ If[qbn["DualQ"], Map[ToBoxes[#["Dual"], format] &], Map[ToBoxes[#, format] &]] @ {names}
},
    InterpretationBox[boxes, qbn]
]

