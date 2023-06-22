Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumBasis"]

PackageScope["quantumBasisQ"]
PackageScope["QuantumBasisQ"]
PackageScope["$QuantumBasisDataKeys"]
PackageScope["$QuantumBasisPictures"]
PackageScope["MergeParameterSpecs"]



$QuantumBasisPictures = {
    "Schrodinger", "Schroedinger", "Schrödinger",
     "Heisenberg", "Interaction", "PhaseSpace"
};


(* Messages *)

QuantumBasis::wrongData = "has wrong data";

QuantumBasis::picture = "should have one of the following pictures " <> StringRiffle[$QuantumBasisPictures, ", "];

QuantumBasis::zeroDimension = "can't have zero dimension";

QuantumBasis::inconsistentNames = "element names should have the same length";

QuantumBasis::inconsistentInputs = "number of input qudits `` should be positive integer and less than or equal to number of total qudits ``";

QuantumBasis::inconsistentElements = "elements should have the same dimensions";

QuantumBasis::dependentElements = "elements should be linearly independent";



$QuantumBasisDataKeys = {"Input", "Output", "Picture", "Label", "ParameterSpec"}

$QuantumBasisDefaults = {
    "Input" -> QuditBasis[],
    "Output" -> QuditBasis[],
    "Picture" -> "Schrödinger",
    "Label" -> None,
    "ParameterSpec" -> {}
}


ParameterSpecQ[spec_] := MatchQ[spec, subSpecs : {{_, _ ? NumericQ, _ ? NumericQ}...} /; DuplicateFreeQ[subSpecs[[All, 1]]]]

quantumBasisQ[QuantumBasis[data_Association]] := Enclose[
    ConfirmAssert[ContainsAll[Keys[data], $QuantumBasisDataKeys], Message[QuantumBasis::wrongData]];

    ConfirmAssert[QuditBasisQ[data["Input"]]];
    ConfirmAssert[QuditBasisQ[data["Output"]]];

    ConfirmAssert[MemberQ[$QuantumBasisPictures, data["Picture"]], Message[QuantumBasis::picture]];
    (*ConfirmAssert[Length[inputElements] + Length[outputElements] > 0, Message[QuantumBasis::zeroDimension]];*)

    ConfirmAssert[ParameterSpecQ[data["ParameterSpec"]]];
    True,

    False &
]

quantumBasisQ[___] := False


QuantumBasisQ[qb : QuantumBasis[_]] := System`Private`HoldValidQ[qb]

QuantumBasisQ[___] := False


(* mutation *)

QuantumBasis[qb_QuantumBasis] := qb

QuantumBasis[picture : Alternatives @@ $QuantumBasisPictures] := QuantumBasis["Computational", picture]

QuantumBasis[QuantumBasis[data_Association], picture : Alternatives @@ $QuantumBasisPictures] := QuantumBasis[<|data, "Picture" -> picture|>]

QuantumBasis[QuantumBasis[data_Association], rules : OptionsPattern[]] :=
    QuantumBasis[<|data, Reverse @ Select[Flatten[{rules}], MemberQ[$QuantumBasisDataKeys, First[#]] &]|>]

QuantumBasis[data_Association, args__] := Enclose @ Fold[ConfirmBy[QuantumBasis[##], QuantumBasisQ] &, ConfirmBy[QuantumBasis[data], QuantumBasisQ], Reverse @ {args}]



(* construction *)

QuantumBasis[elements : (a_ /; ArrayQ[a, d_ /; d > 1, NumericQ]) | _Association ? (Not @* KeyExistsQ["Output"]), args___] :=
    Enclose @ QuantumBasis[<|"Output" -> ConfirmBy[QuditBasis[elements], QuditBasisQ]|>, args]

QuantumBasis[output : _QuditBasis | _List, input : _QuditBasis | _List, args___] :=
    Enclose @ QuantumBasis["Output" -> ConfirmBy[QuditBasis[output], QuditBasisQ], "Input" -> ConfirmBy[QuditBasis[input], QuditBasisQ]["Dual"], args]

QuantumBasis[output_QuditBasis ? QuditBasisQ, args___] := QuantumBasis["Output" -> output, args]

QuantumBasis[arg : {(_QuditName | _Integer | _ ? propQ) ..}, args___] := Enclose @ QuantumBasis["Output" -> ConfirmBy[QuditBasis[arg], QuditBasisQ], args]

QuantumBasis[params_List, args___] := Enclose @ QuantumTensorProduct[ConfirmBy[QuantumBasis[#, args], QuantumBasisQ] & /@ params]

QuantumBasis[{}, args___] := QuantumBasis[QuditBasis[{}], args]

(* defaults *)
QuantumBasis[data_Association ? (Keys /* Not @* ContainsAll[$QuantumBasisDataKeys]), args___] :=
    QuantumBasis[<|$QuantumBasisDefaults, data|>, args]


QuantumBasis[data_Association] /; AssociationQ[data["Input"]] := QuantumBasis[MapAt[QuditBasis, data, "Input"]]

QuantumBasis[data_Association] /; AssociationQ[data["Output"]] := QuantumBasis[MapAt[QuditBasis, data, "Output"]]


defaultParameterSpec[{p : Except[_List]}] := defaultParameterSpec[{p, 0, 1}]

defaultParameterSpec[{p_, i_ ? NumericQ}] := defaultParameterSpec[{p, i, i + 1}]

defaultParameterSpec[{p_, i_ ? NumericQ, f_ ? NumericQ}] := {{p, i, f}}

defaultParameterSpec[ps_List] := Catenate[defaultParameterSpec /@ ps]

(* defaultParameterSpec[spec : Except[{_List...}, _List]] := defaultParameterSpec[Take[spec, UpTo[3]]] *)

defaultParameterSpec[p : Except[_List]] := defaultParameterSpec[{p}]


QuantumBasis[data_Association] /; !MatchQ[data["ParameterSpec"], {{_, _, _}...}] :=
    QuantumBasis[<|data, "ParameterSpec" -> defaultParameterSpec[data["ParameterSpec"]]|>]


(* multiplicity *)

QuantumBasis[qb_ ? QuantumBasisQ, 1, args___] := QuantumBasis[qb, args]

QuantumBasis[qb_ ? QuantumBasisQ, multiplicity_Integer, args___] := QuantumBasis[qb,
    "Input" -> QuditBasis[qb["Input"], multiplicity],
    "Output" -> QuditBasis[qb["Output"], multiplicity],
    args
]

QuantumBasis[arg_, multiplicity_Integer ? Positive, args___] := Enclose @ With[{
    bases = Table[ConfirmBy[QuantumBasis[arg, args], QuantumBasisQ], multiplicity]
},
    If[ multiplicity > 0,
        If[ Equal @@ bases,
            QuantumBasis[
                QuantumTensorProduct[bases],
                "Label" -> If[multiplicity > 1, First[bases]["Label"] ^ CircleTimes[multiplicity], First[bases]["Label"]]
            ],
            QuantumTensorProduct[bases]
        ],
        QuantumBasis[args]
    ]
]

QuantumBasis[param : name_String | {name_String, ___}, args___] :=
    Enclose @ QuantumBasis[<|"Output" -> ConfirmBy[QuditBasis[param], QuditBasisQ], "Label" -> StringDelete[name, "Basis"]|>, args]

QuantumBasis[param : _ ? nameQ | _Integer, args___] :=
    Enclose @ QuantumBasis[<|"Output" -> ConfirmBy[QuditBasis[param], QuditBasisQ]|>, args]

QuantumBasis[args : (_String ? (MatchQ[Alternatives @@ $QuantumBasisPictures]) | OptionsPattern[]) ...] :=
    QuantumBasis["Computational", args, "Label" -> None]

QuantumBasis[qb_QuantumBasis, args__] := Enclose @ QuantumBasis[ConfirmBy[QuantumBasis[args], QuantumBasisQ], qb["Meta"]]

qb_QuantumBasis /; System`Private`HoldNotValidQ[qb] && quantumBasisQ[Unevaluated @ qb] := (
    System`Private`HoldSetValid[qb];
    System`Private`HoldSetNoEntry[qb]
)


(* equality *)

QuantumBasis /: Equal[qb__QuantumBasis ? QuantumBasisQ] := Equal @@ (#["Input"] & /@ {qb}) && Equal @@ (#["Output"] & /@ {qb})


(* N *)

N[qb_QuantumBasis, n_] /; ! AllTrue[
    Join[Values[qb["Output"]["Representations"]], Values[qb["Input"]["Representations"]]],
    InexactNumberQ[#] || ArrayQ[#, _, InexactNumberQ] &] := QuantumBasis[qb, "Output" -> N[qb["Output"], n], "Input" -> N[qb["Input"], n]]

SetAttributes[QuantumBasis, NHoldAll]


(* addition *)

QuantumBasis /: Plus[qb__QuantumBasis ? QuantumBasisQ] :=
    QuantumBasis[
        "Output" -> Plus @@ (#["Output"] & /@ {qb}),
        "Input" -> Plus @@ (#["Input"] & /@ {qb}),
        "Label" -> Plus @@ (#["Label"] & /@ {qb})
    ]

MergeParameterSpecs[objs___] := Enclose @ Block[{specs = Catenate[ConfirmBy[#["ParameterSpec"], ParameterSpecQ] & /@ {objs}]},
    KeyValueMap[Prepend[#2, #1] &, GroupBy[specs, First, {Min[#[[All, 2]]], Max[#[[All, 3]]]} & ]]
]

