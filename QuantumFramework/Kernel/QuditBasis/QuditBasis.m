Package["Wolfram`QuantumFramework`"]

PackageExport["QuditBasis"]
PackageExport["QuditName"]

PackageScope["QuditBasisQ"]



QuditBasis::inconsistentNames = "element names should have the same length";

QuditBasis::inconsistentElements = "elements should have the same dimensions";

QuditBasis::dependentElements = "elements should be linearly independent";



quditBasisQ[QuditBasis[representations_Association]] := Enclose[
Module[{
    elementQudits, numericElements
},
    ConfirmAssert[AllTrue[Keys @ representations, MatchQ[{_QuditName, _Integer ? Positive}]]];

    True
],
False &
]

quditBasisQ[___] := False


QuditBasisQ[qb : QuditBasis[_]] := System`Private`ValidQ[qb]

QuditBasisQ[___] := False

qb_QuditBasis /; System`Private`HoldNotValidQ[qb] && quditBasisQ[Unevaluated @ qb] := System`Private`HoldSetValid[qb]


QuditBasis[] := QuditBasis[{QuditName[]}, {1}]

QuditBasis[{1...}] := QuditBasis[]

QuditBasis[qb_QuditBasis] := qb

QuditBasis[assoc_Association] /; ! AllTrue[Keys[assoc], MatchQ[{_QuditName, _Integer ? Positive}]] :=
    QuditBasis[
        KeyMap[{QuditName[#], 1} &, assoc]
    ]

QuditBasis[names : {Except[_Integer | (name_String | {name_String, ___} /; MemberQ[$QuditBasisNames, name])] ..}] :=
    QuditBasis[names, IdentityMatrix[Length[names]]]

QuditBasis[names_List, elements_List] :=
    QuditBasis[AssociationThread[names, elements]]

QuditBasis[elements_Association] /; Not @ AllTrue[elements, NumericQ[#] || SparseArrayQ[#] &] :=
    QuditBasis[Map[If[NumericQ[#], #, SparseArray[#]] &, elements]]

QuditBasis[elements_Association] /; !OrderedQ[Reverse /@ Keys[elements]] :=
    QuditBasis[KeySortBy[elements, Reverse]]


(* tensor product of multiple parameter basis *)

QuditBasis[{name_String, params_List}, args___] := QuantumTensorProduct @@ (QuditBasis[{name, #}, args] & /@ params)

QuditBasis[params_List] := QuantumTensorProduct @@ (QuditBasis /@ params)


(* multiplicity *)

QuditBasis[
    name : _String | {_String, PatternSequence[] | PatternSequence[Except[_List], ___]} | _Integer | _Association,
    multiplicity_Integer ? Positive, args___] :=
    QuditBasis[QuditBasis[name, args], multiplicity, args]

QuditBasis[_QuditBasis, 0] := QuditBasis[]

QuditBasis[dimension_Integer, multiplicity_Integer ? Positive] := QuditBasis[QuditBasis[dimension], multiplicity]

QuditBasis[qb_QuditBasis ? QuditBasisQ, multiplicity_Integer ? Positive] :=
    If[multiplicity > 1, QuantumTensorProduct[Table[qb, multiplicity]], qb]


(* basis cast *)

QuditBasis[source_QuditBasis, target_QuditBasis] := If[
    target["Dimension"] > source["Dimension"],
    target[{"TakeDimension", source["Dimension"]}],
    QuantumTensorProduct[target, source[{"DropDimension", target["Dimension"]}]]
]

QuditBasis /: qb1_QuditBasis == qb2_QuditBasis := Values[qb1["Representations"]] === Values[qb2["Representations"]]


(* formatting *)

QuditBasis /: MakeBoxes[qb_QuditBasis /; QuditBasisQ[Unevaluated @ qb], format_] :=
    With[{boxes = ToBoxes[Normal /@ qb["Association"], format]}, InterpretationBox[boxes, qb]]

