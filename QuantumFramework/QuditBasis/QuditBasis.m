Package["QuantumFramework`"]

PackageExport["QuditBasis"]
PackageExport["QuditBasisName"]

PackageScope["QuditBasisQ"]



QuditBasis["Properties"] = {
    "Association", "Names", "Elements",
    "Size", "Qudits",
    "Dimensions", "NameRank", "NameTensor",
    "ElementDimensions", "ElementDimension", "Rank",
    "MatrixDimensions", "TensorDimensions",
    "Matrix", "Tensor",
    "Dual", "Reverse"
}



QuditBasis::inconsistentNames = "element names should have the same length";

QuditBasis::inconsistentElements = "elements should have the same dimensions";

QuditBasis::dependentElements = "elements should be linearly independent";



quditBasisQ[QuditBasis[names_List, elements_Association]] := Enclose[
Module[{
    elementQudits, numericElements
},
    ConfirmAssert[AllTrue[names, MatchQ[_QuditBasisName]]];

    elementQudits = #["Qudits"] & /@ names;


    ConfirmAssert[Equal @@ elementQudits, Message[QuditBasis::inconsistentNames]];

    (*numericElements = Select[elements, TensorQ[#, NumericQ] &];

    ConfirmAssert[
        Equal @@ Dimensions /@ numericElements,
        Message[QuditBasis::inconsistentElements]
    ];
    ConfirmAssert[
        (Length[numericElements] === 0 || ResourceFunction["LinearlyIndependent"] @ (Flatten /@ numericElements)),
        Message[QuditBasis::dependentElements]
    ];*)

    True
],
False &
]

quditBasisQ[___] := False


QuditBasisQ[qb : QuditBasis[_, _]] := System`Private`ValidQ[qb]

QuditBasisQ[___] := False


QuditBasis[] := QuditBasis[{QuditBasisName[]}, {1}]

QuditBasis[{1...}] := QuditBasis[]

QuditBasis[qb_QuditBasis] := qb

(*QuditBasis[elements_ /; VectorQ[elements, TensorQ], args___] := QuditBasis[
    QuditBasis[Range[0, Length[elements] - 1], elements],
    args
]*)

QuditBasis[assoc_Association, args___] := QuditBasis[Keys[assoc],
    KeyMap[{If[MatchQ[#, _QuditBasisName], #["Name"], #], 1} &, assoc], args]

QuditBasis[names_List, elements_List, args___] :=
    QuditBasis[AssociationThread[names, elements], args]

QuditBasis[names_List, elements_Association] /; Not @ AllTrue[names, MatchQ[_QuditBasisName]] :=
    QuditBasis[Map[QuditBasisName, names], elements]

QuditBasis[names_List, elements_Association] /; Not @ AllTrue[Keys[elements], MatchQ[{_, _Integer ? Positive}]] :=
    QuditBasis[names, KeyMap[{#, 1} &, elements]]

qb_QuditBasis /; System`Private`HoldNotValidQ[qb] && quditBasisQ[Unevaluated @ qb] := System`Private`HoldSetValid[qb]


QuditBasis::undefprop = "QuditBasis property `` is undefined for this basis";

(qb_QuditBasis[prop_ ? propQ, args___]) /; QuditBasisQ[qb] := With[{
    result = QuditBasisProp[qb, prop, args]
},
    (QuditBasisProp[qb, prop, args] = result) /; !FailureQ[Unevaluated @ result] &&
    (!MatchQ[result, _QuditBasisProp] || Message[QuditBasis::undefprop, prop])
]


QuditBasisProp[_, "Properties"] := QuditBasis["Properties"]

QuditBasisProp[QuditBasis[names_, _], "Names"] := names

QuditBasisProp[QuditBasis[_, elements_], "BasisElements"] := elements

QuditBasisProp[qb_, "Dimensions"] := CountDistinct /@ Transpose[Normal /@ qb["Names"]]

QuditBasisProp[qb_, "NameRank"] := Count[qb["Dimensions"], Except[1]]

QuditBasisProp[qb_, "NameTensor"] := ArrayReshape[qb["Names"], qb["Dimensions"]]

QuditBasisProp[qb_, "Elements"] := With[{elements = qb["BasisElements"]},
    TensorProduct @@@ Map[MapIndexed[Lookup[elements, Key[{#1["Name"], First[#2]}], 1] &], Normal /@ qb["Names"]]
]

QuditBasisProp[qb_, "Association"] := AssociationThread[qb["Names"], qb["Elements"]]

QuditBasisProp[qb_, "Size"] := Length[qb["Names"]]

QuditBasisProp[qb_, "Qudits"] := If[Length[qb["Names"]] > 0, First[qb["Names"]]["Qudits"], 0]

QuditBasisProp[qb_, "ElementDimensions"] := Dimensions @ First[MaximalBy[qb["Elements"], ArrayDepth], 0]

QuditBasisProp[qb_, "ElementDimension"] := Times @@ qb["ElementDimensions"]

QuditBasisProp[qb_, "Rank"] := Length @ qb["ElementDimensions"]

QuditBasisProp[qb_, "Dimension"] := Times @@ qb["Dimensions"]

QuditBasisProp[qb_, "MatrixDimensions"] := {qb["ElementDimension"], qb["Dimension"]}

QuditBasisProp[qb_, "TensorDimensions"] := Join[qb["ElementDimensions"], qb["Dimensions"]]

QuditBasisProp[qb_, "Tensor"] := ArrayReshape[Transpose[qb["Elements"], Cycles[{RotateRight @ Range[qb["Rank"] + 1, 1 , -1]}]], qb["TensorDimensions"]]

QuditBasisProp[qb_, "Matrix"] := ArrayReshape[qb["Tensor"], qb["MatrixDimensions"]]

QuditBasisProp[qb_, "Dual"] := QuditBasis[#["Dual"] & /@ qb["Names"], qb["BasisElements"]]

QuditBasisProp[qb_, "DualQ"] := AllTrue[qb["Names"], #["DualQ"] &]

QuditBasisProp[qb_, "Reverse"] := QuditBasis[Reverse @ qb["Names"], qb["BasisElements"]]

QuditBasisProp[qb_, "Canonical"] := QuditBasis[Sort @ qb["Names"], qb["BasisElements"]]

QuditBasisProp[qb_, "Uncurry"] := QuditBasis[KeyMap[QuditBasisName @* Row @* Map[#["Name"] &] @* Normal] @ qb["Association"]]


QuditBasisProp[qb_, {"Permute", perm_Cycles}] := Enclose @ QuditBasis[
    #[{"Permute", perm}] & /@ qb["Names"],
    KeyMap[MapAt[PermutationList[perm, qb["Rank"]][[#]] &, 2]] @ qb["BasisElements"]
]

QuditBasisProp[qb_, {"Ordered", qudits_Integer, order_ ? orderQ}] := If[qb["Dimension"] <= 1, qb,
    With[{arity = Max[qudits, Max[order]]},
        QuantumTensorProduct[qb, QuditBasis[2, arity - qb["Qudits"]]][{"Permute",
            InversePermutation @ FindPermutation[Join[order, Complement[Range[arity], order]]]}]
    ]
]

QuditBasisProp[qb_, "RemoveIdentities"] := QuditBasis[
    (QuditBasisName @@ Delete[Normal[#], Position[qb["Dimensions"], 1]])["Group"] & /@ qb["Names"],
    Select[qb["BasisElements"], TensorRank[#] > 0 &]
]


QuditBasis[_QuditBasis, 0] := QuditBasis[]

QuditBasis[dimension_Integer, multiplicity_Integer ? Positive] := QuditBasis[QuditBasis[dimension], multiplicity]

QuditBasis[qb_QuditBasis ? QuditBasisQ, multiplicity_Integer ? Positive] :=
    If[multiplicity > 1, QuantumTensorProduct[Table[qb, multiplicity]], qb]


(* tensor product of multiple parameter basis *)

QuditBasis[{name_String, params_List}, args___] := QuantumTensorProduct @@ (QuditBasis[{name, #}, args] & /@ params)

QuditBasis[params_List] := QuantumTensorProduct @@ (QuditBasis /@ params)

QuditBasis[name : _String | {_String, Except[_List]} | _Integer, multiplicity_Integer ? Positive, args___] :=
    QuditBasis[QuditBasis[name, args], multiplicity, args]


QuditBasis /: MakeBoxes[qb_QuditBasis /; QuditBasisQ[Unevaluated @ qb], format_] :=
    With[{boxes = ToBoxes[qb["Association"], format]}, InterpretationBox[boxes, qb]]

