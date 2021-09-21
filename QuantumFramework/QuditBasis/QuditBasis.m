Package["QuantumFramework`"]

PackageExport["QuditBasis"]
PackageExport["QuditBasisName"]

PackageScope["QuditBasisQ"]



QuditBasis["Properties"] = {"Association", "Names", "Elements", "Qudits", "Dimensions", "Dual"}



QuditBasis::inconsistentNames = "element names should have the same length";

QuditBasis::inconsistentElements = "elements should have the same dimensions";

QuditBasis::dependentElements = "elements should be linearly independent";



quditBasisQ[QuditBasis[names_List, elements_List]] := Enclose[
Module[{
    elementQudits, numericElements
},
    ConfirmAssert[AllTrue[names, MatchQ[_QuditBasisName]]];
    elementQudits = #["Qudits"] & /@ names;


    ConfirmAssert[Equal @@ elementQudits, Message[QuditBasis::inconsistentNames]];

    numericElements = Select[elements, TensorQ[#, NumericQ] &];

    ConfirmAssert[
        Equal @@ Dimensions /@ numericElements,
        Message[QuditBasis::inconsistentElements]
    ];
    ConfirmAssert[
        (Length[numericElements] === 0 || ResourceFunction["LinearlyIndependent"] @ (Flatten /@ numericElements)),
        Message[QuditBasis::dependentElements]
    ];

    True
],
False &
]

quditBasisQ[___] := False


QuditBasisQ[qb : QuditBasis[_, _]] := System`Private`ValidQ[qb]

QuditBasisQ[___] := False


QuditBasis[] := QuditBasis[{QuditBasisName[]}, {1}]

QuditBasis[qb_QuditBasis] := qb

(*QuditBasis[elements_ /; VectorQ[elements, TensorQ], args___] := QuditBasis[
    QuditBasis[Range[0, Length[elements] - 1], elements],
    args
]*)

QuditBasis[assoc_Association, args___] := QuditBasis[Keys[assoc], Values[assoc], args]

QuditBasis[names_List, elements_] /; Not @ AllTrue[names, MatchQ[_QuditBasisName]] :=
    QuditBasis[Map[QuditBasisName, names], elements]

qb_QuditBasis /; System`Private`HoldNotValidQ[qb] && quditBasisQ[Unevaluated @ qb] := System`Private`HoldSetValid[qb]


QuditBasis::undefprop = "QuditBasis property `` is undefined for this basis";

(qb_QuditBasis[prop_ ? propQ, args___]) /; QuditBasisQ[qb] := With[{
    result = QuditBasisProp[qb, prop, args]
},
    (QuditBasisProp[qb, prop, args] = result) /; !FailureQ[Unevaluated @ result] &&
    (!MatchQ[result, _QuditBasisProp] || Message[QuditBasis::undefprop, prop])
]


QuditBasisProp[_, "Properties"] := QuditBasis["Properties"]

QuditBasisProp[QuditBasis[names_, elements_], "Association"] := AssociationThread[names, elements]

QuditBasisProp[QuditBasis[names_, _], "Names"] := names

QuditBasisProp[QuditBasis[_, elements_], "Elements"] := elements

QuditBasisProp[qb_, "Qudits"] := If[Length[qb["Names"]] > 0, First[qb["Names"]]["Qudits"], 0]

QuditBasisProp[qb_, "Dimensions"] := CountDistinct /@ Transpose[Normal /@ qb["Names"]]

QuditBasisProp[qb_, "Dimension"] := Times @@ qb["Dimensions"]

QuditBasisProp[qb_, "Rank"] := Length @ qb["Dimensions"]

QuditBasisProp[qb_, "Dual"] := QuditBasis[#["Dual"] & /@ qb["Names"], qb["Elements"]]

QuditBasisProp[qb_, "Reverse"] := QuditBasis[Reverse @ qb["Names"], Reverse @ qb["Elements"]]

QuditBasisProp[qb_, {"Permute", perm_Cycles}] := Enclose @ QuditBasis[
    #[{"Permute", perm}] & /@ qb["Names"],
    Transpose[If[TensorRank[#] === qb["Rank"], #, Confirm @ ArrayReshape[#, qb["Dimensions"]]], perm] & /@ qb["Elements"]
]

QuditBasisProp[qb_, {"Ordered", qudits_Integer, order_ ? orderQ}] := If[qb["Dimension"] <= 1, qb,
    With[{arity = Max[qudits, Max[order]]},
        QuantumTensorProduct[qb, QuditBasis[2, arity - qb["Qudits"]]][{"Permute",
            InversePermutation @ FindPermutation[Join[order, Complement[Range[arity], order]]]}]
    ]
]

QuditBasisProp[qb_, "RemoveIdentities"] := QuditBasis[
    (QuditBasisName @@ Delete[Normal[#], Position[qb["Dimensions"], 1]])["Group"] & /@ qb["Names"],
    qb["Elements"]
]


QuantumTensorProduct[qb1_QuditBasis, qb2_QuditBasis] := Enclose @ QuditBasis[
    QuantumTensorProduct @@@ Tuples[{ConfirmBy[qb1, QuditBasisQ]["Names"], ConfirmBy[qb2, QuditBasisQ]["Names"]}],
    TensorProduct @@@ Tuples[{qb1["Elements"], qb2["Elements"]}]
]["RemoveIdentities"]


QuditBasis[_QuditBasis, 0] := QuditBasis[]

QuditBasis[qb_QuditBasis ? QuditBasisQ, multiplicity_Integer ? Positive] :=
    If[multiplicity > 1, QuantumTensorProduct[Table[qb, multiplicity]], qb]


(* tensor product of multiple parameter basis *)

QuditBasis[{name_String, params_List}, args___] := QuantumTensorProduct @@ (QuditBasis[{name, #}, args] & /@ params)

QuditBasis[params_List] := QuantumTensorProduct @@ (QuditBasis /@ params)

QuditBasis[name : _String | {_String, Except[_List]} | _Integer, multiplicity_Integer ? Positive, args___] :=
    QuditBasis[QuditBasis[name, args], multiplicity, args]


QuditBasis /: MakeBoxes[qb_QuditBasis /; QuditBasisQ[Unevaluated @ qb], format_] :=
    ToBoxes[qb["Association"], format]

