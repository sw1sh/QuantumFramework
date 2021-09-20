Package["QuantumFramework`"]

PackageExport["QuditBasis"]
PackageExport["QuditBasisName"]

PackageScope["QuditBasisQ"]



QuditBasis["Properties"] = {"Association", "Names", "Elements", "Qudits", "Dimensions", "Dual"}



QuditBasis::inconsistentNames = "element names should have the same length";

QuditBasis::inconsistentElements = "elements should have the same dimensions";

QuditBasis::dependentElements = "elements should be linearly independent";



quditBasisQ[QuditBasis[elements_Association]] := Enclose[
Module[{
    elementQudits, numericElements
},
    ConfirmAssert[AllTrue[Keys[elements], MatchQ[_QuditBasisName]]];
    elementQudits = #["Qudits"] & /@ Keys[elements];


    ConfirmAssert[Equal @@ elementQudits, Message[QuditBasis::inconsistentNames]];

    numericElements = Select[elements, TensorQ[#, NumericQ] &];

    ConfirmAssert[
        Equal @@ Dimensions /@ numericElements,
        Message[QuditBasis::inconsistentElements]
    ];
    ConfirmAssert[
        (Length[numericElements] === 0 || ResourceFunction["LinearlyIndependent"] @ Values[Flatten /@ numericElements]),
        Message[QuditBasis::dependentElements]
    ];

    True
],
False &
]

quditBasisQ[___] := False


QuditBasisQ[qb : QuditBasis[_Association]] := System`Private`ValidQ[qb]

QuditBasisQ[___] := False


QuditBasis[] := QuditBasis[<|QuditBasisName[] -> 1|>]

QuditBasis[qb_QuditBasis] := qb

QuditBasis[elements_ /; VectorQ[elements, TensorQ], args___] := QuditBasis[
    QuditBasis[AssociationThread[Range[0, Length[elements] - 1], elements]],
    args
]

QuditBasis[elements_Association] /; Not @ AllTrue[Keys[elements], MatchQ[_QuditBasisName]] :=
    QuditBasis[KeyMap[QuditBasisName, elements]]

qb_QuditBasis /; System`Private`HoldNotValidQ[qb] && quditBasisQ[Unevaluated @ qb] := System`Private`HoldSetValid[qb]


QuditBasis::undefprop = "QuditBasis property `` is undefined for this basis";

(qb_QuditBasis[prop_ ? propQ, args___]) /; QuditBasisQ[qb] := With[{
    result = QuditBasisProp[qb, prop, args]
},
    (QuditBasisProp[qb, prop, args] = result) /; !FailureQ[Unevaluated @ result] &&
    (!MatchQ[result, _QuditBasisProp] || Message[QuditBasis::undefprop, prop])
]


QuditBasisProp[_, "Properties"] := QuditBasis["Properties"]

QuditBasisProp[QuditBasis[elements_Association], "Association"] := elements

QuditBasisProp[QuditBasis[elements_Association], "Names"] := Keys @ elements

QuditBasisProp[QuditBasis[elements_Association], "Elements"] := Values @ elements

QuditBasisProp[qb_, "Qudits"] := If[Length[qb["Names"]] > 0, First[qb["Names"]]["Qudits"], 0]

QuditBasisProp[qb_, "Dimensions"] := CountDistinct /@ Transpose[Normal /@ qb["Names"]]

QuditBasisProp[qb_, "Dimension"] := Times @@ qb["Dimensions"]

QuditBasisProp[qb_, "Rank"] := Length @ qb["Dimensions"]

QuditBasisProp[qb_, "Dual"] := QuditBasis[AssociationThread[#["Dual"] & /@ qb["Names"], qb["Elements"]]]

QuditBasisProp[qb_, {"Permute", perm_Cycles}] := Enclose @ QuditBasis[AssociationThread[
    #[{"Permute", perm}] & /@ qb["Names"],
    Transpose[If[TensorRank[#] === qb["Rank"], #, Confirm @ ArrayReshape[#, qb["Dimensions"]]], perm] & /@ qb["Elements"]
]]

QuditBasisProp[qb_, {"Ordered", qudits_Integer, order_ ? orderQ}] := If[qb["Dimension"] <= 1, qb,
    With[{arity = Max[qudits, Max[order]]},
        QuantumTensorProduct[qb, QuditBasis[2, arity - qb["Qudits"]]][{"Permute",
            InversePermutation @ FindPermutation[Join[order, Complement[Range[arity], order]]]}]
    ]
]


QuantumTensorProduct[qb1_QuditBasis, qb2_QuditBasis] := Enclose @ QuditBasis @ AssociationThread[
    QuantumTensorProduct @@@ Tuples[{ConfirmBy[qb1, QuditBasisQ]["Names"], ConfirmBy[qb2, QuditBasisQ]["Names"]}],
    TensorProduct @@@ Tuples[{qb1["Elements"], qb2["Elements"]}]
]


QuditBasis[_QuditBasis, 0] := QuditBasis[]

QuditBasis[qb_QuditBasis ? QuditBasisQ, multiplicity_Integer ? Positive] :=
    If[multiplicity > 1, QuantumTensorProduct[Table[qb, multiplicity]], qb]


(* tensor product of multiple parameter basis *)

QuditBasis[{name_String, params_List}, args___] := QuantumTensorProduct @@ (QuditBasis[{name, #}, args] & /@ params)

QuditBasis[params_List, args___] := QuantumTensorProduct @@ (QuditBasis[#, args] & /@ params)

QuditBasis[name : _String | {_String, Except[_List]}, multiplicity_Integer ? Positive, args___] :=
    QuditBasis[QuditBasis[name, args], multiplicity, args]


QuditBasis /: MakeBoxes[qb : QuditBasis[elements_] /; QuditBasisQ[Unevaluated @ qb], format_] :=
    ToBoxes[elements, format]

