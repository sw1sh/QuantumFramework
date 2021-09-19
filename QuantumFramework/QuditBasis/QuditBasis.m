Package["QuantumFramework`"]

PackageExport["QuditBasis"]
PackageExport["QuditBasisName"]

PackageScope["QuditBasisQ"]


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

QuditBasis[elements_Association] /; Not @ AllTrue[Keys[elements], MatchQ[_QuditBasisName]] :=
    QuditBasis[KeyMap[QuditBasisName, elements]]

qb_QuditBasis /; System`Private`HoldNotValidQ[qb] && quditBasisQ[Unevaluated @ qb] := System`Private`HoldSetValid[qb]


QuditBasis[elements_Association]["Names"] := Keys @ elements

QuditBasis[elements_Association]["Elements"] := Values @ elements

(qb_QuditBasis ? QuditBasisQ)["Qudits"] := If[Length[qb["Names"]] > 0, First[qb["Names"]]["Qudits"], 0]

(qb_QuditBasis ? QuditBasisQ)["Dimensions"] := CountDistinct /@ Transpose[Normal /@ qb["Names"]]


QuditBasis[qb_ ? QuditBasisQ, multiplicity_Integer] := QuditBasis @ AssociationThread[
    QuantumTensorProduct @@@ Tuples[qb["Names"], multiplicity],
    kroneckerProduct @@@ Tuples[qb["Elements"], multiplicity]
]


QuditBasis /: MakeBoxes[qb : QuditBasis[elements_] /; QuditBasisQ[Unevaluated @ qb], format_] :=
    ToBoxes[elements, format]

