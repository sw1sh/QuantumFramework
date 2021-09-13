Package["QuantumFramework`"]

PackageExport["QuantumBasis"]

PackageScope["quantumBasisQ"]
PackageScope["QuantumBasisQ"]
PackageScope["$QuantumBasisDataKeys"]
PackageScope["$QuantumBasisPictures"]



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



$QuantumBasisDataKeys = {"Elements", "Picture", "InputQudits"}


quantumBasisQ[QuantumBasis[data_Association]] := Enclose[
Module[{
    basisElements, picture, inputQudits, nameLengths, numericElements
},
    ConfirmAssert[ContainsExactly[Keys[data], $QuantumBasisDataKeys], Message[QuantumBasis::wrongData]];

    basisElements = data["Elements"];
    picture = data["Picture"];
    inputQudits = data["InputQudits"];

    ConfirmAssert[MemberQ[$QuantumBasisPictures, picture], Message[QuantumBasis::picture]];
    ConfirmAssert[Length[basisElements] > 0, Message[QuantumBasis::zeroDimension]];

    nameLengths = basisElementNameLength /@ Keys[basisElements];

    ConfirmAssert[Equal @@ nameLengths, Message[QuantumBasis::inconsistentNames]];
    ConfirmAssert[inputQudits <= First[nameLengths], Message[QuantumBasis::inconsistentInputs, inputQudits, First[nameLengths]]];

    numericElements = Select[basisElements, TensorQ[#, NumericQ] &];

    ConfirmAssert[Equal @@ Dimensions /@ numericElements, Message[QuantumBasis::inconsistentElements]];
    ConfirmAssert[
        Length[numericElements] === 0 || ResourceFunction["LinearlyIndependent"] @ Values[Flatten /@ numericElements],
        Message[QuantumBasis::dependentElements]
    ];

    True
],
False &
]

quantumBasisQ[___] := False

QuantumBasisQ = System`Private`ValidQ;


(* mutation *)

QuantumBasis[qb_QuantumBasis] := qb

QuantumBasis[QuantumBasis[data_Association], picture_String] := QuantumBasis[<|data, "Picture" -> picture|>]

QuantumBasis[QuantumBasis[data_Association], rules : _Rule ..] := QuantumBasis[<|data, rules|>]

QuantumBasis[data_Association, args__] := QuantumBasis[QuantumBasis[data], args]


(* construction *)

QuantumBasis[elements_Association ? (Not @* KeyExistsQ["Elements"]), args___] := QuantumBasis[<|"Elements" -> elements|>, args]

QuantumBasis[elements_ /; VectorQ[elements, TensorQ], args___] := QuantumBasis[
    AssociationThread[Ket[#] & /@ Range[0, Length[elements] - 1], elements],
    args
]

(* defaults *)
QuantumBasis[data_Association ? (Keys /* Not @* ContainsExactly[$QuantumBasisDataKeys]), args___] :=
    QuantumBasis[<|<|"InputQudits" -> 0, "Picture" -> "Schrodinger"|>, data|>, args]



(* multiplicity *)

(* TODO: generalize to any rank *)

QuantumBasis[qb_ ? QuantumBasisQ, 1] := qb

QuantumBasis[qb_ ? QuantumBasisQ, multiplicity_Integer] /; multiplicity > 1 && qb["Rank"] == 1 := QuantumBasis[
    AssociationThread[
        CircleTimes @@ # & /@ Tuples[qb["BasisElementNames"], multiplicity],
        Flatten[Apply[KroneckerProduct, #]] & /@ Distribute[Table[qb["BasisElements"], multiplicity], List]
    ],
    qb["Picture"]
]

QuantumBasis[qb_ ? QuantumBasisQ, multiplicity_Integer] /; multiplicity > 1 && qb["Rank"] == 2 := QuantumBasis[
    AssociationThread[
        CircleTimes @@ # & /@ Tuples[qb["BasisElementNames"], multiplicity],
        Flatten[Outer[KroneckerProduct, ##, 1] & @@ Table[qb["BasisElements"], multiplicity], multiplicity - 1]
    ],
    qb["Picture"]
]


(* rename *)

QuantumBasis[qb : QuantumBasis[data_Association], names_List, args___] /; Length[names] === qb["Size"] :=
    QuantumBasis[QuantumBasis[<|data, "Elements" -> AssociationThread[names, qb["BasisElements"]]|>], args]


(* tensor product of multiple parameter basis *)
QuantumBasis[{name_String, params_List}, args___] := QuantumTensorProduct @@ (QuantumBasis[{name, #}, args] & /@ params)

QuantumBasis[params_List, args___] := QuantumTensorProduct @@ (QuantumBasis[#, args] & /@ params)


QuantumBasis[name : _String | {_String, Except[_List]}, multiplicity_Integer ? Positive, args___] :=
    QuantumBasis[QuantumBasis[name, args], multiplicity, args]


qb_QuantumBasis /; System`Private`HoldNotValidQ[qb] && quantumBasisQ[Unevaluated @ qb] := System`Private`HoldSetValid[qb]


