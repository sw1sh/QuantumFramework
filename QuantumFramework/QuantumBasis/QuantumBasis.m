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



$QuantumBasisDataKeys = {"Input", "Output", "Picture", "Label"}


quantumBasisQ[QuantumBasis[data_Association]] := Enclose[
Module[{
},
    ConfirmAssert[ContainsExactly[Keys[data], $QuantumBasisDataKeys], Message[QuantumBasis::wrongData]];

    ConfirmAssert[QuditBasisQ[data["Input"]]];
    ConfirmAssert[QuditBasisQ[data["Output"]]];

    ConfirmAssert[MemberQ[$QuantumBasisPictures, data["Picture"]], Message[QuantumBasis::picture]];
    (*ConfirmAssert[Length[inputElements] + Length[outputElements] > 0, Message[QuantumBasis::zeroDimension]];*)

    True
],
False &
]

quantumBasisQ[___] := False


QuantumBasisQ[qb : QuantumBasis[_Association]] := System`Private`ValidQ[qb]

QuantumBasisQ[___] := False


(* mutation *)

QuantumBasis[qb_QuantumBasis] := qb

QuantumBasis[QuantumBasis[data_Association], picture_String] := QuantumBasis[<|data, "Picture" -> picture|>]

QuantumBasis[QuantumBasis[data_Association], rules : _Rule ..] := QuantumBasis[<|data, rules|>]

QuantumBasis[data_Association, args__] := Fold[QuantumBasis, QuantumBasis[data], {args}]


(* construction *)

QuantumBasis[elements_Association ? (Not @* KeyExistsQ["Output"]), args___] := QuantumBasis[<|"Output" -> QuditBasis[elements]|>, args]

QuantumBasis[elements_ /; VectorQ[elements, TensorQ], args___] := QuantumBasis[
    AssociationThread[Range[0, Length[elements] - 1], elements],
    args
]


(* defaults *)
QuantumBasis[data_Association ? (Keys /* Not @* ContainsExactly[$QuantumBasisDataKeys]), args___] :=
    QuantumBasis[<|<|"Input" -> QuditBasis[], "Output" -> QuditBasis[], "Picture" -> "Schrodinger", "Label" -> None|>, data|>, args]


QuantumBasis[data_Association] /; AssociationQ[data["Input"]] := QuantumBasis[MapAt[QuditBasis, data, "Input"]]

QuantumBasis[data_Association] /; AssociationQ[data["Output"]] := QuantumBasis[MapAt[QuditBasis, data, "Output"]]


(* multiplicity *)

QuantumBasis[qb_ ? QuantumBasisQ, 1, args___] := QuantumBasis[qb, args]

QuantumBasis[qb_ ? QuantumBasisQ, multiplicity_Integer, args___] := QuantumBasis[qb,
    "Input" -> QuditBasis[qb["Input"], multiplicity],
    "Output" -> QuditBasis[qb["Output"], multiplicity],
    args
]


(* tensor product of multiple parameter basis *)
QuantumBasis[{name_String, params_List}, args___] := QuantumTensorProduct @@ (QuantumBasis[{name, #}, args] & /@ params)

QuantumBasis[params_List, args___] := QuantumTensorProduct @@ (QuantumBasis[#, args] & /@ params)


QuantumBasis[name : _String | {_String, Except[_List]}, multiplicity_Integer ? Positive, args___] :=
    QuantumBasis[QuantumBasis[name, args], multiplicity, args]


qb_QuantumBasis /; System`Private`HoldNotValidQ[qb] && quantumBasisQ[Unevaluated @ qb] := System`Private`HoldSetValid[qb]

