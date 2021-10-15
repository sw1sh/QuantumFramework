Package["Wolfram`QuantumFramework`"]

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

QuantumBasis[picture : Alternatives @@ $QuantumBasisPictures] := QuantumBasis["Computational", picture]

QuantumBasis[QuantumBasis[data_Association], picture : Alternatives @@ $QuantumBasisPictures] := QuantumBasis[<|data, "Picture" -> picture|>]

QuantumBasis[QuantumBasis[data_Association], rules : _Rule ..] := QuantumBasis[<|data, rules|>]

QuantumBasis[data_Association, args__] := Fold[QuantumBasis, QuantumBasis[data], {args}]


(* construction *)

QuantumBasis[elements_Association ? (Not @* KeyExistsQ["Output"]), args___] := QuantumBasis[<|"Output" -> QuditBasis[elements]|>, args]

QuantumBasis[output : _QuditBasis | _List, args___] := QuantumBasis["Output" -> QuditBasis[output], args]

QuantumBasis[output : _QuditBasis | _List, input : _QuditBasis | _List | _Integer, args___] :=
    QuantumBasis["Output" -> QuditBasis[output], "Input" -> QuditBasis[input]["Dual"], args]


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

QuantumBasis[param : name_String | {name_String, ___}, args___] :=
    QuantumBasis[<|"Output" -> QuditBasis[param], "Label" -> StringDelete[name, "Basis"]|>, args]

QuantumBasis[param : _ ? nameQ | _Integer, args___] :=
    QuantumBasis[<|"Output" -> QuditBasis[param]|>, args]

QuantumBasis[args : (_String ? (MatchQ[Alternatives @@ $QuantumBasisPictures]) | _Rule) ...] := QuantumBasis["Computational", args]


qb_QuantumBasis /; System`Private`HoldNotValidQ[qb] && quantumBasisQ[Unevaluated @ qb] := System`Private`HoldSetValid[qb]


(* equality *)

QuantumBasis /: (qb1_QuantumBasis ? QuantumBasisQ) ==
    (qb2_QuantumBasis ? QuantumBasisQ) := qb1["MatrixRepresentation"] == qb2["MatrixRepresentation"]

