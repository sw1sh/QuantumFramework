Package["QuantumFramework`"]

PackageExport["QuantumBasis"]

PackageScope["QuantumBasisQ"]
PackageScope["$QuantumBasisPictures"]
PackageScope["QuantumBasisProp"]



$QuantumBasisPictures = {
    "Schrodinger", "Schroedinger", "Schrödinger",
     "Heisenberg", "Interaction", "PhaseSpace"
};


QuantumBasis::picture = "should have one of the following pictures " <> StringRiffle[$QuantumBasisPictures, ", "];

QuantumBasis::zeroDimensions = "can't have zero dimension";

QuantumBasis::inconsistentNames = "element names should have the same length";

QuantumBasis::inconsistentElements = "elements should have the same dimensions";

QuantumBasis::dependent = "elements should be linearly independent";


QuantumBasisQ[QuantumBasis[basisElements_Association, picture_String]] :=
    (MemberQ[$QuantumBasisPictures, picture] || (Message[QuantumBasis::picture]; False)) &&
    (Length[basisElements] > 0 || (Message[QuantumBasis::zeroDimensions]; False)) &&
    (Equal @@ basisElementNameLength /@ Keys[basisElements] || (Message[QuantumBasis::inconsistentNames]; False)) &&
    (
        AnyTrue[basisElements, symbolicTensorQ] ||
        (
           Equal @@ Dimensions /@ basisElements ||
           (Message[QuantumBasis::inconsistentElements]; False)
        ) &&
        (
            AllTrue[basisElements, ArrayQ[#, _ ? NumericQ] &] &&
            ResourceFunction["LinearlyIndependent"] @ Values[Flatten /@ basisElements] ||
            (Message[QuantumBasis::dependent]; False)
        )
    )

QuantumBasisQ[___] := False


qb_QuantumBasis["ValidQ"] := QuantumBasisQ[qb]


qb : QuantumBasis[_Association, _String] := qb /; ! QuantumBasisQ[Unevaluated @ qb]


$QuantumBasisProperties = {
     "BasisElementNames", "BasisElements", "BasisElementAssociation",
     "Association",
     "BasisElementDimensions", "BasisElementDimension",
     "NormalizedBasisElements",
     "MatrixRepresentation",
     "Projectors",
     "Size", "Rank",
     "Dimensions", "Dimension", "Qudits",
     "Picture"
};

QuantumBasis["Properties"] := $QuantumBasisProperties


QuantumBasis::undefprop = "QuantumBasis property `` is undefined for this basis";

expr : (qb_QuantumBasis[prop_ ? propQ, args___]) /; QuantumBasisQ[qb] :=
    Enclose[
        ConfirmMatch[
            QuantumBasisProp[qb, prop, args],
            Except[_QuantumBasisProp],
            Message[QuantumBasis::undefprop, prop]
        ],
        Defer[expr] &
    ]


(* mutation *)

QuantumBasis[qb_ ? QuantumBasisQ] := qb

QuantumBasis[qb_ ? QuantumBasisQ, picture_String] := QuantumBasis[qb["Association"], picture]


(* construction *)

(* default basis *)
QuantumBasis[picture_String] := QuantumBasis["Computational", picture]

(* default picture *)
QuantumBasis[assoc_Association] := QuantumBasis[assoc, "Schrodinger"]


QuantumBasis[assoc_Association, args : Except[_String]] := Enclose @
    QuantumBasis[ConfirmBy[QuantumBasis[assoc], QuantumBasisQ], args]


QuantumBasis[elements_ /; VectorQ[elements, TensorQ], args___] := QuantumBasis[
    AssociationThread[Ket[#] & /@ Range[0, Length[elements] - 1], elements],
    args
]


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


(* tensor product of multiple parameter basis *)
QuantumBasis[{name_String, params_List}, args___] := QuantumTensorProduct @@ (QuantumBasis[{name, #}, args] & /@ params)

QuantumBasis[params_List, args___] := QuantumTensorProduct @@ (QuantumBasis[#, args] & /@ params)


QuantumBasis[name : _String | {_String, Except[_List]}, multiplicity_Integer ? Positive, args___] :=
    QuantumBasis[QuantumBasis[name, args], multiplicity, args]

