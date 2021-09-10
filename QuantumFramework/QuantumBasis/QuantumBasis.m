Package["QuantumFramework`"]

PackageExport["QuantumBasis"]

PackageScope["ValidQuantumBasisQ"]
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


ValidQuantumBasisQ[qb_[basisElements_Association, picture_String]] :=
    SymbolName[qb] === "QuantumBasis" &&
    (MemberQ[$QuantumBasisPictures, picture] || Message[QuantumBasis::picture]) &&
    (Length[basisElements] > 0 || Message[QuantumBasis::zeroDimensions]) &&
    (Equal @@ basisElementNameLength /@ Keys[basisElements] || Message[QuantumBasis::inconsistentNames]) &&
    (
        AnyTrue[basisElements, symbolicTensorQ] ||
        (
           Equal @@ Dimensions /@ basisElements ||
           Message[QuantumBasis::inconsistentElements]
        )
    ) &&
    (
        AllTrue[basisElements, ArrayQ[#, _ ? NumericQ] &] &&
        ResourceFunction["LinearlyIndependent"] @ Values[Flatten /@ basisElements] ||
        Message[QuantumBasis::dependent]
    )

ValidQuantumBasisQ[___] := False


qb_QuantumBasis["ValidQ"] := ValidQuantumBasisQ[qb]



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

expr : (qb_QuantumBasis[arg : prop_String | {prop_String, ___}, args___]) /; ValidQuantumBasisQ[qb] :=
    Enclose[
        ConfirmMatch[
            QuantumBasisProp[qb, arg, args],
            Except[_QuantumBasisProp],
            Message[QuantumBasis::undefprop, prop]
        ],
    Defer[expr] &
    ]

QuantumBasisProp[_, "Properties"] := QuantumBasis["Properties"]


(* getters *)

QuantumBasisProp[QuantumBasis[basisElements_, _], "BasisElementAssociation" | "Association"] := basisElements

QuantumBasisProp[QuantumBasis[_, picture_String], "Picture"] := picture


(* mutation *)

QuantumBasis[qb_ ? ValidQuantumBasisQ] := qb

QuantumBasis[qb_ ? ValidQuantumBasisQ, picture_String] := QuantumBasis[qb["Association"], picture]


(* construction *)

(* default basis *)
QuantumBasis[picture_String] := QuantumBasis["Computational", picture]

(* default picture *)
QuantumBasis[assoc_Association] := QuantumBasis[assoc, "Schrodinger"]


QuantumBasis[assoc_Association, args : Except[_String]] := Enclose @
    QuantumBasis[ConfirmBy[QuantumBasis[assoc], ValidQuantumBasisQ], args]


QuantumBasis[elements_ /; VectorQ[elements, TensorQ], args___] := QuantumBasis[
    AssociationThread[Ket[#] & /@ Range[0, Length[elements] - 1], elements],
    args
]


(* multiplicity *)

(* TODO: generalize to any rank *)

QuantumBasis[qb_ ? ValidQuantumBasisQ, 1] := qb

QuantumBasis[qb_ ? ValidQuantumBasisQ, multiplicity_Integer] /; multiplicity > 1 && qb["Rank"] == 1 := QuantumBasis[
    AssociationThread[
        CircleTimes @@ # & /@ Tuples[qb["BasisElementNames"], multiplicity],
        Flatten[Apply[KroneckerProduct, #]] & /@ Distribute[Table[qb["BasisElement"], multiplicity], List]
    ],
    qb["Picture"]
]

QuantumBasis[qb_ ? ValidQuantumBasisQ, multiplicity_Integer] /; multiplicity > 1 && qb["Rank"] == 2 := QuantumBasis[
    AssociationThread[
        CircleTimes @@ # & /@ Tuples[qb["BasisElementNames"], multiplicity],
        Flatten[Outer[KroneckerProduct, ##, 1] & @@ Table[qb["BasisElement"], multiplicity], multiplicity - 1]
    ],
    qb["Picture"]
]


(* tensor product of multiple parameter basis *)
QuantumBasis[{name_String, params_List}, args___] := QuantumTensorProduct @@ (QuantumBasis[{name, #}, args] & /@ params)

QuantumBasis[params_List, args___] := QuantumTensorProduct @@ (QuantumBasis[#, args] & /@ params)


QuantumBasis[name : _String | {_String, Except[_List]}, multiplicity_Integer ? Positive, args___] :=
    QuantumBasis[QuantumBasis[name, args], multiplicity, args]

