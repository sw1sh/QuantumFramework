Package["QuantumFramework`"]

PackageScope["QuantumBasisProp"]



$QuantumBasisProperties = {
     "BasisElementNames", "BasisElements", "BasisElementAssociation",
     "Association",
     "BasisElementDimensions", "BasisElementDimension",
     "NormalizedBasisElements",
     "MatrixRepresentation",
     "Projectors",
     "Size", "Rank", "Type",
     "Dimensions", "Dimension",
     "Qudits", "InputQudits", "OutputQudits",
     "Picture"
};

QuantumBasis["Properties"] := DeleteDuplicates @ Join[$QuantumBasisProperties, $QuantumBasisDataKeys]

qb_QuantumBasis["ValidQ"] := quantumBasisQ[qb]


QuantumBasis::undefprop = "QuantumBasis property `` is undefined for this basis";

(qb_QuantumBasis[prop_ ? propQ, args___]) /; QuantumBasisQ[qb] := With[{
    result = QuantumBasisProp[qb, prop, args]
},
    (QuantumBasisProp[qb, prop, args] = result) /; !MatchQ[result, _QuantumBasisProp] || Message[QuantumBasis::undefprop, prop]
]


QuantumBasisProp[_, "Properties"] := QuantumBasis["Properties"]


(* getters *)

QuantumBasisProp[QuantumBasis[data_Association], key_] /; KeyExistsQ[data, key] := data[key]

QuantumBasisProp[qb_, "BasisElementAssociation" | "Association"] := qb["Elements"]


QuantumBasisProp[qb_, "BasisElementNames"] := Keys[qb["BasisElementAssociation"]]

QuantumBasisProp[qb_, "NormalBasisElementNames"] := normalBasisElementName /@ qb["BasisElementNames"]

QuantumBasisProp[qb_, "BasisElements"] := Values[qb["BasisElementAssociation"]]

QuantumBasisProp[qb_, "Size"] := Length[qb["BasisElements"]]

QuantumBasisProp[qb_, "BasisElementDimensions"] := Dimensions @ First @ MaximalBy[qb["BasisElements"], ArrayDepth]

QuantumBasisProp[qb_, "BasisElementDimension"] := Times @@ qb["BasisElementDimensions"]

QuantumBasisProp[qb_, "Rank"] := Length[qb["BasisElementDimensions"]]

QuantumBasisProp[qb_, "Type"] := Switch[qb["Rank"], 0, "Scalar", 1, "Vector", 2, "Matrix", _, "Tensor"]

QuantumBasisProp[qb_, "Qudits"] := basisElementNameLength @ First[qb["BasisElementNames"]]

QuantumBasisProp[qb_, "OutputQudits"] := qb["Qudits"] - qb["InputQudits"]

QuantumBasisProp[qb_, "Dimensions"] := basisElementNamesDimensions @qb["BasisElementNames"]

QuantumBasisProp[qb_, "Dimension"] := Times @@ qb["Dimensions"]


QuantumBasisProp[qb_, "NormalizedBasisElements"] /; qb["Rank"] == 1 := Orthogonalize[qb["BasisElements"]]

QuantumBasisProp[qb_, "NormalizedBasisElements"] /; qb["Rank"] >= 2 := TakeList[#, qb["BasisElementDimensions"]] & /@ (
    Orthogonalize[Flatten[#] & /@ qb["BasisElements"]]
)


QuantumBasisProp[qb_, "MatrixRepresentation"] /; qb["Rank"] == 0 := {{}}

QuantumBasisProp[qb_, "MatrixRepresentation"] /; qb["Rank"] == 1 := Total[
    Apply[TensorProduct, #] & /@ Transpose[{Orthogonalize[qb["BasisElements"]], IdentityMatrix[qb["Size"]]}]
]

QuantumBasisProp[qb_, "MatrixRepresentation"] := Transpose[Flatten[#] & /@ qb["BasisElements"]] /; qb["Rank"] >= 2


QuantumBasisProp[qb_, "Projectors"] := KroneckerProduct[ConjugateTranspose[#, Reverse @ Range @ TensorRank @ #], #] & /@ qb["BasisElements"]

