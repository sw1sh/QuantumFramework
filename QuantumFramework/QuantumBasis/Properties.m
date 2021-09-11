Package["QuantumFramework`"]



QuantumBasisProp[_, "Properties"] := QuantumBasis["Properties"]


(* getters *)

QuantumBasisProp[QuantumBasis[basisElements_, _], "BasisElementAssociation" | "Association"] := basisElements

QuantumBasisProp[QuantumBasis[_, picture_String], "Picture"] := picture


QuantumBasisProp[qb_, "BasisElementNames"] := Keys[qb["BasisElementAssociation"]]

QuantumBasisProp[qb_, "NormalBasisElementNames"] := normalBasisElementName /@ qb["BasisElementNames"]

QuantumBasisProp[qb_, "BasisElements"] := Values[qb["BasisElementAssociation"]]

QuantumBasisProp[qb_, "Size"] := Length[qb["BasisElements"]]

QuantumBasisProp[qb_, "BasisElementDimensions"] := DeleteCases[First @ MaximalBy[Dimensions /@ qb["BasisElements"], Length], 0]

QuantumBasisProp[qb_, "BasisElementDimension"] := Times @@ qb["BasisElementDimensions"]

QuantumBasisProp[qb_, "Rank"] := Length[qb["BasisElementDimensions"]]

QuantumBasisProp[qb_, "Qudits"] := basisElementNameLength @ First[qb["BasisElementNames"]]


QuantumBasisProp[qb_, "Dimensions"] := CountDistinct /@ Transpose[normalBasisElementName /@ qb["BasisElementNames"]]

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

