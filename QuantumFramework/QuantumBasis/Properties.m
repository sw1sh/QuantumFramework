Package["QuantumFramework`"]

PackageScope["QuantumBasisProp"]



$QuantumBasisProperties = {
     "InputElements", "OutputElements", "BasisElementAssociation", "Association", "Elements",
     "InputBasisElementNames", "OutputBasisElementNames", "BasisElementNames",
     "NormalBasisElementNames",
     "InputBasisElements", "OutputBasisElements", "BasisElements",
     "InputBasisElementDimensions", "OutputBasisElementDimensions", "BasisElementDimensions", "BasisElementDimension",
     "MatrixElementDimensions",
     "OrthogonalBasisElements",
     "Projectors", "PureStates", "PureEffects", "PureMaps",
     "InputSize", "OutputSize", "Size",
     "InputRank", "OutputRank", "Rank", "Type",
     "InputNameDimensions", "InputDimensions", "InputNameDimension", "InputDimension",
     "OutputNameDimensions", "OutputDimensions", "OutputNameDimension", "OutputDimension",
     "NameDimensions", "Dimensions", "Dimension",
     "MatrixNameDimensions", "MatrixElementDimensions",
     "TensorDimensions", "MatrixDimensions",
     "InputTensor", "InputMatrix",
     "OutputTensor", "OutputMatrix",
     "TensorRepresentation", "Tensor", "MatrixRepresentation", "Matrix",
     "Qudits", "InputQudits", "OutputQudits", "InputBasis", "OutputBasis",
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


(* computed *)

QuantumBasisProp[qb_, "InputElements"] := qb["Input"](* /. <||> -> <|$BasisNameZero -> 0|>*)

QuantumBasisProp[qb_, "OutputElements"] := qb["Output"](* /. <||> -> <|$BasisNameZero -> 0|>*)

QuantumBasisProp[qb_, "InputBasisElementNames"] := simplifyBasisElementName /@ Keys @ qb["InputElements"]

QuantumBasisProp[qb_, "OutputBasisElementNames"] := simplifyBasisElementName /@ Keys @ qb["OutputElements"]

QuantumBasisProp[qb_, "InputBasisElements"] := Values @ qb["InputElements"]

QuantumBasisProp[qb_, "OutputBasisElements"] := Values @ qb["OutputElements"]


QuantumBasisProp[qb_, "BasisElementNames"] := prettyBasisElementName @* simplifyBasisElementName /@
    QuantumTensorProduct @@@ Tuples[{qb["OutputBasisElementNames"], qb["InputBasisElementNames"] /. Ket -> Bra}]

QuantumBasisProp[qb_, "BasisElements"] := kroneckerProduct @@@ Tuples[{qb["OutputBasisElements"], qb["InputBasisElements"]}]


QuantumBasisProp[qb_, "BasisElementAssociation" | "Association" | "Elements"] := AssociationThread[
    qb["BasisElementNames"],
    qb["BasisElements"]
]


QuantumBasisProp[qb_, "NormalBasisElementNames"] := normalBasisElementName /@ qb["BasisElementNames"]


QuantumBasisProp[qb_, "InputSize"] := Length[qb["InputElements"]]

QuantumBasisProp[qb_, "OutputSize"] := Length[qb["OutputElements"]]

QuantumBasisProp[qb_, "Size"] := qb["InputSize"] + qb["OutputSize"]


QuantumBasisProp[qb_, "InputBasisElementDimensions"] := Dimensions @ First[MaximalBy[qb["InputBasisElements"], ArrayDepth], 0]

QuantumBasisProp[qb_, "OutputBasisElementDimensions"] := Dimensions @ First[MaximalBy[qb["OutputBasisElements"], ArrayDepth], 0]

QuantumBasisProp[qb_, "BasisElementDimensions"] := Join[qb["OutputBasisElementDimensions"], qb["InputBasisElementDimensions"]]


QuantumBasisProp[qb_, "InputBasisElementDimension"] := Times @@ qb["InputBasisElementDimensions"]

QuantumBasisProp[qb_, "OutputBasisElementDimension"] := Times @@ qb["OutputBasisElementDimensions"]

QuantumBasisProp[qb_, "BasisElementDimension"] := Times @@ qb["BasisElementDimensions"]


QuantumBasisProp[qb_, "InputRank"] := Length[qb["InputBasisElementDimensions"]]

QuantumBasisProp[qb_, "OutputRank"] := Length[qb["OutputBasisElementDimensions"]]

QuantumBasisProp[qb_, "Rank"] := Length[qb["BasisElementDimensions"]]


QuantumBasisProp[qb_, "Type"] := Switch[qb["Rank"], 0, "Scalar", 1, "Vector", 2, "Matrix", _, "Tensor"]


QuantumBasisProp[qb_, "InputQudits"] := basisElementQudits @ First[qb["InputBasisElementNames"], $BasisNameZero]

QuantumBasisProp[qb_, "OutputQudits"] := basisElementQudits @ First[qb["OutputBasisElementNames"], $BasisNameZero]

QuantumBasisProp[qb_, "Qudits"] := qb["InputQudits"] + qb["OutputQudits"]


QuantumBasisProp[qb_, "InputNameDimensions" | "InputDimensions"] := basisElementNamesDimensions @ qb["InputBasisElementNames"]

QuantumBasisProp[qb_, "OutputNameDimensions" | "OutputDimensions"] := basisElementNamesDimensions @ qb["OutputBasisElementNames"]

QuantumBasisProp[qb_, "NameDimensions" | "Dimensions"] := DeleteCases[Join[qb["OutputNameDimensions"], qb["InputNameDimensions"]], 1]


QuantumBasisProp[qb_, "InputNameDimension" | "InputDimension"] := Times @@ qb["InputNameDimensions"]

QuantumBasisProp[qb_, "OutputNameDimension" | "OutputDimension"] := Times @@ qb["OutputNameDimensions"]

QuantumBasisProp[qb_, "NameDimension" | "Dimension"] := Times @@ qb["NameDimensions"]


QuantumBasisProp[qb_, "MatrixNameDimensions"] := {qb["OutputNameDimension"], qb["InputNameDimension"]}

QuantumBasisProp[qb_, "MatrixElementDimensions"] := {qb["OutputBasisElementDimension"], qb["InputBasisElementDimension"]}

QuantumBasisProp[qb_, "TensorDimensions"] := Join[qb["Dimensions"], qb["BasisElementDimensions"]]

QuantumBasisProp[qb_, "MatrixDimensions"] := {qb["Dimension"], qb["BasisElementDimension"]}


QuantumBasisProp[qb_, "InputBasis"] := QuantumBasis[qb, "Output" -> <|$BasisNameIdentity -> 1|>]

QuantumBasisProp[qb_, "OutputBasis"] := QuantumBasis[qb, "Input" -> <|$BasisNameIdentity -> 1|>]


QuantumBasisProp[qb_, "OrthogonalBasisElements"] := ArrayReshape[#, qb["BasisElementDimensions"]] & /@ (
    Orthogonalize[Flatten /@ qb["BasisElements"]]
)


QuantumBasisProp[qb_, "InputTensor"] := Transpose[qb["InputBasisElements"], Cycles[{Reverse @ RotateLeft @ Range[qb["InputRank"] + 1]}]]

QuantumBasisProp[qb_, "OutputTensor"] := Transpose[qb["OutputBasisElements"], Cycles[{Reverse @ RotateLeft @ Range[qb["OutputRank"] + 1]}]]

QuantumBasisProp[qb_, "InputMatrix"] := ArrayReshape[qb["InputTensor"], {qb["InputBasisElementDimension"], qb["InputSize"]}]

QuantumBasisProp[qb_, "OutputMatrix"] := ArrayReshape[qb["OutputTensor"], {qb["OutputBasisElementDimension"], qb["OutputSize"]}]

QuantumBasisProp[qb_, "TensorRepresentation" | "Tensor"] := ArrayReshape[qb["BasisElements"], qb["TensorDimensions"]]

QuantumBasisProp[qb_, "MatrixRepresentation" | "Matrix"] := ArrayReshape[qb["BasisElements"], qb["MatrixDimensions"]]


QuantumBasisProp[qb_, "Projectors"] := projector /@ qb["Matrix"]

QuantumBasisProp[qb_, "PureStates"] := QuantumDiscreteState[SparseArray[# -> 1, qb["OutputDimension"]], qb] & /@ Range[qb["OutputDimension"]]

QuantumBasisProp[qb_, "PureEffects"] := QuantumDiscreteState[SparseArray[# -> 1, qb["InputDimension"]], qb] & /@ Range[qb["InputDimension"]]

QuantumBasisProp[qb_, "PureMaps" | "PureOperators"] :=
    Table[
        QuantumDiscreteState[SparseArray[{i, j} -> 1, {qb["OutputDimension"], qb["InputDimension"]}], qb],
        {i, qb["OutputDimension"]}, {j, qb["InputDimension"]}
    ]

