Package["QuantumFramework`"]

PackageScope["QuantumBasisProp"]



$QuantumBasisProperties = {
     "BasisElementAssociation", "Association", "Elements",
     "InputBasisElementNames", "OutputBasisElementNames", "BasisElementNames", "Names",
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
     "Dual", "Transpose", "ConjugateTranspose",
     "Picture"
};

QuantumBasis["Properties"] := DeleteDuplicates @ Join[$QuantumBasisProperties, $QuantumBasisDataKeys]

qb_QuantumBasis["ValidQ"] := quantumBasisQ[qb]


QuantumBasis::undefprop = "QuantumBasis property `` is undefined for this basis";

(qb_QuantumBasis[prop_ ? propQ, args___]) /; QuantumBasisQ[qb] := With[{
    result = QuantumBasisProp[qb, prop, args]
},
    (QuantumBasisProp[qb, prop, args] = result) /; !FailureQ[Unevaluated @ result] &&
    (!MatchQ[result, _QuantumBasisProp] || Message[QuantumBasis::undefprop, prop])
]


QuantumBasisProp[_, "Properties"] := QuantumBasis["Properties"]


(* getters *)

QuantumBasisProp[QuantumBasis[data_Association], key_] /; KeyExistsQ[data, key] := data[key]


(* computed *)


QuantumBasisProp[qb_, "InputBasisElementNames"] := qb["Input"]["Names"]

QuantumBasisProp[qb_, "OutputBasisElementNames"] := qb["Output"]["Names"]

QuantumBasisProp[qb_, "InputBasisElements"] := qb["Input"]["Elements"]

QuantumBasisProp[qb_, "OutputBasisElements"] := qb["Output"]["Elements"]


QuantumBasisProp[qb_, "BasisElementNames" | "Names"] :=
    QuantumTensorProduct @@@ Tuples[{qb["OutputBasisElementNames"], qb["InputBasisElementNames"]}]

QuantumBasisProp[qb_, "BasisElements" | "Elements"] := TensorProduct @@@ Tuples[{qb["OutputBasisElements"], qb["InputBasisElements"]}]


QuantumBasisProp[qb_, "BasisElementAssociation" | "Association" | "Elements"] := AssociationThread[
    qb["BasisElementNames"],
    qb["BasisElements"]
]


QuantumBasisProp[qb_, "NormalBasisElementNames"] := Normal /@ qb["BasisElementNames"]


QuantumBasisProp[qb_, "InputSize"] := qb["Input"]["Size"]

QuantumBasisProp[qb_, "OutputSize"] := qb["Output"]["Size"]

QuantumBasisProp[qb_, "Size"] := qb["InputSize"] + qb["OutputSize"]


QuantumBasisProp[qb_, "InputBasisElementDimensions"] := qb["Input"]["ElementDimensions"]

QuantumBasisProp[qb_, "OutputBasisElementDimensions"] := qb["Output"]["ElementDimensions"]

QuantumBasisProp[qb_, "BasisElementDimensions"] := Join[qb["OutputBasisElementDimensions"], qb["InputBasisElementDimensions"]]


QuantumBasisProp[qb_, "InputBasisElementDimension"] := Times @@ qb["InputBasisElementDimensions"]

QuantumBasisProp[qb_, "OutputBasisElementDimension"] := Times @@ qb["OutputBasisElementDimensions"]

QuantumBasisProp[qb_, "BasisElementDimension"] := Times @@ qb["BasisElementDimensions"]


QuantumBasisProp[qb_, "InputRank"] := Length[qb["InputBasisElementDimensions"]]

QuantumBasisProp[qb_, "OutputRank"] := Length[qb["OutputBasisElementDimensions"]]

QuantumBasisProp[qb_, "Rank"] := Length[qb["BasisElementDimensions"]]


QuantumBasisProp[qb_, "Type"] := Switch[qb["Rank"], 0, "Scalar", 1, "Vector", 2, "Matrix", _, "Tensor"]


QuantumBasisProp[qb_, "InputQudits"] := qb["Input"]["Qudits"]

QuantumBasisProp[qb_, "OutputQudits"] := qb["Output"]["Qudits"]

QuantumBasisProp[qb_, "Qudits"] := qb["InputQudits"] + qb["OutputQudits"]


QuantumBasisProp[qb_, "InputNameDimensions" | "InputDimensions"] := qb["Input"]["Dimensions"]

QuantumBasisProp[qb_, "OutputNameDimensions" | "OutputDimensions"] := qb["Output"]["Dimensions"]

QuantumBasisProp[qb_, "NameDimensions" | "Dimensions"] := DeleteCases[Join[qb["OutputNameDimensions"], qb["InputNameDimensions"]], 1]


QuantumBasisProp[qb_, "InputNameDimension" | "InputDimension"] := Times @@ qb["InputNameDimensions"]

QuantumBasisProp[qb_, "OutputNameDimension" | "OutputDimension"] := Times @@ qb["OutputNameDimensions"]

QuantumBasisProp[qb_, "NameDimension" | "Dimension"] := Times @@ qb["NameDimensions"]


QuantumBasisProp[qb_, "MatrixNameDimensions"] := {qb["OutputNameDimension"], qb["InputNameDimension"]}

QuantumBasisProp[qb_, "MatrixElementDimensions"] := {qb["OutputBasisElementDimension"], qb["InputBasisElementDimension"]}

QuantumBasisProp[qb_, "TensorDimensions"] := Join[qb["BasisElementDimensions"], qb["Dimensions"]]

QuantumBasisProp[qb_, "MatrixDimensions"] := {qb["BasisElementDimension"], qb["Dimension"]}


QuantumBasisProp[qb_, "InputBasis"] := QuantumBasis[qb, "Output" -> QuditBasis[]]

QuantumBasisProp[qb_, "OutputBasis"] := QuantumBasis[qb, "Input" -> QuditBasis[]]


QuantumBasisProp[qb_, "OrthogonalBasisElements"] := ArrayReshape[#, qb["BasisElementDimensions"]] & /@ (
    Orthogonalize[Flatten /@ qb["BasisElements"]]
)


QuantumBasisProp[qb_, "InputTensor"] := qb["Input"]["Tensor"]

QuantumBasisProp[qb_, "OutputTensor"] := qb["Output"]["Tensor"]

QuantumBasisProp[qb_, "InputMatrix"] := qb["Input"]["Matrix"]

QuantumBasisProp[qb_, "OutputMatrix"] := qb["Output"]["Matrix"]

QuantumBasisProp[qb_, "TensorRepresentation" | "Tensor"] := ArrayReshape[qb["Matrix"], qb["TensorDimensions"]]

QuantumBasisProp[qb_, "MatrixRepresentation" | "Matrix"] :=  KroneckerProduct[qb["OutputMatrix"], qb["InputMatrix"]]


QuantumBasisProp[qb_, "Projectors"] := projector /@ qb["Matrix"]

QuantumBasisProp[qb_, "PureStates"] := QuantumState[SparseArray[# -> 1, qb["OutputDimension"]], qb] & /@ Range[qb["OutputDimension"]]

QuantumBasisProp[qb_, "PureEffects"] := QuantumState[SparseArray[# -> 1, qb["InputDimension"]], qb] & /@ Range[qb["InputDimension"]]

QuantumBasisProp[qb_, "PureMaps" | "PureOperators"] :=
    Table[
        QuantumState[Flatten @ SparseArray[{i, j} -> 1, {qb["OutputDimension"], qb["InputDimension"]}], qb],
        {i, qb["OutputDimension"]}, {j, qb["InputDimension"]}
    ]

QuantumBasisProp[qb_, "Dual"] := QuantumBasis[qb, "Input" -> qb["Input"]["Dual"], "Output" -> qb["Output"]["Dual"]]

QuantumBasisProp[qb_, "Transpose"] := QuantumBasis[qb,
    "Input" -> qb["Output"], "Output" -> qb["Input"],
    "Label" -> Superscript[qb["Label"], "T"]
]

QuantumBasisProp[qb_, "Dagger" | "ConjugateTranspose"] := QuantumBasis[qb,
    "Input" -> qb["Output"]["Dual"], "Output" -> qb["Input"]["Dual"],
    "Label" -> Superscript[qb["Label"], "\[Dagger]"]
]

