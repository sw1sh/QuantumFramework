Package["Wolfram`QuantumFramework`"]

PackageScope["QuantumBasisProp"]



$QuantumBasisProperties = {
     "ElementAssociation", "Association", "Elements",
     "InputElementNames", "OutputElementNames", "ElementNames", "Names",
     "NormalElementNames",
     "InputElements", "OutputElements",
     "InputElementDimensions", "OutputElementDimensions", "ElementDimensions", "ElementDimension",
     "MatrixElementDimensions",
     "OrthogonalElements",
     "Projectors", "PureStates", "PureEffects", "PureMaps",
     "InputSize", "OutputSize", "Size",
     "InputRank", "OutputRank", "Rank", "Type",
     "InputNameDimensions", "InputDimensions", "InputNameDimension", "InputDimension",
     "OutputNameDimensions", "OutputDimensions", "OutputNameDimension", "OutputDimension",
     "NameDimensions", "Dimensions", "Dimension",
     "OutputBasis", "InputBasis", "QuditBasis", "HasInputQ",
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
    If[TrueQ[$QuantumFrameworkPropCache], QuantumBasisProp[qb, prop, args] = result, result] /; !FailureQ[Unevaluated @ result] &&
    (!MatchQ[result, _QuantumBasisProp] || Message[QuantumBasis::undefprop, prop])
]


QuantumBasisProp[_, "Properties"] := QuantumBasis["Properties"]


(* getters *)

QuantumBasisProp[QuantumBasis[data_Association], key_] /; KeyExistsQ[data, key] := data[key]


(* computed *)


QuantumBasisProp[qb_, "InputElementNames"] := qb["Input"]["Names"]

QuantumBasisProp[qb_, "OutputElementNames"] := qb["Output"]["Names"]

QuantumBasisProp[qb_, "InputElements"] := qb["Input"]["Elements"]

QuantumBasisProp[qb_, "OutputElements"] := qb["Output"]["Elements"]


QuantumBasisProp[qb_, "ElementNames" | "Names"] :=
    QuantumTensorProduct @@@ Tuples[{qb["OutputElementNames"], qb["InputElementNames"]}]

QuantumBasisProp[qb_, "Elements"] := ArrayReshape[Transpose[
    TensorProduct[qb["OutputElements"], qb["InputElements"]],
    FindPermutation[Join[{1, qb["OutputRank"] + 2}, Range[qb["OutputRank"]] + 1]]
],
    Prepend[qb["ElementDimensions"], qb["OutputSize"] qb["InputSize"]]
]


QuantumBasisProp[qb_, "ElementAssociation" | "Association"] := Association @ Thread[
    qb["ElementNames"] -> qb["Elements"]
]


QuantumBasisProp[qb_, "NormalElementNames"] := Normal /@ qb["ElementNames"]


QuantumBasisProp[qb_, "InputSize"] := qb["Input"]["Size"]

QuantumBasisProp[qb_, "OutputSize"] := qb["Output"]["Size"]

QuantumBasisProp[qb_, "Size"] := qb["InputSize"] + qb["OutputSize"]


QuantumBasisProp[qb_, "InputElementDimensions"] := qb["Input"]["ElementDimensions"]

QuantumBasisProp[qb_, "OutputElementDimensions"] := qb["Output"]["ElementDimensions"]

QuantumBasisProp[qb_, "ElementDimensions"] := Join[qb["OutputElementDimensions"], qb["InputElementDimensions"]]


QuantumBasisProp[qb_, "InputElementDimension"] := qb["Input"]["ElementDimension"]

QuantumBasisProp[qb_, "OutputElementDimension"] := qb["Output"]["ElementDimension"]

QuantumBasisProp[qb_, "ElementDimension"] := qb["OutputElementDimension"] qb["InputElementDimension"]


QuantumBasisProp[qb_, "InputRank"] := qb["Input"]["Rank"]

QuantumBasisProp[qb_, "OutputRank"] := qb["Output"]["Rank"]

QuantumBasisProp[qb_, "Rank"] := qb["OutputRank"] + qb["InputRank"]


QuantumBasisProp[qb_, "Type"] := Switch[qb["Rank"], 0, "Scalar", 1, "Vector", 2, "Matrix", _, "Tensor"]


QuantumBasisProp[qb_, "InputQudits"] := qb["Input"]["Qudits"]

QuantumBasisProp[qb_, "OutputQudits"] := qb["Output"]["Qudits"]

QuantumBasisProp[qb_, "Qudits"] := qb["InputQudits"] + qb["OutputQudits"]


QuantumBasisProp[qb_, "InputNameDimensions" | "InputDimensions"] := qb["Input"]["Dimensions"]

QuantumBasisProp[qb_, "OutputNameDimensions" | "OutputDimensions"] := qb["Output"]["Dimensions"]

QuantumBasisProp[qb_, "NameDimensions" | "Dimensions"] := DeleteCases[Join[qb["OutputNameDimensions"], qb["InputNameDimensions"]], 1]


QuantumBasisProp[qb_, "InputNameDimension" | "InputDimension"] := qb["Input"]["Dimension"]

QuantumBasisProp[qb_, "OutputNameDimension" | "OutputDimension"] := qb["Output"]["Dimension"]

QuantumBasisProp[qb_, "NameDimension" | "Dimension"] := qb["OutputDimension"] qb["InputDimension"]


QuantumBasisProp[qb_, "MatrixNameDimensions"] := {qb["OutputNameDimension"], qb["InputNameDimension"]}

QuantumBasisProp[qb_, "MatrixElementDimensions"] := {qb["OutputElementDimension"], qb["InputElementDimension"]}

QuantumBasisProp[qb_, "TensorDimensions"] := Join[qb["ElementDimensions"], qb["Dimensions"]]

QuantumBasisProp[qb_, "MatrixDimensions"] := {qb["ElementDimension"], qb["Dimension"]}


QuantumBasisProp[qb_, "InputBasis"] := QuantumBasis[qb, "Output" -> QuditBasis[]]

QuantumBasisProp[qb_, "OutputBasis"] := QuantumBasis[qb, "Input" -> QuditBasis[]]

QuantumBasisProp[qb_, "QuditBasis"] := QuantumTensorProduct[qb["Output"], qb["Input"]]

QuantumBasisProp[qb_, "HasInputQ"] := qb["InputDimension"] > 1


QuantumBasisProp[qb_, "OrthogonalElements"] := ArrayReshape[#, qb["ElementDimensions"]] & /@ (
    Orthogonalize[Flatten /@ qb["Elements"]]
)


QuantumBasisProp[qb_, "InputTensor"] := qb["Input"]["Tensor"]

QuantumBasisProp[qb_, "OutputTensor"] := qb["Output"]["Tensor"]

QuantumBasisProp[qb_, "InputMatrix"] := qb["Input"]["Matrix"]

QuantumBasisProp[qb_, "OutputMatrix"] := qb["Output"]["Matrix"]

QuantumBasisProp[qb_, "TensorRepresentation" | "Tensor"] := ArrayReshape[qb["Matrix"], qb["TensorDimensions"]]

QuantumBasisProp[qb_, "MatrixRepresentation" | "Matrix"] := KroneckerProduct[qb["OutputMatrix"], qb["InputMatrix"]]


QuantumBasisProp[qb_, "Projectors"] := projector @* Flatten /@ qb["Elements"]

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

QuantumBasisProp[qb_, {"Permute", perm_Cycles}] :=
    QuantumBasis @@ QuantumTensorProduct[qb["Output"], qb["Input"]][{"Permute", perm}][
        {"Split", toggleShift[PermutationList[perm, qb["Qudits"]], qb["OutputQudits"]]}
    ]

QuantumBasisProp[qb_, {"Split", n_Integer}] := QuantumBasis @@ QuantumTensorProduct[qb["Output"], qb["Input"]][{"Split", n}]


QuantumBasisProp[qb_, "Dagger" | "ConjugateTranspose"] := QuantumBasis[qb,
    "Input" -> qb["Output"]["Dual"], "Output" -> qb["Input"]["Dual"],
    "Label" -> Superscript[qb["Label"], "\[Dagger]"]
]

QuantumBasisProp[qb_, {"PermuteInput", perm_Cycles}] := QuantumBasis[qb, "Input" -> qb["Input"][{"Permute", perm}]]

QuantumBasisProp[qb_, {"PermuteOutput", perm_Cycles}] := QuantumBasis[qb, "Output" -> qb["Output"][{"Permute", perm}]]

QuantumBasisProp[qb_, "SortedQ"] := qb["Output"]["SortedQ"] && qb["Input"]["SortedQ"]

QuantumBasisProp[qb_, "Sort"] := QuantumBasis[qb, "Output" -> qb["Output"]["Sort"], "Input" -> qb["Input"]["Sort"]]

QuantumBasisProp[qb_, "Numeric"] := QuantumBasis[qb, "Output" -> qb["Output"]["Numeric"], "Input" -> qb["Input"]["Numeric"]]

