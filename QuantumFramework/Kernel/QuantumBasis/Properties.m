Package["Wolfram`QuantumFramework`"]



$QuantumBasisProperties = {
    "ElementAssociation", "Association", "Elements",
    "InputElementNames", "OutputElementNames", "ElementNames", "Names",
    "NormalElementNames",
    "InputElements", "OutputElements",
    "InputElementDimensions", "OutputElementDimensions", "InputElementDimension", "OutputElementDimension",
    "ElementDimensions", "ElementDimension",
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
    "Qudits", "InputQudits", "OutputQudits",
    "FullQudits", "FullInputQudits", "FullOutputQudits",
    "InputBasis", "OutputBasis",
    "Dual", "Transpose", "ConjugateTranspose",
    "Label", "LabelHead",
    "Picture",
    "Diagram",
    "ParameterArity", "Parameters", "InitialParameters", "FinalParameters"
};

QuantumBasis["Properties"] := Union @ Join[$QuantumBasisProperties, $QuantumBasisDataKeys]

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

QuantumBasisProp[QuantumBasis[data_Association], "Meta"] := Normal @ data[[Key /@ {"Picture", "Label", "ParameterSpec"}]]

(* computed *)

QuantumBasisProp[qb_, "LabelHead"] := NestWhile[Head, qb["Label"], MatchQ[Except[_String | _Symbol]]]

QuantumBasisProp[qb_, "QuditBasis"] := QuantumTensorProduct[qb["Output"], qb["Input"]]

QuantumBasisProp[qb_, "Representations"] := qb["QuditBasis"]["Representations"]

QuantumBasisProp[qb_, "InputElementNames", pos_ : All] := qb["Input"]["Names", pos]

QuantumBasisProp[qb_, "OutputElementNames", pos_ : All] := qb["Output"]["Names", pos]

QuantumBasisProp[qb_, "InputElements"] := ArrayReshape[qb["Input"]["Elements"], DeleteCases[qb["Input"]["ElementsDimensions"], 1]]

QuantumBasisProp[qb_, "OutputElements"] := ArrayReshape[qb["Output"]["Elements"], DeleteCases[qb["Output"]["ElementsDimensions"], 1]]


QuantumBasisProp[qb_, "ElementNames" | "Names"] :=
    QuantumTensorProduct @@@ Tuples[{qb["OutputElementNames"], qb["InputElementNames"]}]

QuantumBasisProp[qb_, "ElementNames" | "Names", pos : {{_Integer ..} ...}] :=
    MapThread[QuantumTensorProduct, {qb["OutputElementNames", pos[[All, 1]]], qb["InputElementNames", pos[[All, 2]]]}]

QuantumBasisProp[qb_, "ElementNames" | "Names", outPos : {_Integer ..} : All, inPos : {_Integer ..} : All] :=
    QuantumTensorProduct @@@ Tuples[{qb["OutputElementNames", outPos], qb["InputElementNames", inPos]}]

QuantumBasisProp[qb_, "Elements"] := ArrayReshape[
    If[ qb["InputRank"] > 0, Transpose[#, FindPermutation[Join[{1, qb["OutputRank"] + 2}, Range[qb["OutputRank"]] + 1]]], {#}] & @
        TensorProduct[qb["OutputElements"], qb["InputElements"]],
    Prepend[qb["ElementDimensions"], qb["OutputSize"] qb["InputSize"]]
]

QuantumBasisProp[qb_, "ElementVectors"] := ArrayReshape[qb["Elements"], {qb["Dimension"], qb["ElementDimension"]}]


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

QuantumBasisProp[qb_, "FullInputQudits"] := Length[qb["Input"]["Dimensions"]]

QuantumBasisProp[qb_, "FullOutputQudits"] := Length[qb["Output"]["Dimensions"]]

QuantumBasisProp[qb_, "FullQudits"] := qb["FullInputQudits"] + qb["FullOutputQudits"]


QuantumBasisProp[qb_, "InputNameDimensions" | "InputDimensions"] := qb["Input"]["Dimensions"]

QuantumBasisProp[qb_, "OutputNameDimensions" | "OutputDimensions"] := qb["Output"]["Dimensions"]

QuantumBasisProp[qb_, "NameDimensions" | "Dimensions"] := DeleteCases[Join[qb["OutputNameDimensions"], qb["InputNameDimensions"]], 1]


QuantumBasisProp[qb_, "InputNameDimension" | "InputDimension"] := qb["Input"]["Dimension"]

QuantumBasisProp[qb_, "OutputNameDimension" | "OutputDimension"] := qb["Output"]["Dimension"]

QuantumBasisProp[qb_, "NameDimension" | "Dimension"] := qb["OutputDimension"] qb["InputDimension"]


QuantumBasisProp[qb_, "MatrixNameDimensions"] := Replace[{qb["OutputNameDimension"], qb["InputNameDimension"]}, {0, _} :> {0}]

QuantumBasisProp[qb_, "MatrixElementDimensions"] := Replace[{qb["OutputElementDimension"], qb["InputElementDimension"]}, {0, _} :> {0}]

QuantumBasisProp[qb_, "TensorDimensions"] := Join[qb["ElementDimensions"], qb["Dimensions"]]

QuantumBasisProp[qb_, "MatrixDimensions"] := Replace[{qb["ElementDimension"], qb["Dimension"]}, {0, _} :> {0}]


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

QuantumBasisProp[qb_, "ReducedMatrix"] := KroneckerProduct[qb["Output"]["ReducedMatrix"], qb["Input"]["ReducedMatrix"]]


QuantumBasisProp[qb_, "Projectors"] := Module[{x = qb["Elements"], y},
    {m, n} = qb["MatrixDimensions"];
    y = ArrayReshape[x, {n, m}];
    ArrayReshape[KroneckerProduct[y, Conjugate[y]][[;; ;; n + 1]], {n, m, m}]
]

QuantumBasisProp[qb_, "NormalizedProjectors"] := normalizeMatrix @* projector @* Flatten /@ qb["Elements"]

QuantumBasisProp[qb_, "PureStates"] := QuantumState[SparseArray[# -> 1, qb["OutputDimension"]], qb] & /@ Range[qb["OutputDimension"]]

QuantumBasisProp[qb_, "PureEffects"] := QuantumState[SparseArray[# -> 1, qb["InputDimension"]], qb] & /@ Range[qb["InputDimension"]]

QuantumBasisProp[qb_, "PureMaps" | "PureOperators"] :=
    Table[
        With[{array = SparseArray[{i, j} -> 1, {qb["OutputDimension"], qb["InputDimension"]}]}, QuantumState[Flatten @ array, qb]],
        {i, qb["OutputDimension"]}, {j, qb["InputDimension"]}
    ]

QuantumBasisProp[qb_, "Dual", out : {_Integer...}, in : {_Integer...}] :=
    QuantumBasis[qb, "Output" -> qb["Output"]["Dual", out], "Input" -> qb["Input"]["Dual", in]]

QuantumBasisProp[qb_, "Dual", qudits : {_Integer...}] :=
    qb["Dual", Sequence @@ MapAt[# - qb["OutputQudits"] &, 2] @ GatherBy[qudits, LessEqualThan[qb["OutputQudits"]]]]

QuantumBasisProp[qb_, "Dual"] := qb["Dual", Range[qb["Qudits"]]]


QuantumBasisProp[qb_, "Transpose"] := QuantumBasis[qb,
    "Input" -> qb["Output"], "Output" -> qb["Input"],
    "Label" -> Superscript[qb["Label"], "T"]
]

QuantumBasisProp[qb_, "Permute", perm_Cycles] :=
    QuantumBasis @@ Thread[{"Output", "Input"} -> QuantumTensorProduct[qb["Output"], qb["Input"]]["Permute", perm][
        "Split", toggleShift[PermutationList[perm, qb["Qudits"]], qb["OutputQudits"]]
    ]]

QuantumBasisProp[qb_, "Reverse"] := QuantumBasis[qb,
    "Output" -> qb["Output"]["Reverse"], "Input" -> qb["Input"]["Reverse"],
    "Label" -> Superscript[qb["Label"], "R"]
]

QuantumBasisProp[qb_, "Split", n_Integer] :=
    QuantumBasis["Output" -> #1, "Input" -> #2, qb["Meta"]] & @@ QuantumTensorProduct[qb["Output"], qb["Input"]]["Split", n]

QuantumBasisProp[qb_, "SplitDual", n_Integer] /; 0 <= n <= qb["Qudits"] :=
    Which[
        qb["OutputQudits"] < n,
        QuantumBasis["Output" -> (QuantumTensorProduct[#1, #2["Dual"]] & @@ #1["Split", qb["OutputQudits"]]), "Input" -> #2, qb["Meta"]],
        qb["OutputQudits"] > n,
        QuantumBasis["Output" -> #1, "Input" -> (QuantumTensorProduct[#1["Dual"], #2] & @@ #2["Split", qb["OutputQudits"] - n]), qb["Meta"]],
        True,
        QuantumBasis["Output" -> #1, "Input" -> #2, qb["Meta"]]
    ] & @@ QuantumTensorProduct[qb["Output"], qb["Input"]]["Split", n]

QuantumBasisProp[qb_, "SplitDual", n_Integer ? Negative] := qb["SplitDual", Mod[n, qb["Qudits"]]]

QuantumBasisProp[qb_, "SplitDual", _] := qb["SplitDual", qb["Qudits"]]


QuantumBasisProp[qb_, "Dagger" | "ConjugateTranspose"] := QuantumBasis[qb,
    "Input" -> qb["Output"]["Dual"], "Output" -> qb["Input"]["Dual"],
    "Label" -> SuperDagger[qb["Label"]]
]

QuantumBasisProp[qb_, "PermuteInput", perm_Cycles] := QuantumBasis[qb, "Input" -> qb["Input"]["Permute", perm]]

QuantumBasisProp[qb_, "PermuteOutput", perm_Cycles] := QuantumBasis[qb, "Output" -> qb["Output"]["Permute", perm]]

QuantumBasisProp[qb_, "SortedQ"] := qb["Output"]["SortedQ"] && qb["Input"]["SortedQ"]

QuantumBasisProp[qb_, "Sort"] := QuantumBasis[qb, "Output" -> qb["Output"]["Sort"], "Input" -> qb["Input"]["Sort"]]

QuantumBasisProp[qb_, "Decompose"] := QuantumBasis /@ Join[qb["Output"]["Decompose"], qb["Input"]["Decompose"]]



QuantumBasisProp[qb_, "Diagram", opts : OptionsPattern[QuantumDiagramGraphics]] := QuantumDiagramGraphics[
    qb,
    opts,
    "Shape" -> Which[
        qb["OutputQudits"] == 0 && qb["InputQudits"] == 0, "Diamond",
        qb["OutputQudits"] == 0, "Triangle",
        qb["InputQudits"] == 0, "UpsideDownTriangle",
        True, "Rectangle"
    ]
]

QuantumBasisProp[qb_, "ParameterArity"] := Length[qb["ParameterSpec"]]

QuantumBasisProp[qb_, "Parameters"] := qb["ParameterSpec"][[All, 1]]

QuantumBasisProp[qb_, "InitialParameters"] := qb["ParameterSpec"][[All, 2]]

QuantumBasisProp[qb_, "FinalParameters"] := qb["ParameterSpec"][[All, 3]]

