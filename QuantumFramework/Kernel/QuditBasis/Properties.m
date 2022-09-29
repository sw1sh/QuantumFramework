Package["Wolfram`QuantumFramework`"]



QuditBasis["Properties"] = {
    "Representations",
    "Association", "Names", "Elements",
    "Length", "Size", "Qudits", "Rank",
    "Dimensions", "FullDimensions", "NameRank", "NameTensor",
    "ElementDimensions", "ElementDimension", "Rank",
    "MatrixDimensions", "TensorDimensions",
    "ElementsDimensions",
    "Matrix", "Tensor",
    "Dual", "Reverse"
}


QuditBasis::undefprop = "QuditBasis property `` is undefined for this basis";

(qb_QuditBasis[prop_ ? propQ, args___]) /; QuditBasisQ[qb] := With[{
    result = QuditBasisProp[qb, prop, args]
},
    If[$QuantumFrameworkPropCache, QuditBasisProp[qb, prop, args] = result, result] /; !FailureQ[Unevaluated @ result] &&
    (!MatchQ[result, _QuditBasisProp] || Message[QuditBasis::undefprop, prop])
]


QuditBasisProp[_, "Properties"] := Sort @ QuditBasis["Properties"]

QuditBasisProp[QuditBasis[representations_], "Representations"] := representations

QuditBasisProp[qb_, "Length"] := Length[qb["Representations"]]


QuditBasisProp[qb_, "Index"] := DeleteDuplicates @ Keys[qb["Representations"]][[All, -1]]

normalRepresentations[repr_] := Replace[DeleteCases[repr, Except[_ ? ArrayQ]], <||> -> QuditBasis[]["Representations"]]

QuditBasisProp[qb_, "FullDimensions"] := Values @ KeySort @ ResourceFunction["KeyGroupBy"][qb["Representations"], Last, Length @* normalRepresentations]

QuditBasisProp[qb_, "Dimensions"] := If[qb["Length"] > 0, Replace[DeleteCases[qb["FullDimensions"], 1], {} -> {1}], {}]

QuditBasisProp[qb_, "Names"] := If[qb["Length"] > 0,
    QuantumTensorProduct @* Map[QuditName] /@ Tuples @ Values @ KeySort @ ResourceFunction["KeyGroupBy"][qb["Representations"], Last, Keys[normalRepresentations[#]][[All, 1]] &],
    {}
]

QuditBasisProp[qb_, "Names", pos_ : All] := Enclose @ If[qb["Length"] > 0,
    With[{values = Values @ KeySort @ ResourceFunction["KeyGroupBy"][qb["Representations"], Last, Keys[normalRepresentations[#]][[All, 1]] &]},
        QuantumTensorProduct @* Map[QuditName] /@ If[ pos === All,
            Tuples[values],
            MapThread[ConfirmMatch[Part[##], _QuditName] &, {values, #}] & /@ Tuples[Range[Length @ #] & /@ values][[pos]]
        ]
    ],
    {}
]

QuditBasisProp[qb_, "NameRank" | "Qudits"] := Count[qb["Dimensions"], Except[1]]

QuditBasisProp[qb_, "FullNameRank"] := Count[qb["FullDimensions"], Except[1]]

QuditBasisProp[qb_, "FullQudits"] := Length[qb["FullDimensions"]]

QuditBasisProp[qb_, "NameTensor"] := ArrayReshape[qb["Names"], qb["FullDimensions"]]


computationalBasisQ[repr_] := And @@ ResourceFunction["KeyGroupBy"][repr, Last, With[{vs = SparseArray @ Values @ normalRepresentations @ #}, vs == IdentityMatrix[Length[vs], SparseArray]] &]

QuditBasisProp[qb_, "Elements"] /; computationalBasisQ[qb["Representations"]] := With[{
    dims = qb["Dimensions"], dim = qb["Dimension"], qudits = qb["Qudits"]
},
    SparseArray[
        {#, Splice[IntegerDigits[# - 1, MixedRadix[dims], qudits] + 1]} -> 1 & /@ Range[dim],
        Prepend[dims, dim]
    ]
]

QuditBasisProp[qb_, "Elements"] := If[qb["Length"] > 0,
    ArrayReshape[
        Transpose[
            Outer[Times,
                Sequence @@ Values @ KeySort @ ResourceFunction["KeyGroupBy"][
                    qb["RemoveIdentities"]["Representations"], Last, SparseArray @* Values @* normalRepresentations
                ]
            ],
            FindPermutation[Join[Range[1, 2 qb["Qudits"], 2], Range[2, 2 qb["Qudits"], 2]]]
        ],
        Prepend[DeleteCases[qb["ElementDimensions"], 1], qb["Dimension"]]
    ],
    {}
]

QuditBasisProp[qb_, "Association"] := Association @ Thread[qb["Names"] -> qb["Elements"]]

QuditBasisProp[qb_, "Size" | "Dimension"] := If[qb["Length"] > 0, Times @@ qb["Dimensions"], 0]

QuditBasisProp[qb_, "ElementDimensions"] := Catenate @ Values @ KeySort @ ResourceFunction["KeyGroupBy"][
    qb["Representations"],
    Last,
    Map[Dimensions] /* Sort /* Last /* Replace[{} -> {1}]
]

QuditBasisProp[qb_, "ElementDimension"] := If[qb["Size"] > 0, Times @@ qb["ElementDimensions"], 0]

QuditBasisProp[qb_, "Rank"] := If[qb["Size"] > 1, Count[qb["ElementDimensions"], Except[1]], 0]

QuditBasisProp[qb_, "MatrixDimensions"] := {qb["ElementDimension"], qb["Dimension"]}

QuditBasisProp[qb_, "TensorDimensions"] := Join[qb["ElementDimensions"], qb["Dimensions"]]

QuditBasisProp[qb_, "ElementsDimensions"] := Prepend[qb["ElementDimensions"], qb["Dimension"]]

QuditBasisProp[qb_, "Tensor"] := If[qb["Rank"] > 0,
    ArrayReshape[Transpose[ArrayReshape[qb["Elements"], DeleteCases[qb["ElementsDimensions"], 1]], Cycles[{RotateRight @ Range[qb["Rank"] + 1, 1 , -1]}]], qb["TensorDimensions"]],
    qb["Elements"]
]

QuditBasisProp[qb_, "Matrix"] := ArrayReshape[qb["Tensor"], qb["MatrixDimensions"]]

QuditBasisProp[qb_, "Dual"] := QuditBasis @ KeyMap[MapAt[#["Dual"] &, 1], qb["Representations"]]

QuditBasisProp[qb_, "DualQ"] := AllTrue[Keys[qb["RemoveIdentities"]["Representations"]][[All, 1]], #["DualQ"] &]


QuditBasisProp[qb_, "SortedQ"] := OrderedQ[Reverse /@ Keys @ qb["Representations"]]

QuditBasisProp[qb_, "Sort"] := QuditBasis[KeySortBy[qb["Representations"], Reverse]]

QuditBasisProp[qb_, "Permute", perm_Cycles] := Enclose @ If[
    perm === Cycles[{}],
    qb,
    Module[{idx = Delete[qb["Index"], Position[qb["FullDimensions"], 1]], repl},
        repl = Thread[idx -> PermutationList[perm, Length[idx]][[Ordering @ Ordering @ idx]]];
        QuditBasis[
            KeyMap[MapAt[Replace[repl], 2]] @ qb["Representations"]
        ]
    ]
]

QuditBasisProp[qb_, "Reverse"] := qb["Permute", FindPermutation[Reverse @ Range[qb["Qudits"]]]]

QuditBasisProp[qb_, "Ordered", qudits_Integer, order_ ? orderQ] := If[qb["Dimension"] <= 1, qb,
    With[{arity = Max[qudits, Max[order]]},
        QuantumTensorProduct[qb, QuditBasis[2, arity - qb["Qudits"]]]["Permute",
            InversePermutation @ FindPermutation[Join[order, Complement[Range[arity], order]]]]
    ]
]

QuditBasisProp[qb_, "RemoveIdentities"] := QuditBasis @ If[ qb["Dimension"] > 1,
    Select[qb["Representations"], TensorRank[#] > 0 &],
    qb["Representations"][[;; 1]]
]

QuditBasisProp[qb_, "Split", __] /; qb["Size"] == 0 := {QuditBasis[0], QuditBasis[0]}

QuditBasisProp[qb_, "Split", _Integer ? NonNegative] /; qb["Dimension"] == 1 := {QuditBasis[], QuditBasis[]}

QuditBasisProp[qb_, "Split", n_Integer ? NonNegative] /; n <= qb["Qudits"] := Module[{
    repr, idx
},
    repr = KeySortBy[qb["RemoveIdentities"]["Representations"], Last];
    idx = AssociationThread[Union @ Keys[repr][[All, -1]], Range[qb["Qudits"]]];
    {
        If[n > 0, QuditBasis[KeySelect[idx[Last[#]] <= n &] @ repr], QuditBasis[]],
        If[n < qb["Qudits"], QuditBasis[KeyMap[MapAt[idx[#] - n &, 2]] @ KeySelect[idx[Last[#]] > n &] @ repr], QuditBasis[]]
    }
]

QuditBasisProp[qb_, "TakeDimension", dim_Integer] := With[{pos = FirstPosition[FoldList[Times, qb["Dimensions"]], dim]},
    If[ MissingQ[pos],
        Failure[<|"Message" -> "Can't take given number dimensions"|>],
        First @ qb["Split", First[pos]]
    ]
]

QuditBasisProp[qb_, "DropDimension", dim_Integer] := With[{pos = FirstPosition[FoldList[Times, qb["Dimensions"]], dim]},
    If[ MissingQ[pos],
        Failure[<|"Message" -> "Can't drop given number dimensions"|>],
        Last @ qb["Split", First[pos]]
    ]
]

QuditBasisProp[qb_, "Take", n_Integer] := Enclose @ First @ ConfirmBy[qb["Split", n], ListQ]

QuditBasisProp[qb_, "Drop", n_Integer] := Enclose @ Last @ ConfirmBy[qb["Split", n], ListQ]

QuditBasisProp[qb_, "Decompose"] := NestWhileList[Last[#]["Split", 1] &, {None, qb}, Last[#]["Dimension"] > 1 &][[2 ;;, 1]]

QuditBasisProp[qb_, "Delete", n_Integer] := QuantumTensorProduct[qb["Take", n - 1], qb["Drop", n]]

QuditBasisProp[qb_, "Delete", ns : {_Integer...}] := If[
    Complement[Range[qb["Qudits"]], ns] === {},
    QuditBasis[],
    QuantumTensorProduct @@ Delete[qb["Decompose"], List /@ ns]
]

QuditBasisProp[qb1_, "Insert", n_Integer, qb2_ ? QuditBasisQ] := QuantumTensorProduct[Insert[qb1["Split", n - 1], qb2, 2]]

QuditBasisProp[_, "Extract", {}] := QuditBasis[]

QuditBasisProp[qb_, "Extract", pos : {_Integer ..}] := QuantumTensorProduct @ Extract[qb["Decompose"], List /@ pos]

QuditBasisProp[qb1_, "Replace", pos : {_Integer ..}, qb2_ ? QuditBasisQ] := QuantumTensorProduct @ Permute[
    Join[Delete[qb1["Decompose"], List /@ pos], qb2["Decompose"]],
    PermutationCycles @ Join[Delete[Range[qb1["Qudits"]], List /@ pos], pos]
]

QuditBasisProp[qb_, "Identity"] := qb

