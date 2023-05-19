Package["Wolfram`QuantumFramework`"]



QuditBasis["Properties"] = {
    "Representations",
    "Association", "Names", "Elements",
    "Length", "Size", "Qudits", "Rank",
    "Dimensions", "Shape", "NameRank", "NameTensor",
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

normalRepresentations[repr_Association] := If[Length[repr] == 0 || MemberQ[repr, _ ? emptyTensorQ], <||>, Replace[DeleteCases[repr, Except[_ ? ArrayQ]], <||> :> Select[repr, AtomQ, 1]]]

QuditBasisProp[qb_, "Shape" | "FullDimensions"] := If[qb["Length"] > 0, Values @ KeySort @ ResourceFunction["KeyGroupBy"][qb["Representations"], Last, Length @* normalRepresentations], {0}]


shapeDimensions[shape_List] := With[{n = First @ FirstPosition[shape, 0, {-1}, {1}, Heads -> False]},
    DeleteCases[shape[[;; n]], 1]
]

QuditBasisProp[qb_, "Dimensions"] := shapeDimensions[qb["Shape"]]


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

QuditBasisProp[qb_, "NameRank" | "Qudits"] := Count[qb["Dimensions"], Except[0]]

QuditBasisProp[qb_, "FullNameRank" | "ShapeRank"] := Count[TakeWhile[qb["Shape"], # != 0 &], Except[1]]

QuditBasisProp[qb_, "FullQudits"] := Length[qb["Shape"]]

QuditBasisProp[qb_, "NameTensor"] := ArrayReshape[qb["Names"], qb["Shape"]]


computationalBasisQ[repr_] := And @@ ResourceFunction["KeyGroupBy"][repr, Last, With[{vs = Values @ normalRepresentations @ #}, emptyTensorQ[vs] || SparseArray[vs] == IdentityMatrix[Length[vs], SparseArray]] &]

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
                    SparseArrayFlatten /@ qb["Canonical"]["Representations"], Last, Values @* normalRepresentations
                ]
            ],
            FindPermutation[Join[Range[1, 2 qb["Qudits"], 2], Range[2, 2 qb["Qudits"], 2]]]
        ],
        Prepend[DeleteCases[qb["ElementDimensions"], 1], qb["Dimension"]]
    ],
    {}
]

QuditBasisProp[qb_, "ReducedElements"] := If[qb["Length"] > 0,
    ArrayReshape[
        Transpose[
            Outer[Times,
                Sequence @@ Map[If[MatrixQ[#] && ! SquareMatrixQ[#], SparseArray[# . PseudoInverse[RowReduce[#]]], #] &] @ Values @ KeySort @ ResourceFunction["KeyGroupBy"][
                    SparseArrayFlatten /@ qb["Canonical"]["Representations"], Last, Values @* normalRepresentations
                ]
            ],
            FindPermutation[Join[Range[1, 2 qb["Qudits"], 2], Range[2, 2 qb["Qudits"], 2]]]
        ],
        Prepend[qb["Dimensions"], qb["Dimension"]]
    ],
    {}
]

QuditBasisProp[qb_, "Association"] := Association @ Thread[qb["Names"] -> qb["Elements"]]

QuditBasisProp[qb_, "Size" | "Dimension"] := Times @@ qb["Dimensions"]

QuditBasisProp[qb_, "ElementShape"] :=  If[
    qb["Length"] > 0,
    Catenate @ Values @ KeySort @ ResourceFunction["KeyGroupBy"][
        qb["Representations"],
        Last,
        Map[Dimensions] /* Sort /* Last
    ],
    {0}
]

QuditBasisProp[qb_, "ElementDimensions"] := shapeDimensions[qb["Canonical"]["ElementShape"]]

QuditBasisProp[qb_, "ElementDimension"] := Times @@ qb["ElementDimensions"]

QuditBasisProp[qb_, "Rank"] := Count[TakeWhile[qb["ElementDimensions"], # != 0 &], Except[1]]

QuditBasisProp[qb_, "MatrixDimensions"] := Replace[{qb["ElementDimension"], qb["Dimension"]}, {0, _} :> {0}]

QuditBasisProp[qb_, "TensorDimensions"] := Join[qb["ElementDimensions"], qb["Dimensions"]]

QuditBasisProp[qb_, "ElementsDimensions"] := Prepend[qb["ElementDimensions"], Replace[qb["Dimension"], 0 -> Nothing]]

QuditBasisProp[qb_, "Tensor"] := If[qb["Rank"] > 0,
    ArrayReshape[Transpose[ArrayReshape[qb["Elements"], DeleteCases[qb["ElementsDimensions"], 1]], Cycles[{RotateRight @ Range[qb["Rank"] + 1, 1 , -1]}]], qb["TensorDimensions"]],
    qb["Elements"]
]

QuditBasisProp[qb_, "ReducedTensor"] := If[qb["Rank"] > 0,
    ArrayReshape[Transpose[ArrayReshape[qb["ReducedElements"], Prepend[qb["Dimensions"], qb["Dimension"]]], Cycles[{RotateRight @ Range[qb["NameRank"] + 1, 1 , -1]}]], Join[qb["Dimensions"], qb["Dimensions"]]],
    qb["ReducedElements"]
]

QuditBasisProp[qb_, "Matrix"] := ArrayReshape[qb["Tensor"], qb["MatrixDimensions"]]

QuditBasisProp[qb_, "ReducedMatrix"] := ArrayReshape[qb["ReducedTensor"], qb["MatrixDimensions"]]


QuditBasisProp[qb_, "Dual"] := QuditBasis @ KeyMap[MapAt[#["Dual"] &, 1], qb["Representations"]]

QuditBasisProp[qb_, "Dual", qudits : {_Integer...}] := With[{index = Lookup[MapIndexed[First[#2] -> #1 &, qb["Index"]], qudits, Nothing]},
    QuditBasis @ KeyMap[If[MemberQ[index, #[[2]]], MapAt[#["Dual"] &, #, 1], #] &, qb["Representations"]]
]

QuditBasisProp[qb_, "DualQ"] := AllTrue[Keys[qb["Canonical"]["Representations"]][[All, 1]], #["DualQ"] &]


QuditBasisProp[qb_, "SortedQ"] := OrderedQ[Last /@ Keys @ qb["Representations"]]

QuditBasisProp[qb_, "Sort"] := QuditBasis[KeySortBy[qb["Representations"], {Last}]]

QuditBasisProp[qb_, "Permute", perm_Cycles] := Enclose @ If[
    perm === Cycles[{}],
    qb,
    Module[{idx = Delete[qb["Index"], Position[qb["Shape"], 1]], repl},
        repl = Thread[idx -> Permute[idx, perm]];
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

QuditBasisProp[qb_, "Canonical"] := QuditBasis @ Which[
    qb["Dimension"] > 1,
    Select[qb["Representations"], Times @@ Dimensions[#] > 1 &],
    qb["Dimension"] == 0,
    Select[qb["Representations"], # === {} &, 1],
    True,
    Select[qb["Representations"], Times @@ Dimensions[#] == 1 &, 1]
]

QuditBasisProp[qb_, "Split", __] /; qb["Size"] == 0 := {QuditBasis[0], QuditBasis[0]}

QuditBasisProp[qb_, "Split", _Integer] /; qb["Dimension"] == 1 := {QuditBasis[], QuditBasis[]}

QuditBasisProp[qb_, "Split", n_Integer] /; 0 <= n <= qb["Qudits"] := Module[{
    repr, idx
},
    repr = KeySortBy[qb["Canonical"]["Representations"], {Last}];
    idx = AssociationThread[Union @ Keys[repr][[All, -1]], Range[qb["Qudits"]]];
    {
        If[n > 0, QuditBasis[KeySelect[idx[Last[#]] <= n &] @ repr], QuditBasis[]],
        If[n < qb["Qudits"], QuditBasis[KeyMap[MapAt[idx[#] - n &, 2]] @ KeySelect[idx[Last[#]] > n &] @ repr], QuditBasis[]]
    }
]

QuditBasisProp[qb_, "Split", n_Integer ? Negative] := qb["Split", Mod[n, qb["Qudits"]]]

QuditBasisProp[qb_, "Split", _] := qb["Split", qb["Qudits"]]


QuditBasisProp[qb_, "TakeDimension", dim_Integer ? NonNegative] := If[dim <= 1,
    QuditBasis[dim],
    With[{pos = FirstPosition[FoldList[Times, qb["Dimensions"]], dim]},
        If[ MissingQ[pos],
            Failure[<|"Message" -> "Can't take given number dimensions"|>],
            First @ qb["Split", First[pos]]
        ]
    ]
]

QuditBasisProp[qb_, "DropDimension", dim_Integer ? NonNegative] := If[dim <= 1,
    qb,
    With[{pos = FirstPosition[FoldList[Times, qb["Dimensions"]], dim]},
        If[ MissingQ[pos],
            Failure[<|"Message" -> "Can't drop given number dimensions"|>],
            Last @ qb["Split", First[pos]]
        ]
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


QuditBasisProp[qb_, "DimensionSplit" | "DimensionSplitDual", __] /; qb["Dimension"] <= 1 := qb

QuditBasisProp[qb_, prop : "DimensionSplit" | "DimensionSplitDual", q_Integer ? Positive, n_Integer ? Positive] /; q <= qb["Qudits"] && n <= qb["Dimensions"][[q]] :=
    QuantumTensorProduct @ MapAt[
        QuditBasis[
            Join @@ MapAt[KeyMap[Replace[{name_, i_} :> {If[StringEndsQ[prop, "Dual"], name["Dual"], name], i + 1}]], TakeDrop[#["Representations"], n], 2]
        ] &,
        qb["Decompose"],
        q
    ]

QuditBasisProp[qb_, prop : "DimensionSplit" | "DimensionSplitDual", _[q_Integer ? Positive, n_Integer ? Positive]] := qb[prop, q, n]

QuditBasisProp[qb_, prop : "DimensionSplit" | "DimensionSplitDual", splits : {_[Repeated[_Integer ? Positive, {2}]] ..}] := Fold[#1[prop, #2] &, qb, splits]

QuditBasisProp[qb_, prop : "DimensionSplit" | "DimensionSplitDual", splits : {_Integer ? Positive ..}] := qb[prop, Thread[{Range[Length[splits]], splits}]]

