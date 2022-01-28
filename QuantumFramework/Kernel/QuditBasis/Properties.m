Package["Wolfram`QuantumFramework`"]

PackageScope["QuditBasisProp"]



QuditBasis["Properties"] = {
    "Representations",
    "Association", "Names", "Elements",
    "Size", "Qudits",
    "Dimensions", "NameRank", "NameTensor",
    "ElementDimensions", "ElementDimension", "Rank",
    "MatrixDimensions", "TensorDimensions",
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


QuditBasisProp[_, "Properties"] := QuditBasis["Properties"]

QuditBasisProp[QuditBasis[representations_], "Representations"] := representations

QuditBasisProp[qb_, "Qudits"] := If[Length[qb["Representations"]] > 1, Max @ Keys[qb["Representations"]][[All, -1]], 0]

QuditBasisProp[qb_, "Dimensions"] := Values @ KeySort @ ResourceFunction["KeyGroupBy"][qb["Representations"], Last, Length]

QuditBasisProp[qb_, "Names"] := If[Length[qb["Representations"]] > 0,
    QuditName[#]["Group"] & /@ Tuples @ Values @ KeySort @ ResourceFunction["KeyGroupBy"][qb["Representations"], Last, Keys[#][[All, 1]] &],
    {}
]

QuditBasisProp[qb_, "Names", pos_ : All] := If[Length[qb["Representations"]] > 0,
    With[{values = Values @ KeySort @ ResourceFunction["KeyGroupBy"][qb["Representations"], Last, Keys[#][[All, 1]] &]},
        QuditName[#]["Group"] & /@ If[ pos === All,
            Tuples[values],
            (* MapThread[Part, {values, 1 + ResourceFunction["TupleFromIndex"][#, Length[values]]}] & /@ pos *)
            MapThread[Part, {values, #}] & /@ Tuples[Range[Length @ #] & /@ values][[pos]]
        ]
    ],
    {}
]

QuditBasisProp[qb_, "NameRank"] := Count[qb["Dimensions"], Except[1]]

QuditBasisProp[qb_, "NameTensor"] := ArrayReshape[qb["Names"], qb["Dimensions"]]

QuditBasisProp[qb_, "Elements"] := If[Length[qb["Representations"]] > 0,
    SparseArray[TensorProduct @@@ Tuples @ Values @ KeySort @ ResourceFunction["KeyGroupBy"][qb["Representations"], Last, SparseArray @* Values]],
    {}
]

QuditBasisProp[qb_, "Association"] := Association @ Thread[qb["Names"] -> qb["Elements"]]

QuditBasisProp[qb_, "Size"] := If[Length[qb["Representations"]] > 0, Times @@ qb["Dimensions"], 0]

QuditBasisProp[qb_, "ElementDimensions"] := Values @ KeySort @ ResourceFunction["KeyGroupBy"][qb["Representations"], Last, Length]

QuditBasisProp[qb_, "ElementDimension"] := If[qb["Size"] > 0, Times @@ qb["ElementDimensions"], 0]

QuditBasisProp[qb_, "Rank"] := If[qb["Size"] > 1, Length @ qb["ElementDimensions"], 0]

QuditBasisProp[qb_, "Dimension"] := If[qb["Size"] > 0, Times @@ qb["Dimensions"], 0]

QuditBasisProp[qb_, "MatrixDimensions"] := {qb["ElementDimension"], qb["Dimension"]}

QuditBasisProp[qb_, "TensorDimensions"] := Join[qb["ElementDimensions"], qb["Dimensions"]]

QuditBasisProp[qb_, "Tensor"] := If[qb["Rank"] > 0,
    ArrayReshape[Transpose[qb["Elements"], Cycles[{RotateRight @ Range[qb["Rank"] + 1, 1 , -1]}]], qb["TensorDimensions"]],
    qb["Elements"]
]

QuditBasisProp[qb_, "Matrix"] := ArrayReshape[qb["Tensor"], qb["MatrixDimensions"]]

QuditBasisProp[qb_, "Dual"] := QuditBasis @ KeyMap[MapAt[#["Dual"] &, 1], qb["Representations"]]

QuditBasisProp[qb_, "DualQ"] := AllTrue[Keys[qb["Representations"]][[All, 1]], #["DualQ"] &]

QuditBasisProp[qb_, "Reverse"] := QuditBasis @ KeyMap[MapAt[qb["NameRank"] - # + 1 &, 2], qb["Representations"]]


QuditBasisProp[qb_, "SortedQ"] := OrderedQ[Reverse /@ Keys @ qb["Representations"]]

QuditBasisProp[qb_, "Sort"] := QuditBasis[KeySortBy[qb["Representations"], Reverse]]

QuditBasisProp[qb_, {"Permute", perm_Cycles, outputs_Integer : 0}] := Enclose @ If[perm === Cycles[{}], qb,
QuditBasis[
    KeyMap[MapAt[PermutationList[perm, qb["NameRank"]][[#]] &, 2]] @ qb["Representations"]
]]

QuditBasisProp[qb_, "Reverse"] := qb[{"Permute", FindPermutation[Reverse @ Range[qb["Qudits"]]]}]

QuditBasisProp[qb_, {"Ordered", qudits_Integer, order_ ? orderQ}] := If[qb["Dimension"] <= 1, qb,
    With[{arity = Max[qudits, Max[order]]},
        QuantumTensorProduct[qb, QuditBasis[2, arity - qb["Qudits"]]][{"Permute",
            InversePermutation @ FindPermutation[Join[order, Complement[Range[arity], order]]]}]
    ]
]

QuditBasisProp[qb_, "RemoveIdentities"] := If[qb["Size"] > 1,
    QuditBasis[
        Select[qb["Representations"], TensorRank[#] > 0 &]
    ],
    qb
]

QuditBasisProp[qb_, {"Split", __}] /; qb["Size"] == 0 := {QuditBasis[0], QuditBasis[0]}

QuditBasisProp[qb_, {"Split", _Integer ? NonNegative}] /; qb["Dimension"] == 1 := {QuditBasis[], QuditBasis[]}

QuditBasisProp[qb_, {"Split", n_Integer ? NonNegative}] /; n <= qb["Qudits"] := {
    If[n > 0, QuditBasis[KeySelect[Last[#] <= n &] @ qb["Representations"]], QuditBasis[]],
    If[n < qb["Qudits"], QuditBasis[KeyMap[MapAt[# - n &, 2]] @ KeySelect[Last[#] > n &] @ qb["Representations"]], QuditBasis[]]
}

QuditBasisProp[qb_, {"TakeDimension", dim_Integer}] := With[{pos = FirstPosition[FoldList[Times, qb["Dimensions"]], dim]},
    If[ MissingQ[pos],
        Failure[<|"Message" -> "Can't take given number dimensions"|>],
        First @ qb[{"Split", First[pos]}]
    ]
]

QuditBasisProp[qb_, {"DropDimension", dim_Integer}] := With[{pos = FirstPosition[FoldList[Times, qb["Dimensions"]], dim]},
    If[ MissingQ[pos],
        Failure[<|"Message" -> "Can't drop given number dimensions"|>],
        Last @ qb[{"Split", First[pos]}]
    ]
]

QuditBasisProp[qb_, {"Take", n_Integer}] := Enclose @ First @ ConfirmBy[qb[{"Split", n}], ListQ]

QuditBasisProp[qb_, {"Drop", n_Integer}] := Enclose @ Last @ ConfirmBy[qb[{"Split", n}], ListQ]

QuditBasisProp[qb_, "Decompose"] := NestWhileList[Last[#][{"Split", 1}] &, {None, qb}, Last[#]["Dimension"] > 1 &][[2 ;;, 1]]

QuditBasisProp[qb_, {"Delete", n_Integer}] := QuantumTensorProduct[qb[{"Take", n - 1}], qb[{"Drop", n}]]

QuditBasisProp[qb_, {"Delete", ns : {_Integer...}}] := If[
    Complement[Range[qb["Qudits"]], ns] === {},
    QuditBasis[],
    QuantumTensorProduct @@ Delete[qb["Decompose"], List /@ ns]
]

QuditBasisProp[qb1_, {"Insert", n_Integer, qb2_ ? QuditBasisQ}] := QuantumTensorProduct[Insert[qb1[{"Split", n - 1}], qb2, 2]]

QuditBasisProp[_, {"Extract", {}}] := QuditBasis[]

QuditBasisProp[qb_, {"Extract", order_ ? orderQ}] := QuantumTensorProduct @ Extract[qb["Decompose"], List /@ order]

QuditBasisProp[qb1_, {"Replace", order_ ? orderQ, qb2_ ? QuditBasisQ}] := QuantumTensorProduct @ Permute[
    Join[Delete[qb1["Decompose"], List /@ order], qb2["Decompose"]],
    PermutationCycles @ Join[Delete[Range[qb1["Qudits"]], List /@ order], order]
]

QuditBasisProp[qb_, "Identity"] := qb

