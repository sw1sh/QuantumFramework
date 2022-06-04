Package["Wolfram`QuantumFramework`"]

PackageScope["symbolicTensorQ"]
PackageScope["basisMultiplicity"]
PackageScope["nameQ"]
PackageScope["propQ"]
PackageScope["propName"]
PackageScope["stateQ"]
PackageScope["orderQ"]
PackageScope["autoOrderQ"]
PackageScope["targetQ"]
PackageScope["measurementReprQ"]

PackageScope["normalizeMatrix"]
PackageScope["tensorToVector"]
PackageScope["identityMatrix"]
PackageScope["kroneckerProduct"]
PackageScope["projector"]
PackageScope["MatrixPartialTrace"]
PackageScope["BlockDiagonalMatrix"]
PackageScope["eigenvectors"]
PackageScope["eigensystem"]
PackageScope["pauliMatrix"]

PackageScope["toggleSwap"]
PackageScope["toggleShift"]
PackageScope["alignDimensions"]

PackageScope["SparseArrayFlatten"]
PackageScope["SparsePseudoInverse"]
PackageScope["SetPrecisionNumeric"]
PackageScope["TranscendentalRecognize"]

PackageScope["$QuantumFrameworkProfile"]
PackageScope["profile"]




(* *)

symbolicTensorQ[a_] := MatchQ[a, _Symbol] || TensorQ[a] && AnyTrue[Level[a, {-1}], MatchQ[_Symbol]]


basisMultiplicity[dim_, size_] := Quiet[Ceiling @ Log[size, dim] /. 0 | Indeterminate -> 1, Divide::indet]


(* test functions *)

nameQ[name_] := MatchQ[name, _String | {_String, ___}]

propQ[prop_] := MatchQ[prop, _String | {_String, ___} | (_String -> _)]

propName[prop_] := Replace[prop, name_String | {name_String, ___} | (name_String -> _) :> name]

stateQ[state_] := !nameQ[state] && VectorQ[state] && Length[state] > 0 || SquareMatrixQ[state]

orderQ[order_] := VectorQ[order, IntegerQ] && DuplicateFreeQ[order]

autoOrderQ[order_] := MatchQ[order, _ ? orderQ | Automatic | {_ ? orderQ | Automatic, _ ? orderQ | Automatic}]

targetQ[target_] := VectorQ[target, IntegerQ]

measurementReprQ[state_] := TensorQ[state] && MemberQ[{2, 3}, TensorRank[state]]


(* Matrix tools *)

tensorToVector[t_ ? TensorQ] := Flatten[t]

(* scalar *)
tensorToVector[t_] := {t}


toTensor[t_ ? TensorQ] := t

toTensor[t_] := {t}


identityMatrix[0] := {{}}

identityMatrix[n_] := IdentityMatrix[n, SparseArray]


normalizeMatrix[matrix_] := Enclose[ConfirmQuiet[matrix / Tr[matrix], Power::infy]] (*SparseArray[{i_, i_} -> 1 / Tr[matrix], Dimensions[matrix], 1]*)


kroneckerProduct[ts___] := Fold[If[ArrayQ[#1] && ArrayQ[#2], KroneckerProduct[##], Times[##]] &, {ts}]


projector[v_] := KroneckerProduct[v, Conjugate[v]]


MatrixPartialTrace[matrix_, trace_, dimensions_] := ArrayReshape[
    toTensor @ TensorContract[
        ArrayReshape[matrix, Join[dimensions, dimensions]], Thread[{trace, trace + Length[dimensions]}]
    ],
    Table[Times @@ Delete[dimensions, List /@ trace], 2]
]


emptyTensorQ[t_ ? TensorQ] := Last[Dimensions[t]] === 0

matrixQ[m_] := MatrixQ[m] && ! emptyTensorQ[m]

BlockDiagonalMatrix[m1_ ? matrixQ, m2_ ? matrixQ] := Module[{
    r1, r2, c1, c2
},
  	{r1, c1} = Dimensions @ m1;
  	{r2, c2} = Dimensions @ m2;
    Join[
        Join[m1, ConstantArray[0, {r1, c2}, SparseArray], 2],
        Join[ConstantArray[0, {r2, c1}, SparseArray], m2, 2]
    ]
]

BlockDiagonalMatrix[mm__ ? matrixQ] := Fold[BlockDiagonalMatrix, {mm}]

BlockDiagonalMatrix[mm__ ? MatrixQ] := BlockDiagonalMatrix @@ DeleteCases[{mm}, _ ? emptyTensorQ]


Options[eigenvectors] = {"Sort" -> False, "Normalize" -> False, Chop -> False}

eigenvectors[matrix_, OptionsPattern[]] := Map[
    If[ TrueQ[OptionValue["Normalize"]], Normalize, Identity],
    Enclose[
        ConfirmBy[
            If[ TrueQ[OptionValue[Chop]],
                If[ Precision[matrix] === MachinePrecision,
                    Eigenvectors[matrix, ZeroTest -> (Chop[#1] == 0 &)],
                    Eigenvectors[Chop @ matrix]
                ],
                Eigenvectors[matrix]
            ],
            MatrixQ
        ],
        Eigenvectors[matrix] &
    ][[
        If[TrueQ[OptionValue["Sort"]], Ordering[Eigenvalues[matrix]], All]
    ]]
]


Options[eigensystem] = {"Sort" -> False, "Normalize" -> False, Chop -> False}

eigensystem[matrix_, OptionsPattern[]] := Module[{values, vectors},
    {values, vectors} = Enclose[
        ConfirmBy[
            If[ TrueQ[OptionValue[Chop]],
                If[ Precision[matrix] === MachinePrecision,
                    Eigensystem[matrix, ZeroTest -> (Chop[#1] == 0 &)],
                    Eigensystem[Chop @ matrix]
                ],
                Eigensystem[matrix]
            ],
            MatchQ[{_ ? ListQ, _ ? MatrixQ}]
        ],
        Eigensystem[matrix] &
    ];
    If[ TrueQ[OptionValue["Sort"]], With[{ordering = Ordering[values]}, values = values[[ordering]]; vectors = vectors[[ordering]]]];
    If[ TrueQ[OptionValue["Normalize"]], vectors = Normalize /@ vectors];

    {values, vectors}
]


pauliMatrix[n_] := pauliMatrix[n, 2]

pauliMatrix[0, dimension_] := identityMatrix[dimension]

pauliMatrix[1, dimension_] := With[{
    s = (dimension - 1) / 2
},
    SparseArray[
        {a_, b_} :> (KroneckerDelta[a, b + 1] + KroneckerDelta[a + 1, b]) Sqrt[(s + 1) (a + b - 1) - a b],
        {dimension, dimension}
    ]
]

pauliMatrix[2, dimension_] := With[{
    s = (dimension - 1) / 2
},
    SparseArray[
        {a_, b_} :> I (KroneckerDelta[a, b + 1] -  KroneckerDelta[a + 1, b]) Sqrt[(s + 1) (a + b - 1) - a b],
        {dimension, dimension}
    ]
]

pauliMatrix[3, dimension_] := With[{
    s = (dimension - 1) / 2
},
    SparseArray[
        {a_, b_} :> 2 (s + 1 - a) KroneckerDelta[a, b],
        {dimension, dimension}
    ]
]


(* optimization *)

SparseArrayFlatten[sa_SparseArray] := With[{dims = sa["Dimensions"]},
	With[{
        (* if all dimensions are equal don't fold *)
		cs = If[Equal @@ dims, dims ^ Range[Length[dims] - 1, 0, -1], Reverse @ FoldList[Times, 1, dims[[-1 ;; 2 ;; -1]]]],
		ps = Catenate @ MapThread[ConstantArray, {Range[0, dims[[1]] - 1], Differences @ sa["RowPointers"]}]
	},
		SparseArray[
            Automatic,
            {Times @@ dims},
            sa["ImplicitValue"],
            {
                1,
                {
                    {0, sa["ExplicitLength"]},
                    (* List /@ (1 + First[cs] * ps + Total[(sa["ColumnIndices"] - 1) * Threaded[Rest @ cs], {2}]) *)
                    List /@ (1 + First[cs] * ps + Total[Thread[Rest[cs] #] & /@ (sa["ColumnIndices"] - 1), {2}])
                },
                sa["ExplicitValues"]
            }
        ]
    ] /; Length[dims] > 11
]

SparseArrayFlatten[x_ ? NumericQ] := x

SparseArrayFlatten[array_] := Flatten[array]


SparsePseudoInverse[matrix_] := SparseArray @ If[SquareMatrixQ[matrix], Inverse[matrix], PseudoInverse[matrix]]


SetPrecisionNumeric[x_ /; NumericQ[x] || ArrayQ[x, _, NumericQ]] := SetPrecision[x, $MachinePrecision - 2]

SetPrecisionNumeric[x_] := x


(* helpers *)

toggleSwap[xs : {_Integer...}, n_Integer] := MapIndexed[(#1 > n) != (First[#2] > n) &, xs]

toggleShift[xs : {_Integer...}, n_Integer] := n - Subtract @@ Total /@ TakeDrop[Boole @ toggleSwap[xs, n], n]


alignDimensions[xs_, {}] := {{xs}, {xs}}

alignDimensions[{}, ys_] := {{ys}, {ys}}

alignDimensions[xs : {_Integer..}, ys : {_Integer..}] := Module[{
    as = FoldList[Times, xs], bs = FoldList[Times, ys], p, first, second
},
    p = Min[Intersection[as, bs]];
    If[ IntegerQ[p],
        first = TakeDrop[xs, First @ FirstPosition[as, p]];
        second = TakeDrop[ys, First @ FirstPosition[bs, p]];
        DeleteCases[{}] /@ MapThread[
            ReverseApplied[Prepend],
            {
                {first, second}[[All, 1]],
                alignDimensions @@ {first, second}[[All, 2]]
            }
        ],

        Missing[]
    ]
]


TranscendentalRecognize[num_ ? NumericQ, basis : _ ? VectorQ : {Pi}] := Module[{lr, ans},
  lr = FindIntegerNullVector[Prepend[N[basis, Precision[num]], num]];
  ans = Rest[lr] . basis / First[lr];
  Sign[N[ans]] Sign[num] ans
]


$QuantumFrameworkProfile = False

profile[label_] := Function[{expr}, If[TrueQ[$QuantumFrameworkProfile], EchoTiming[expr, label], expr], HoldFirst]

