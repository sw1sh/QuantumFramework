Package["Wolfram`QuantumFramework`"]

PackageScope["SymbolicQ"]
PackageScope["basisMultiplicity"]
PackageScope["nameQ"]
PackageScope["propQ"]
PackageScope["propName"]
PackageScope["stateQ"]
PackageScope["orderQ"]
PackageScope["autoOrderQ"]
PackageScope["targetQ"]
PackageScope["targetsQ"]
PackageScope["measurementReprQ"]
PackageScope["emptyTensorQ"]

PackageScope["normalizeMatrix"]
PackageScope["tensorToVector"]
PackageScope["identityMatrix"]
PackageScope["kroneckerProduct"]
PackageScope["projector"]
PackageScope["MatrixPartialTrace"]
PackageScope["EinsteinSummation"]
PackageScope["blockDiagonalMatrix"]
PackageScope["eigenvectors"]
PackageScope["eigensystem"]
PackageScope["pauliMatrix"]
PackageScope["spinMatrix"]
PackageScope["fanoMatrix"]

PackageScope["toggleSwap"]
PackageScope["toggleShift"]
PackageScope["alignDimensions"]

PackageScope["SparseArrayFlatten"]
PackageScope["SparsePseudoInverse"]
PackageScope["SetPrecisionNumeric"]
PackageScope["TranscendentalRecognize"]

PackageScope["QuditAdjacencyMatrix"]

PackageScope["$QuantumFrameworkProfile"]
PackageScope["profile"]
PackageScope["Memoize"]




(* *)

SymbolicQ[expr_] := ! FreeQ[expr, sym_Symbol ? Developer`SymbolQ /; ! MemberQ[{Rational, Complex}, sym] && ! MemberQ[Attributes[sym], NumericFunction | Constant]]

basisMultiplicity[dim_, size_] := Quiet[Replace[Ceiling @ Log[size, dim], Except[_Integer ? Positive] -> 1], {Divide::indet, Power::infy}]


(* test functions *)

nameQ[name_] := MatchQ[name, _String | {_String, ___} | _String[___]]

propQ[prop_] := MatchQ[prop, _String | {_String, ___} | (_String -> _)]

propName[prop_] := Replace[prop, name_String | {name_String, ___} | (name_String -> _) :> name]

stateQ[state_] := ! MatchQ[state, name_String | {_nameString, ___} /; MemberQ[$QuantumStateNames, name]] && VectorQ[state] && Length[state] >= 0 || SquareMatrixQ[state]

orderQ[order_] := VectorQ[order, IntegerQ] && DuplicateFreeQ[order]

autoOrderQ[order_] := MatchQ[order, _ ? orderQ | Automatic | {_ ? orderQ | Automatic, _ ? orderQ | Automatic}]

targetQ[target_] := VectorQ[target, IntegerQ] && AllTrue[target, Positive]

targetsQ[targets_] := VectorQ[targets, targetQ]

measurementReprQ[state_] := TensorQ[state] && MemberQ[{2, 3}, TensorRank[state]]


(* Matrix tools *)

tensorToVector[t_ ? TensorQ] := Flatten[t]

(* scalar *)
tensorToVector[t_] := {t}


toTensor[t_ ? TensorQ] := t

toTensor[t_] := {t}


identityMatrix[0 | {_, 0} | {0, _}] := {{}}

identityMatrix[n_] := IdentityMatrix[n, SparseArray]


normalizeMatrix[matrix_] := With[{tr = Tr[matrix]}, If[tr == 0, matrix, matrix / tr]]


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


blockDiagonalMatrix[ms : {__ ? MatrixQ}] := MyBlockDiagonalMatrix[DeleteCases[ms, _ ? emptyTensorQ]]


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
        If[TrueQ[OptionValue["Sort"]], OrderingBy[Eigenvalues[matrix], {Abs[#], Mod[Arg[#], 2 Pi]} &], All]
    ]]
]


Options[eigensystem] = {"Sort" -> False, "Normalize" -> False, Chop -> False}

eigensystem[matrix_, OptionsPattern[]] := Module[{values, vectors},
    {values, vectors} = Chop @ FullSimplify @ Enclose[
        ConfirmBy[
            If[ TrueQ[OptionValue[Chop]],
                If[ Precision[matrix] === MachinePrecision,
                    Quiet @ Check[
                        Eigensystem[matrix, ZeroTest -> (Chop[N[#1]] == 0 &)],
                        Eigensystem[matrix],
                        Eigensystem::eivec0
                    ],
                    Eigensystem[Chop @ matrix]
                ],
                Eigensystem[matrix]
            ],
            MatchQ[{_ ? ListQ, _ ? MatrixQ}]
        ],
        Eigensystem[matrix] &
    ];
    If[ TrueQ[OptionValue["Sort"]], With[{ordering = OrderingBy[values, If[Length[values] > 2 && ContainsOnly[Arg[values], {0, Pi}], Identity, {Mod[Arg[#], 2 Pi], Abs[#]} &]]}, values = values[[ordering]]; vectors = vectors[[ordering]]]];
    If[ TrueQ[OptionValue["Normalize"]], vectors = Normalize[If[First[#] != 0, # / First[#], #]] & /@ vectors];

    {values, vectors}
]


pauliMatrix[n_] := pauliMatrix[n, 2]

pauliMatrix[0, dimension_] := identityMatrix[dimension]

pauliMatrix[1, dimension_] := SparseArray[Table[{n, Mod[n - 1, dimension]} + 1 -> 1, {n, 0, dimension - 1}], {dimension, dimension}]

spinMatrix[1, dimension_] := With[{
    s = (dimension - 1) / 2
},
    SparseArray[
        {a_, b_} :> (KroneckerDelta[a, b + 1] + KroneckerDelta[a + 1, b]) Sqrt[(s + 1) (a + b - 1) - a b],
        {dimension, dimension}
    ]
]

pauliMatrix[2, dimension_] := - I pauliMatrix[3, dimension] . pauliMatrix[1, dimension]

spinMatrix[2, dimension_] := With[{
    s = (dimension - 1) / 2
},
    SparseArray[
        {a_, b_} :> I (KroneckerDelta[a, b + 1] -  KroneckerDelta[a + 1, b]) Sqrt[(s + 1) (a + b - 1) - a b],
        {dimension, dimension}
    ]
]

pauliMatrix[3, dimension_] := SparseArray[Table[{n, n} + 1 -> Exp[- 2 Pi I n / dimension], {n, 0, dimension - 1}], {dimension, dimension}]

spinMatrix[3, dimension_] := With[{
    s = (dimension - 1) / 2
},
    SparseArray[
        {a_, b_} :> 2 (s + 1 - a) KroneckerDelta[a, b],
        {dimension, dimension}
    ]
]

fanoMatrix[d_, q_, p_, x_ : Automatic, z_ : Automatic] :=
    FullSimplify @
    Exp[I Pi q p / d] *
        MatrixPower[FourierMatrix[d], 2] .
            MatrixPower[Replace[z, Automatic :> ConjugateTranspose[pauliMatrix[3, d]]], q] .
                    MatrixPower[Replace[x, Automatic :> ConjugateTranspose[pauliMatrix[1, d]]], p]

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


SparsePseudoInverse[matrix_] := SparseArray @ If[SquareMatrixQ[matrix], Quiet @ Check[Inverse[matrix], PseudoInverse[matrix]], PseudoInverse[matrix]]


SetPrecisionNumeric[x_ /; NumericQ[x] || ArrayQ[x, _, NumericQ]] := SetPrecision[x, $MachinePrecision - 3]

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

ReverseHalf[list_] /; Divisible[Length[list], 2] := Catenate @ Reverse[TakeDrop[list, Length[list] / 2]]

HadamardGrayRowPermutation[n_Integer ? Positive] := FindPermutation @ Nest[Insert[#, Splice @ ReverseHalf[# + Length[#]], Length[#] / 2 + 1] &, {1, 2}, n - 1]

(* TODO: WFR *)
Memoize[f_ ? Developer`SymbolQ] := With[{g = Unique[f]},
    DownValues[g] = MapAt[ReplaceAll[f -> g], DownValues[f], {All, 1}];
    ResourceFunction["BlockProtected"][{f},
        DownValues[f] = {HoldPattern[f[args___]] :> ResourceFunction["BlockProtected"][{f}, f[args] = g[args]]};
    ]
]

MyBlockDiagonalMatrix[{m1_ ? matrixQ, m2_ ? matrixQ}] := Block[{
    r1, r2, c1, c2
},
  	{r1, c1} = Dimensions @ m1;
  	{r2, c2} = Dimensions @ m2;
    Join[
        Join[m1, ConstantArray[0, {r1, c2}, SparseArray], 2],
        Join[ConstantArray[0, {r2, c1}, SparseArray], m2, 2]
    ]
]

MyBlockDiagonalMatrix[mm : {__ ? matrixQ}] := Fold[MyBlockDiagonalMatrix[{##}] &, mm]

MyBlockDiagonalMatrix[{}] := {{}}



EinsteinSummation[in_List, arrays_] := Module[{
	res = isum[in -> Cases[Tally @ Flatten @ in, {_, 1}][[All, 1]], arrays]
},
	res /; res =!= $Failed
]

EinsteinSummation[in_List -> out_, arrays_] := isum[in -> out, arrays]

EinsteinSummation[s_String, arrays_] := EinsteinSummation[
	Replace[
		StringSplit[s, "->"],
		{{in_, out_} :> Characters[StringSplit[in, ","]] -> Characters[out],
		{in_} :> Characters[StringSplit[in, ","]]}
	],
	arrays
]

isum[in_List -> out_, arrays_List] := Enclose @ Module[{
	indices, dimensions, contracted, contractions, multiplicity, tensor, transpose
},
	If[ Length[in] != Length[arrays],
        Message[EinsteinSummation::length, Length[in], Length[arrays]];
	    Confirm[$Failed]
    ];
	MapThread[
		If[ IntegerQ @ TensorRank[#1] && Length[#1] != TensorRank[#2],
			Message[EinsteinSummation::shape, #1, #2];
            Confirm[$Failed]
		] &,
		{in, arrays}
	];
	indices = Catenate[in];
    dimensions = Catenate[Dimensions /@ arrays];
	contracted = DeleteElements[indices, 1 -> out];
	contractions = Flatten[Take[Position[indices, #[[1]], {1}, Heads -> False], UpTo[#[[2]]]]] & /@ Tally[contracted];
    If[! AllTrue[contractions, Equal @@ dimensions[[#]] &], Message[EinsteinSummation::dim]; Confirm[$Failed]];
	indices = DeleteElements[indices, 1 -> contracted];
	If[! ContainsAll[indices, out], Message[EinsteinSummation::output]; Confirm[$Failed]];
    tensor = Activate @ TensorContract[Inactive[TensorProduct] @@ arrays, contractions];
	multiplicity = Max @ Merge[{Counts[out], Counts[indices]}, Apply[Ceiling[#1 / #2] &]];
	If[ multiplicity > 1,
		indices = Catenate @ ConstantArray[indices, multiplicity];
		contracted = DeleteElements[indices, 1 -> out];
		contractions = Flatten[Take[Position[indices, #[[1]]], #[[2]]]] & /@ Tally[contracted];
		indices = DeleteElements[indices, 1 -> contracted];
		tensor = Activate @ TensorContract[Inactive[TensorProduct] @@ ConstantArray[tensor, multiplicity], contractions];
	];
	transpose = FindPermutation[indices, out];
	If[ArrayQ[tensor], Transpose[tensor, transpose], tensor]
]

EinsteinSummation::length = "Number of index specifications (`1`) does not match the number of tensors (`2`)";
EinsteinSummation::shape = "Index specification `1` does not match the tensor rank of `2`";
EinsteinSummation::dim = "Dimensions of contracted indices do not much";
(*EinsteinSummation::repeat = "Index specifications `1` are repeated more than twice";*)
EinsteinSummation::output = "The uncontracted indices can't compose the desired output";


QuditAdjacencyMatrix[x_, d_ : Automatic] := {{Abs[Normalize[x]] Mod[Arg[x], 2 Pi] / (Pi / (2 Replace[d, Automatic -> 2]))}}
QuditAdjacencyMatrix[xs_ ? VectorQ, dim_ : Automatic] := With[{d = Replace[dim, Automatic :> Length[xs]]},
	Abs[xs] DiagonalMatrix[Mod[Arg[xs], 2 Pi] / (Pi / (2 d)) + 1] + If[d > 1, SparseArray[Band[{2, 1}] -> 1, {d, d}], 0]
]

