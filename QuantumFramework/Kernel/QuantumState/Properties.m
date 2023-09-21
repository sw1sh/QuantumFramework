Package["Wolfram`QuantumFramework`"]



$QuantumStateProperties = {
    "StateType", "State", "Basis",
    "Amplitudes", "Weights", "Probabilities", "Probability", "StateVector", "DensityMatrix",
    "NormalizedState", "NormalizedAmplitudes", "NormalizedStateVector", "NormalizedDensityMatrix",
    "Entropy", "VonNeumannEntropy",
    "Purity", "Type", "PureStateQ", "MixedStateQ", "MatrixQ", "VectorQ", "UnknownQ", "PhysicalQ",
    "Kind", "Scalar",
    "Norm", "TraceNorm", "NormalizedQ",
    "BlochSphericalCoordinates", "BlochCartesianCoordinates",
    "BlochPlot",
    "Projector", "NormalizedProjector",
    "Operator", "NormalizedOperator",
    "Eigenvalues", "Eigenvectors", "Eigenstates",
    "Computational", "SchmidtBasis", "SpectralBasis", "PrimeBasis", "UniformBasis",
    "StateTensor", "StateMatrix",
    "VectorState", "MatrixState",
    "Tensor", "Matrix",
    "Purify", "Unpurify",
    "Bend", "BendDual", "Unbend", "Double",
    "Pure", "Mixed",
    "Trace", "Transpose", "Conjugate", "ConjugateTranspose",
    "Physical",
    "ReverseOutput", "ReverseInput", "Reverse",
    "TensorReverseOutput", "TensorReverseInput",
    "Split", "SplitDual",
    "Bipartition",
    "Disentangle", "Decompose", "DecomposeWithAmplitudes", "DecomposeWithProbabilities",
    "SchmidtDecompose",
    "Formula", "Simplify", "FullSimplify"
};

QuantumState["Properties"] := Union @ Join[$QuantumStateProperties, QuantumBasis["Properties"]]


qs_QuantumState["ValidQ"] := QuantumStateQ[qs]


QuantumState::undefprop = "property `` is undefined for this state"

QuantumState::failprop = "property `` failed with ``"

(qs_QuantumState[prop_ ? propQ, args___]) /; QuantumStateQ[qs] := With[{
    result = QuantumStateProp[qs, prop, args]
},
    If[ TrueQ[$QuantumFrameworkPropCache] &&
        ! MemberQ[{"Properties", "Basis"}, prop] &&
        QuantumStateProp[qs, "Basis"]["ParameterArity"] == 0,
        QuantumStateProp[qs, prop, args] = result,
        result
    ] /;
        (!FailureQ[Unevaluated @ result] || Message[QuantumState::failprop, prop, result]) &&
        (!MatchQ[Unevaluated @ result, _QuantumStateProp] || Message[QuantumState::undefprop, prop])
]


QuantumStateProp[qs_, "Properties"] :=
    If[qs["Dimension"] == 2, QuantumState["Properties"], DeleteCases[QuantumState["Properties"], _ ? (StringStartsQ["Bloch"])]]


(* getters *)

QuantumStateProp[QuantumState[state_, _], "State"] := state

QuantumStateProp[QuantumState[_, basis_], "Basis"] := basis


(* two types of states *)

QuantumStateProp[qs_, "StateType"] := Which[
    VectorQ[qs["State"]],
    "Vector",
    SquareMatrixQ[qs["State"]],
    "Matrix",
    True,
    "UnknownType"
]

QuantumStateProp[qs_, "VectorQ" | "VectorStateQ"] := qs["StateType"] === "Vector"

QuantumStateProp[qs_, "MatrixQ" | "MatrixStateQ"] := qs["StateType"] === "Matrix"

QuantumStateProp[qs_, "UnknownQ" | "UnknownStateQ"] := qs["StateType"] === "UnknownType"

QuantumStateProp[qs_, "PhysicalQ"] := ! qs["UnknownQ"]

QuantumStateProp[qs_, "NumericQ"] := ArrayQ[qs["State"], 1 | 2, NumericQ]


QuantumStateProp[qs_, "Kind"] := Which[
    qs["InputQudits"] === qs["OutputQudits"] === 0,
    "Scalar",
    qs["InputQudits"] === 0,
    "State",
    qs["OutputQudits"] === 0,
    "Effect",
    True,
    "Map"
]

(* amplitudes are only defined for pure states *)

QuantumState::notpure = "is not a pure state";

QuantumStateProp[qs_, "Amplitudes"] := Module[{s = qs["Pure"], result},
    result = Enclose @ KeySort @ Association @ Thread[s["ElementNames"] -> ConfirmBy[Chop @ s["StateVector"], VectorQ]];
    result /; !FailureQ[result]
]

QuantumStateProp[qs_, "StateVector"] := Module[{result},
    result = Enclose @ Which[
        qs["StateType"] === "Vector",
        qs["State"],
        qs["PureStateQ"],
        Module[{eigenvalues = Chop[qs["Eigenvalues"]], pick},
            pick = ConfirmBy[Map[# =!= 0 &, eigenvalues], Apply[Or]];
            SparseArray @ First @ MapThread[Times, {Pick[qs["Eigenvectors", "Normalize" -> qs["NormalizedQ"]], pick], Pick[eigenvalues, pick]}]
        ],
        qs["Type"] === "Degenerate",
        SparseArray[{}, qs["Dimension"]],
        True,
        SparseArrayFlatten @ qs["Bend"]["State"]
    ];
    result /; !FailureQ[result]
]

QuantumStateProp[qs_, "Scalar" | "Number"] /; qs["Kind"] === "Scalar" := First[Flatten[qs["State"]]]

QuantumStateProp[qs_, "Weights"] := Which[
    qs["PureStateQ"] || qs["VectorQ"] && ! qs["NumericQ"],
    Abs[qs["StateVector"]] ^ 2,
    qs["PhysicalQ"] || ! qs["NumericQ"],
    Diagonal @ qs["DensityMatrix"],
    True,
    qs["Physical"]["Weights"]
]

QuantumStateProp[qs_, "Probabilities"] := Re @ Normalize[qs["Weights"], Total]

QuantumStateProp[qs_, "ProbabilityAssociation" | "Probability"] := With[{proba = Chop @ SparseArray @ qs["Probabilities"]},
    AssociationThread[
        qs["Names", QuotientRemainder[Catenate @ proba["ExplicitPositions"] - 1, qs["InputDimension"]] + 1],
        proba["ExplicitValues"]
    ]
]

QuantumStateProp[qs_, "Distribution"] := CategoricalDistribution[qs["Names"], qs["Probabilities"]]

QuantumStateProp[qs_, "Formula", OptionsPattern[]] /; qs["DegenerateStateQ"] := 0

QuantumStateProp[qs_, "Formula", OptionsPattern["Normalize" -> False]] := With[{s = qs["Pure"]},
    If[ s["Dimension"] == 0, 0,
        With[{v = SparseArray @ s[If[TrueQ[OptionValue["Normalize"]], "NormalizedStateVector", "StateVector"]], d = s["InputDimension"]},
            With[{pos = Catenate @ v["ExplicitPositions"]}, s["Names", Thread[{Quotient[pos - 1, d] + 1, Mod[pos - 1, d] + 1}]]] . v["ExplicitValues"]
        ]
    ]
]

QuantumStateProp[qs_, prop : "Simplify" | "FullSimplify"] := QuantumState[Map[Symbol[prop], qs["State"], {If[qs["VectorQ"], 1, 2]}], qs["Basis"]]


(* normalization *)

QuantumStateProp[qs_, "Norm"] := FullSimplify @ If[qs["StateType"] === "Vector", Norm[qs["StateVector"]], Tr @ qs["DensityMatrix"]]

QuantumStateProp[qs_, "TraceNorm"] := Total @ SingularValueList @ qs["DensityMatrix"]


QuantumStateProp[qs_, "NormalizedQ"] := qs["Norm"] == 1

QuantumStateProp[qs_, "NormalizedState"] := With[{state = qs["State"]},
    QuantumState[
        Switch[qs["StateType"], "Vector", Normalize[state], "Matrix", normalizeMatrix[state], _, state],
        qs["Basis"]
    ]
]

QuantumStateProp[qs_, "NormalizedAmplitudes"] := Enclose @ With[{amplitudes = qs["Amplitudes"]},
    ConfirmQuiet[amplitudes / Norm[Values[amplitudes]], Power::infy]
]

QuantumStateProp[qs_, "NormalizedStateVector"] := Normalize @ qs["StateVector"]

QuantumStateProp[qs_, "CanonicalStateVector"] := qs["StateVector"] / First[SparseArray[Chop @ qs["StateVector"]]["ExplicitValues"], 1]

QuantumStateProp[qs_, "NormalizedDensityMatrix"] := Quiet @ Enclose @ Confirm[normalizeMatrix @ qs["DensityMatrix"]]

QuantumStateProp[qs_, "Operator", args___] := QuantumOperator[qs["Projector"], args]

QuantumStateProp[qs_, "NormalizedOperator"] := qs["NormalizedProjector"]["Amplitudes"]


(* density matrix *)

QuantumStateProp[qs_, "DensityMatrix"] /; qs["StateType"] === "Vector" :=
    With[{state = qs["StateVector"]}, KroneckerProduct[state, Conjugate[state]]]

QuantumStateProp[qs_, "DensityMatrix"] /; qs["StateType"] === "Matrix" := qs["State"]

QuantumStateProp[qs_, "DensityTensor"] := ArrayReshape[qs["DensityMatrix"], Join[qs["Dimensions"], qs["Dimensions"]]]

QuantumStateProp[qs_, "Projector"] := QuantumState[Flatten @ qs["DensityMatrix"],
    QuantumBasis[qs["Basis"],
        "Output" -> QuantumTensorProduct[qs["Output"], qs["Input"]],
        "Input" -> QuantumTensorProduct[qs["Output"]["Dual"], qs["Input"]["Dual"]]]
]

QuantumStateProp[qs_, "NormalizedProjector"] := QuantumState[Flatten @ qs["NormalizedDensityMatrix"], QuantumBasis[qs["Basis"], "Input" -> qs["Output"]["Dual"]]]

QuantumStateProp[qs_, "MatrixDimensions"] := Replace[{qs["Dimension"], qs["Dimension"]}, {0, 0} -> {1, 0}]

QuantumStateProp[qs_, "Eigenvalues"] := Eigenvalues[qs["DensityMatrix"]]

QuantumStateProp[qs_, "Eigenvectors", opts___] := eigenvectors[qs["DensityMatrix"], opts]

QuantumStateProp[qs_, "Eigensystem", opts___] := eigensystem[qs["DensityMatrix"], opts]

QuantumStateProp[qs_, "Eigenstates", opts___] := QuantumState[#, qs["Basis"]] & /@ qs["Eigenvectors", opts]


(* entropy *)

QuantumStateProp[qs_, "VonNeumannEntropy" | "Entropy", logBase_ ? NumericQ] := Enclose[With[{
        matrix = qs["NormalizedDensityMatrix"]
    },
    If[
        qs["PureStateQ"],
        0,
        ConfirmAssert[qs["Dimension"] < 2 ^ 10];
        Enclose[Chop @ Simplify @ - Tr[matrix . ConfirmBy[MatrixLog[matrix, Method -> "Jordan"], MatrixQ]] / Log[logBase], $Failed &]
    ]
],
    Indeterminate &
]

QuantumStateProp[qs_, "VonNeumannEntropy" | "Entropy"] := Quantity[qs["VonNeumannEntropy", 2], "Bits"]

QuantumStateProp[qs_, {"VonNeumannEntropy" | "Entropy", logBase_}] := qs["VonNeumannEntropy", logBase]


(* purity *)

QuantumStateProp[qs_, "Purity"] := Enclose[
    If[ qs["StateType"] === "Vector",
        1,
        ConfirmAssert[qs["Dimension"] < 2 ^ 10];
        Simplify @ Abs[Tr[MatrixPower[ConfirmBy[qs["NormalizedDensityMatrix"], MatrixQ], 2]]]
    ],
    Indeterminate &
]

QuantumStateProp[qs_, "Type"] := Which[
    qs["Dimension"] == 0,
    "Empty",
    AllTrue[qs["State"], TrueQ[# == 0] &, If[qs["VectorQ"], 1, 2]],
    (* TrueQ[qs["Purity"] == Indeterminate], *)
    "Degenerate",
    qs["VectorQ"] || PositiveSemidefiniteMatrixQ[N @ qs["DensityMatrix"]] && TrueQ[qs["Purity"] == 1],
    "Pure",
    PositiveSemidefiniteMatrixQ[N @ qs["DensityMatrix"]],
    "Mixed",
    True,
    "Unknown"
]

QuantumStateProp[qs_, "PureStateQ"] := qs["Type"] === "Pure"

QuantumStateProp[qs_, "MixedStateQ"] := qs["Type"] === "Mixed"

QuantumStateProp[qs_, "DegenerateStateQ"] := qs["Type"] === "Degenerate"

QuantumStateProp[qs_, "EmptyStateQ"] := qs["Type"] === "Empty"

QuantumStateProp[qs_, "UnknownQ"] := qs["Type"] === "Unknown"


(* transforms *)

QuantumStateProp[qs_, "Computational"] := If[
    qs["Basis"]["ComputationalQ"],
    qs,
    QuantumState[qs,
        "Output" -> QuditBasis[qs["OutputDimensions"]],
        "Input" -> QuditBasis[qs["InputDimensions"]]["Dual"]
    ]
]

QuantumStateProp[qs_, "Canonical"] := QuantumState[qs, qs["Basis"]["Canonical"]]


QuantumStateProp[qs_, "SchmidtBasis", dim : _Integer | Automatic : Automatic] /; dim === Automatic || Divisible[qs["Dimension"], dim] := Module[{
    uMatrix, alphaValues, wMatrix,
    n = Replace[dim, Automatic -> If[
        qs["Qudits"] > 1,
        First @ qs["Dimensions"],
        Replace[Divisors[qs["Dimension"]], {{1, d_} :> d, {___, d_, _} :> d}]
    ]],
    m,
    state = qs["Computational"],
    mat
},
    m = qs["Dimension"] / n;
    mat = If[ qs["PureStateQ"] || qs["DegenerateStateQ"],
        ArrayReshape[state["StateVector"], {n, m}],
        ArrayReshape[
            Transpose[ArrayReshape[state["DensityMatrix"], {n, m, n, m}], 2 <-> 3],
            {n, m} ^ 2
        ]
    ];
    {uMatrix, alphaValues, wMatrix} = SingularValueDecomposition[mat];
    QuantumState[
        If[ qs["PureStateQ"],
            Flatten @ alphaValues,
            ArrayReshape[Transpose[ArrayReshape[alphaValues, {n, n, m, m}], 2 <-> 3], {n * m, n * m}]
        ],
        QuantumBasis[
            {
                QuditBasis[Association @ MapIndexed[Subscript["u", First @ #2] -> #1 &, Transpose[uMatrix]]],
                QuditBasis[Association @ MapIndexed[Subscript["v", First @ #2] -> #1 &, ConjugateTranspose[wMatrix]]]
            }
        ]
    ]
]


primeFactors[n_] := Catenate[Table @@@ FactorInteger[n]]

QuantumStateProp[qs_, "PrimeBasis"] := QuantumState[qs, QuantumBasis[primeFactors[qs["OutputDimension"]], primeFactors[qs["InputDimension"]]]]


QuantumStateProp[qs_, "Bipartition", bipartition : {{_Integer..}, {_Integer..}}] := With[{
    qudits = Catenate @ bipartition,
    allQudits = Range[qs["Qudits"]]
},
    With[{trace = Complement[allQudits, qudits]},
        QuantumPartialTrace[qs, trace][
            "Bipartition",
            bipartition[[1]] /. Thread[
                Join[Complement[allQudits, trace], trace] -> allQudits
            ]
        ]
    ] /; DuplicateFreeQ[qudits] && ContainsAll[allQudits, qudits]
]

QuantumStateProp[qs_, "Bipartition", qudits : {_Integer..}] /; ContainsAll[Range[qs["Qudits"]], qudits] :=
    qs["Split", qs["Qudits"]]["Permute", FindPermutation[Join[qudits, Complement[Range[qs["Qudits"]], qudits]]]]["Bipartition", Times @@ qs["Dimensions"][[qudits]]]

QuantumStateProp[qs_, "Bipartition", dim : _Integer | Automatic : Automatic] := If[
    qs["Qudits"] == 2,
    qs,

    If[ qs["VectorQ"],
        qs["SchmidtBasis", dim],
        (qs["Eigenvalues"] . (#["SchmidtBasis", dim]["MatrixState"] & /@ qs["Eigenstates"]))
    ]
]

QuantumStateProp[qs_, "SpectralBasis"] := QuantumState[
    Chop @ SparseArray @ If[qs["VectorQ"], qs["Eigenvalues"], DiagonalMatrix[qs["Eigenvalues"]]],
    QuantumBasis[Association @ Catenate @ MapIndexed[
            If[ qs["InputDimension"] == 1,
                Subscript["s", First @ #2],
                QuditName[QuditName[Subscript["s", First @ #2]], QuditName[Subscript["s", Last @ #2]]["Dual"]]
            ] -> #1 &,
            Partition[qs["Eigenvectors"], qs["InputDimension"]],
            {2}
        ]
    ]
]

QuantumStateProp[qs_, "UniformBasis"] /; qs["VectorQ"] := QuantumState[Table[1 / Sqrt[qs["Dimension"]], qs["Dimension"]],
    QuantumBasis[AssociationThread[Array[Subscript["u", #] &, qs["Dimension"]], Normal[DiagonalMatrix[Sqrt[qs["Dimension"]] qs["StateVector"]]]]]
]

QuantumStateProp[qs_, "Disentangle"] /; qs["PureStateQ"] := With[{s = qs["SchmidtBasis"]},
	MapThread[QuantumState[SparseArray[#1 -> #2, s["Dimension"]], s["Basis"]] &, {s["StateVector"]["ExplicitPositions"], s["StateVector"]["ExplicitValues"]}]
]

QuantumStateProp[qs_, "Decompose", {}] := {{qs}}

QuantumStateProp[qs_, "Decompose", dims_List] /; qs["PureStateQ"] || qs["DegenerateStateQ"] := If[Length[dims] == 1, {{qs}}, Module[{s = qs["SchmidtBasis", First[dims]], basis},
    basis = s["Basis"]["Decompose"];
    If[s["DegenerateStateQ"], Return[{QuantumState[0, #] & /@ basis}]];
	MapIndexed[{v, idx} |->
		Splice[Catenate /@ Tuples[If[Length[basis] == 2, MapAt[#["Decompose", {}] &, 1], Identity] @
            MapAt[#["Decompose", Rest[dims]] &, -1] @ MapIndexed[QuantumState[SparseArray[idx -> If[#2 === {1}, v, 1], #["Dimension"]], #] &, basis]]],
		s["StateVector"]["ExplicitValues"]
	]
]]

decompose[qs_, {}] := {1 -> {qs}}

decompose[qs_, dims_List] := If[Length[dims] == 1, {1 -> {qs}}, Module[{s = qs["SchmidtBasis", First[dims]], basis},
    basis = s["Basis"]["Decompose"];
    If[s["DegenerateStateQ"], Return[{1 -> (QuantumState[0, #] & /@ basis)}]];
	MapIndexed[{v, idx} |->
		With[{decomp = If[Length[basis] == 2, MapAt[decompose[#, {}] &, 1], Identity] @
            MapAt[decompose[#, Rest[dims]] &, -1] @ Map[QuantumState[SparseArray[idx -> 1, #["Dimension"]], #] &, basis]
        },
            Splice @ Thread[v Times @@@ Tuples[decomp[[All, All, 1]]] -> Catenate /@ Tuples[decomp[[All, All, 2]]]]
        ],
		s["StateVector"]["ExplicitValues"]
	]
]]

QuantumStateProp[qs_, "DecomposeWithProbabilities", dims_List] /; qs["PureStateQ"] || qs["DegenerateStateQ"] := SubsetMap[Normalize[#, Total] &, {All, 1}] @ decompose[qs, dims]

QuantumStateProp[qs_, "DecomposeWithAmplitudes", dims_List] /; qs["PureStateQ"] || qs["DegenerateStateQ"] := decompose[qs, dims]

QuantumStateProp[qs_, prop : "Decompose" | "DecomposeWithProbabilities" | "DecomposeWithAmplitudes"] /; qs["PureStateQ"] || qs["DegenerateStateQ"] := qs[prop, Catenate[Table @@@ FactorInteger[qs["Dimension"]]]]

QuantumStateProp[qs_, prop : "Decompose" | "DecomposeWithProbabilities" | "DecomposeWithAmplitudes", args___] /; qs["MixedStateQ"] := qs["Bend"][prop, args]


QuantumStateProp[qs_, "SchmidtDecompose"] := #1 Inactive[CircleTimes] @@ ##2 & @@@
		MapAt[MatrixForm @ ArrayReshape[#["Computational"]["StateVector"], If[qs["MatrixQ"], qs["MatrixNameDimensions"], qs["OutputDimensions"]]] &, {All, 2, All}] @
			If[qs["MatrixQ"], qs["Bend"], qs]["DecomposeWithProbabilities", {If[qs["MatrixQ"], qs["Dimension"], qs["OutputDimension"]]}] // Total // Chop


QuantumStateProp[qs_, "Compress"] := With[{decomp = qs["DecomposeWithProbabilities"]},
	QuantumState[
		decomp[[All, 1]],
		QuantumBasis @ QuditBasis[
            Array[Subscript[\[FormalC], #] &, Length[decomp]],
            Join @@@ Map[First @ Extract[#["Basis"]["Representations"], #["State"]["ExplicitPositions"]] &, decomp[[All, 2]], {2}]
        ]
	]
]

PrimesUpTo[n_Integer] := If[n < 2, {}, First @ NestWhile[{Append[#[[1]], Prime[#[[2]]]], #[[2]] + 1} &, {{2}, 2}, Last[#[[1]]] < n &]]

DimensionPartition[n_Integer] := Sort @ First @ TakeSmallestBy[IntegerPartitions[n, All, PrimesUpTo[n]], Apply[Times], 1]

QuantumStateProp[qs_, "Uncompress", ds : {_Integer ? Positive..} | Automatic : Automatic] /; qs["Qudits"] == 1 := Enclose @ Module[{
    qb = qs["Basis"]["QuditBasis"]["Canonical"],
    repr, d,
    dims
},
    repr = Values[qb["Representations"]];
    d = qb["ElementDimension"];
    dims = ConfirmBy[Replace[ds, Automatic -> DimensionPartition[d]], Total[#] == d &];
    QuantumState[
        qs["StateVector"] . Flatten /@ kroneckerProduct @@@ Map[TakeList[#, dims] &, repr],
        dims
    ]
]

QuantumStateProp[qs_, "Uncompress", dims : {_Integer ? Positive..} | Automatic : Automatic] := QuantumState[qs, QuantumBasis[qs["Dimension"]]]["Uncompress", dims]


QuantumStateProp[qs_, "Normalized" | "NormalizedState" | "Normalize"] :=
    QuantumState[If[qs["StateType"] === "Vector", qs["NormalizedStateVector"], qs["NormalizedDensityMatrix"]], qs["Basis"]]


QuantumStateProp[qs_, "Bend"] := QuantumState[Flatten @ qs["DensityMatrix"], QuantumTensorProduct[qs["Basis"], qs["Basis"]]]

QuantumStateProp[qs_, "BendDual"] := QuantumState[Flatten @ qs["DensityMatrix"], QuantumTensorProduct[qs["Basis"], qs["Basis"]["Dual"]]]


QuantumStateProp[qs_, "Double"] := QuantumTensorProduct[qs, qs["Dual"]]


QuantumStateProp[qs_, "Unbend"] := Enclose @ Which[
    qs["PureStateQ"] && IntegerQ[Sqrt[qs["Dimension"]]],
    With[{dimension = Sqrt[qs["Dimension"]]},
        QuantumState[
            ArrayReshape[qs["StateVector"], Table[dimension, 2]],
            QuantumBasis @ ConfirmBy[qs["QuditBasis"]["TakeDimension", dimension], QuditBasisQ]
        ]
    ],
    True,
    qs
]

QuantumStateProp[qs_, "Purify"] := Sqrt[qs]["Bend"]

QuantumStateProp[qs_, "Unpurify"] := qs["Unbend"] ^ 2


QuantumStateProp[qs_, "Pure"] := If[qs["PureStateQ"] || qs["DegenerateStateQ"], qs, qs["Bend"]]

QuantumStateProp[qs_, "Mixed"] := If[qs["MixedStateQ"], qs, qs["Unbend"]]


QuantumStateProp[qs_, "VectorState"] := If[qs["StateType"] === "Vector", qs, QuantumState[qs["StateVector"], qs["Basis"]]]

QuantumStateProp[qs_, "MatrixState"] := If[qs["StateType"] === "Matrix", qs, QuantumState[qs["DensityMatrix"], qs["Basis"]]]

QuantumStateProp[qs_, "Transpose"] := With[{qb = qs["Basis"]["Transpose"]},
    QuantumState[If[qs["StateType"] === "Vector", Flatten, ArrayReshape[#, qb["MatrixDimensions"]] &] @ Transpose[qs["StateMatrix"]], qb]
]

QuantumStateProp[qs_, "Transpose", qudits : {_Integer...}] := QuantumState[
    ArrayReshape[
        Transpose[ArrayReshape[qs["DensityMatrix"], Join[qs["Dimensions"], qs["Dimensions"]]], Cycles[{#, # + qs["Qudits"]} & /@ qudits]],
        {qs["Dimension"], qs["Dimension"]}
    ],
    qs["Basis"]
]

QuantumStateProp[qs_, "ReverseOutput"] := qs["PermuteOutput", FindPermutation[Reverse @ Range @ qs["OutputQudits"]]]

QuantumStateProp[qs_, "ReverseInput"] := qs["PermuteInput", FindPermutation[Reverse @ Range @ qs["InputQudits"]]]

QuantumStateProp[qs_, "Reverse"] := qs["ReverseOutput"]["ReverseInput"]

QuantumStateProp[qs_, "Trace"] := QuantumPartialTrace[qs]

QuantumStateProp[qs_, "Trace", qudits : {_Integer...}] := QuantumPartialTrace[qs, qudits]

QuantumStateProp[qs_, "Discard", qudits : {_Integer...}] /; ContainsOnly[qudits, Range[qs["OutputQudits"]]] :=
    QuantumOperator["Discard", qudits, qs["OutputDimensions"][[qudits]]][qs]

QuantumStateProp[qs_, "Dual", args___] := QuantumState[Conjugate[qs["State"]], qs["Basis"]["Dual", args]]

QuantumStateProp[qs_, "Conjugate"] := QuantumState[Conjugate[qs["State"]], qs["Basis"]]


QuantumStateProp[qs_, "ConjugateTranspose" | "Dagger"] := With[{qb = qs["Basis"]["ConjugateTranspose"]},
    QuantumState[
        If[qs["VectorQ"], Flatten @ Conjugate @ qs["Transpose"]["State"], ConjugateTranspose @ qs["State"]],
        qb
    ]
]

QuantumStateProp[qs_, "Physical"] := If[qs["PhysicalQ"], qs,
	Block[{d, u},
		{d, u} = eigensystem[qs["NormalizedDensityMatrix"], Chop -> True, "Normalize" -> True];
		d = Normalize[Max[#, 0] & /@ Re[d], Total];
		QuantumState[Transpose[u] . DiagonalMatrix[d] . Conjugate[u] // Chop, qs["Basis"]]
    ]
]

QuantumStateProp[qs_, "EigenPrune", n : _Integer ? Positive : 1] :=
	Block[{d, u, p},
		{d, u} = eigensystem[qs["DensityMatrix"], Chop -> True, "Normalize" -> True];
        p = PositionLargest[Abs[d], n];
		d = Normalize[Extract[d, p], Total];
        u = Extract[u, p];
		QuantumState[Transpose[u] . DiagonalMatrix[d] . Conjugate[u] // Chop, qs["Basis"]]
    ]


QuantumStateProp[qs_, "TensorReverseOutput", qudits : {_Integer...}] := QuantumState[
    If[qs["VectorQ"], SparseArrayFlatten, ArrayReshape[#, qs["MatrixDimensions"]] &] @
        Reverse[qs["StateTensor"], If[qs["VectorQ"], qudits, Join[qudits, qs["Qudits"] + qudits]]],
    qs["Basis"]
]

QuantumStateProp[qs_, "TensorReverseInput", qudits : {_Integer...}] := QuantumState[
    If[qs["VectorQ"], SparseArrayFlatten, ArrayReshape[#, qs["MatrixDimensions"]] &] @
        Reverse[qs["StateTensor"], qs["OutputQudits"] + If[qs["VectorQ"], qudits, Join[qudits, qs["Qudits"] + qudits]]],
    qs["Basis"]
]


QuantumStateProp[qs_, "Permute", perm_Cycles] := If[
    perm === Cycles[{}],
    qs,
    QuantumState[
        If[ qs["PureStateQ"],
            SparseArrayFlatten @ Transpose[qs["StateTensor"], perm],
            ArrayReshape[
                Transpose[qs["StateTensor"], PermutationCycles[With[{list = PermutationList[perm, qs["Qudits"]]}, Join[list, list + qs["Qudits"]]]]],
                Table[qs["Dimension"], 2]
            ]
        ],
        qs["Basis"]["Permute", perm]
    ]
]

QuantumStateProp[qs_, "PermuteOutput", perm_Cycles] := If[
    perm === Cycles[{}],
    qs,
    QuantumState[
        If[ qs["VectorQ"],
            SparseArrayFlatten @ Transpose[qs["StateTensor"], perm],
            ArrayReshape[
                Transpose[qs["StateTensor"], PermutationCycles[With[{list = PermutationList[perm, qs["Qudits"]]}, Join[list, list + qs["Qudits"]]]]],
                Table[qs["Dimension"], 2]
            ]
        ],
        qs["Basis"]["PermuteOutput", perm]
    ]
]

QuantumStateProp[qs_, "PermuteInput", perm_Cycles] := If[
    perm === Cycles[{}],
    qs,
    QuantumState[
        With[{inPerm = PermutationCycles @ Join[Range[qs["OutputQudits"]], PermutationList[perm, qs["InputQudits"]] + qs["OutputQudits"]]},
            If[ qs["VectorQ"],
                SparseArrayFlatten @ Transpose[qs["StateTensor"], inPerm],
                ArrayReshape[
                    Transpose[qs["StateTensor"], PermutationCycles[With[{list = PermutationList[inPerm, qs["Qudits"]]}, Join[list, list + qs["Qudits"]]]]],
                    Table[qs["Dimension"], 2]
                ]
            ]
        ],
        qs["Basis"]["PermuteInput", perm]
    ]
]


QuantumStateProp[qs_, "Split", n_Integer : 0] := With[{basis = qs["Basis"]["Split", n]},
    QuantumState[qs["State"], basis]
    (* QuantumState[QuantumState[qs["Computational"]["State"], QuantumBasis[basis["OutputDimensions"], basis["InputDimensions"]]], basis] *)
]

QuantumStateProp[qs_, "SplitDual", n_Integer : 0] := With[{basis = qs["Basis"]["SplitDual", n]},
    QuantumState[qs["State"], basis]
]


QuantumStateProp[qs_, "UnstackOutput", n_Integer : 1] /; 1 <= n <= qs["OutputQudits"] :=
    Module[{state = If[n == 1, qs, qs["PermuteOutput", FindPermutation[RotateLeft[Range[qs["OutputQudits"]], n - 1]]]], basis},
        basis = QuantumBasis[state["Basis"], "Output" -> Last @ state["Output"]["Split", 1]];
        QuantumState[#, basis] & /@ ArrayReshape[state["State"], {First @ state["Dimensions"], Times @@ Rest @ state["Dimensions"]}]
    ]

QuantumStateProp[qs_, "UnstackInput", n_Integer : 1] /; 1 <= n <= qs["InputQudits"] :=
    #["Transpose"] & /@ qs["Transpose"]["UnstackOutput", n]


(* representations *)

QuantumStateProp[qs_, "StateTensor"] := If[
    qs["VectorQ"],
    ArrayReshape[qs["StateVector"], qs["Dimensions"]],
    ArrayReshape[qs["DensityMatrix"], Join[qs["Dimensions"], qs["Dimensions"]]]
]

QuantumStateProp[qs_, "StateMatrix"] := If[
    qs["VectorQ"],
    ArrayReshape[qs["StateVector"], qs["MatrixNameDimensions"]],
    ArrayReshape[
        Transpose[qs["StateTensor"], FindPermutation[
            With[{input = Join[Range[qs["OutputQudits"]], qs["Qudits"] + Range[qs["OutputQudits"]]]},
                Join[input, Complement[Range[2 qs["Qudits"]], input]]
            ]
        ]],
        qs["MatrixNameDimensions"] ^ 2
    ]
]


QuantumStateProp[qs_, "Matrix"] := qs["Computational"]["StateMatrix"]

QuantumStateProp[qs_, "VectorRepresentation"] := qs["Computational"]["StateVector"]

QuantumStateProp[qs_, "MatrixRepresentation"] := qs["Computational"]["DensityMatrix"]

QuantumStateProp[qs_, "Tensor" | "TensorRepresentation"] := qs["Computational"]["StateTensor"]

QuantumStateProp[qs_, "NormalizedMatrixRepresentation"] := normalizeMatrix @ qs["MatrixRepresentation"]


(* block sphere*)

QuantumStateProp[qs_, "BlochSphericalCoordinates"] /; qs["Dimension"] == 2 := With[{
    state = Simplify @ Normal[qs["Computational"]["NormalizedStateVector"]],
    matrix = Simplify @ Normal[qs["Computational"]["NormalizedDensityMatrix"]]
},
    If[
        qs["PureStateQ"],

        With[{
            alpha = state[[1]], beta = state[[2]]
        },
            {1, 2 ArcCos[Abs[alpha]], Arg[beta] - Arg[alpha]}
        ],

        With[{
            u = 2 Re[matrix[[1, 2]]], v = 2 Im[matrix[[2, 1]]], w = Re[matrix[[1, 1]] - matrix[[2, 2]]]
        },
            If[ u == v == w == 0,
                {0, 0, 0},
                Map[Abs, CoordinateTransformData["Cartesian" -> "Spherical", "Mapping", {u, v, w}]]
            ]
        ]
    ]
]

QuantumStateProp[qs_, "BlochCartesianCoordinates"] /; qs["Dimension"] == 2 :=  With[{
    state = Simplify @ Normal[qs["Computational"]["NormalizedStateVector"]],
    matrix = Simplify @ Normal[qs["Computational"]["NormalizedDensityMatrix"]]
},
    If[
        qs["PureStateQ"],

        Module[{
            alpha = state[[1]], beta = state[[2]], theta, phi
        },
            {theta, phi} = {2 ArcCos[Abs[alpha]], Arg[beta] - Arg[alpha]};
            {Sin[theta] Cos[phi], Sin[theta] Sin[phi], Cos[theta]}
        ],

        With[{
            u = 2 Re[matrix[[1, 2]]], v = 2 Im[matrix[[2, 1]]], w = Re[matrix[[1, 1]] - matrix[[2, 2]]]
        },
            {u, v, w}
        ]
    ]
]


Options[BlochPlot] = Join[{"ShowLabels" -> True, "ShowGreatCircles" -> True, "ShowAxes" -> True}, Options[Graphics3D]]

BlochPlot[qs_, opts : OptionsPattern[]] := Module[{
    greatCircles, referenceStates, u, v, w
},
    greatCircles = If[
        TrueQ[OptionValue["ShowGreatCircles"]],
        ParametricPlot3D[
            {{Cos[t], Sin[t], 0}, {0, Cos[t], Sin[t]}, {Cos[t], 0, Sin[t]}},
            {t, 0, 2 Pi},
            PlotStyle -> ConstantArray[{Black, Thin}, 3]
        ],
        Nothing
    ];
    referenceStates = Graphics3D[{
        Opacity[0.4], Sphere[],Black, Thickness[0.0125], Opacity[1.0],
        If[ TrueQ[OptionValue["ShowAxes"]],
            Splice @ {Line[{{0, 1, 0}, {0, -1, 0}}], Line[{{0, 0, 1}, {0, 0, -1}}], Line[{{1, 0, 0}, {-1, 0, 0}}]},
            Nothing
        ],
        If[ TrueQ[OptionValue["ShowLabels"]],
            Splice @ {
                Text[Ket[{0}], {0, 0, 1.3}],  Text[Ket[{1}], {0, 0, -1.3}],
                Text[Ket[{"R"}], {0, 1.3, 0}], Text[Ket[{"L"}], {0, -1.3, 0}],
                Text[Ket[{"+"}], {1.3, 0, 0}], Text[Ket[{"-"}], {-1.3, 0, 0}]
            },
            Nothing
        ],
        Red, Arrowheads[0.05], Arrow[Tube[{{0, 0, 0}, {u, v, w}}, 0.03], {0, -0.01}]
    }
    ];
    {u, v, w} = qs["BlochCartesianCoordinates"];
    Show[{greatCircles, referenceStates},
        FilterRules[{opts}, Options[Graphics3D]],
        PlotRange -> All,
        ViewPoint -> {1, 1, 1},
        Axes -> False,
        Boxed -> False,
        PlotRange -> {{-1.7, 1.7}, {-1.7, 1.7}, {-1.7, 1.7}}
    ]
]

QuantumStateProp[qs_, "BlochPlot" | "BlochSpherePlot", opts : OptionsPattern[BlochPlot]] /; qs["Dimension"] == 2 := BlochPlot[qs, opts]

(* basis properties *)

QuantumStateProp[qs_, prop_ ? propQ, args___] /;
    MatchQ[prop, Alternatives @@ Intersection[QuantumBasis["Properties"], QuantumState["Properties"]]] := qs["Basis"][prop, args]

