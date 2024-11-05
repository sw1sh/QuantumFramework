Package["Wolfram`QuantumFramework`"]



$QuantumStateProperties = {
    "StateType", "State", "Basis",
    "Amplitudes", "Amplitude", "Weights", "Probabilities", "Probability", "StateVector", "DensityMatrix",
    "AmplitudesList", "AmplitudeList", "ProbabilitiesList", "ProbabilityList",
    "NormalizedState", "NormalizedAmplitudes", "NormalizedStateVector", "NormalizedDensityMatrix",
    "Entropy", "VonNeumannEntropy", "LogicalEntropy",
    "Purity", "Type", "PureStateQ", "MixedStateQ", "MatrixQ", "VectorQ", "UnknownQ", "PhysicalQ",
    "NumericQ", "NumberQ",
    "Kind", "Scalar",
    "Norm", "TraceNorm", "NormalizedQ",
    "BlochSphericalCoordinates", "BlochCartesianCoordinates",
    "Projector", "NormalizedProjector",
    "Operator", "NormalizedOperator",
    "Eigenvalues", "Eigenvectors", "Eigenstates",
    "Computational", "SchmidtBasis", "SpectralBasis", "PrimeBasis", "UniformBasis",
    "StateTensor", "StateMatrix",
    "VectorState", "MatrixState",
    "Tensor", "Matrix", "Table",
    "Purify", "Unpurify",
    "Bend", "BendDual", "Unbend", "Double",
    "Pure", "Mixed",
    "Trace", "Transpose", "Conjugate", "ConjugateTranspose", "Inverse",
    "Physical",
    "ReverseOutput", "ReverseInput", "Reverse",
    "TensorReverseOutput", "TensorReverseInput",
    "Split", "SplitDual",
    "Bipartition",
    "Disentangle", "Decompose", "DecomposeWithAmplitudes", "DecomposeWithProbabilities",
    "SchmidtDecompose",
    "Formula", "Simplify", "FullSimplify",
    "Diagram", "CircuitDiagram",
    "BlochPlot", "AmplitudePlot", "ProbabilityPlot",
    "PieChart", "SectorChart"
};

QuantumState["Properties"] := Union @ Join[$QuantumStateProperties, QuantumBasis["Properties"]]


qs_QuantumState["ValidQ"] := QuantumStateQ[qs]


QuantumState::undefprop = "property `` is undefined for this state"

QuantumState::failprop = "property `` failed with ``"

(qs_QuantumState[prop_ ? propQ, args___]) /; QuantumStateQ[Unevaluated[qs]] := With[{
    result = QuantumStateProp[qs, prop, args]
},
    If[ TrueQ[$QuantumFrameworkPropCache] &&
        ! MemberQ[{"Properties", "Basis"}, prop] &&
        QuantumStateProp[qs, "Basis"]["ParameterArity"] == 0,
        Quiet[QuantumStateProp[qs, prop, args] = result, Rule::rhs],
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

QuantumStateProp[qs_, "NumericQ"] := ArrayQ[Replace[qs["State"], sa_ ? SparseArrayQ :> sa["ExplicitValues"]], 1 | 2, NumericQ]

QuantumStateProp[qs_, "NumberQ"] := ArrayQ[Replace[qs["State"], sa_ ? SparseArrayQ :> sa["ExplicitValues"]], 1 | 2, InexactNumberQ]


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

QuantumStateProp[qs_, "Amplitudes"] := Block[{s = qs["Pure"], result},
    result = Enclose @ Association @ Thread[s["ElementNames"] -> ConfirmBy[Chop @ s["StateVector"], VectorQ]];
    result /; ! FailureQ[result]
]

QuantumStateProp[qs_, "Amplitude"] := Block[{s = qs["Pure"], v},
    v = Enclose @ ConfirmBy[Chop @ s["StateVector"], VectorQ];
    AssociationThread[
        s["Names", QuotientRemainder[Catenate @ v["ExplicitPositions"] - 1, s["InputDimension"]] + 1],
        v["ExplicitValues"]
    ] /; ! FailureQ[v]
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

QuantumStateProp[qs_, "AmplitudeList"] := Values @ qs["Amplitude"]

QuantumStateProp[qs_, "AmplitudesList"] := Normal @ qs["StateVector"]

QuantumStateProp[qs_, "Scalar" | "Number"] /; qs["Kind"] === "Scalar" := First[Flatten[qs["State"]]]

QuantumStateProp[qs_, "Weights"] := Which[
    qs["PureStateQ"] || qs["VectorQ"] && ! qs["NumericQ"],
    Abs[qs["StateVector"]] ^ 2,
    qs["PhysicalQ"] || ! qs["NumericQ"],
    Abs @ Diagonal @ qs["DensityMatrix"],
    True,
    qs["Physical"]["Weights"]
]

QuantumStateProp[qs_, "Weight"] := Normalize[qs["Weights"], Total]

QuantumStateProp[qs_, "Probabilities"] := AssociationThread[qs["Names"], qs["ProbabilityList"]]

QuantumStateProp[qs_, "ProbabilityList" | "ProbabilitiesList"] := Normal @ qs["Weight"]

QuantumStateProp[qs_, "ProbabilityAssociation" | "Probability"] := With[{proba = Chop @ SparseArray @ qs["Weight"]},
    AssociationThread[
        qs["Names", QuotientRemainder[Catenate @ proba["ExplicitPositions"] - 1, qs["InputDimension"]] + 1],
        proba["ExplicitValues"]
    ]
]

QuantumStateProp[qs_, "Distribution"] := CategoricalDistribution[qs["Names"], qs["ProbabilitiesList"]]

QuantumStateProp[qs_, "MultivariateDistribution"] := CategoricalDistribution[Comap[qs["QuditBasis"]["Decompose"], "Names"], ArrayReshape[qs["ProbabilitiesList"], qs["Dimensions"]]]

QuantumStateProp[qs_, "PhaseSpace"] /; qs["Picture"] === "PhaseSpace" := Enclose @ With[{dims = ConfirmBy[Sqrt[qs["Dimensions"]], AllTrue[IntegerQ]]},
    Fold[
        With[{ds = Dimensions[#1][[#2]] {1, 1}, lds = Dimensions[#1][[;; #2 - 1]], rds = Dimensions[#1][[#2 + 2 ;;]]},
            If[ EvenQ[ds[[1]]],
                Block[{makeTensor},
                    makeTensor[t_] := TensorProduct[ConstantArray[1, lds, SparseArray], t, ConstantArray[1, rds, SparseArray]];
                    Join[
                        Join[#1, #1 makeTensor[SparseArray[{i_, _} :> (-1) ^ Mod[i + 1, 2], ds]], #2 + 1],
                        Join[#1 makeTensor[SparseArray[{_, j_} :> (-1) ^ Mod[j + 1, 2], ds]], #1 makeTensor[SparseArray[{i_, j_} :> (-1) ^ Mod[i + j, 2], ds]], #2 + 1],
                        #2
                    ]
                ],
                #1
            ]
        ] &,
        ArrayReshape[qs["StateVector"], Catenate[{#, #} & /@ dims]],
        2 Range[qs["Qudits"]] - 1
    ]
]

QuantumStateProp[qs_, "TransitionPhaseSpace"] /; qs["Picture"] === "PhaseSpace" := Enclose @ With[{dims = ConfirmBy[Sqrt[qs["Dimensions"]], AllTrue[IntegerQ]]},
    SparseArray @ Fold[
        With[{ds = Dimensions[#1][[#2]] {1, 1}, lds = Dimensions[#1][[;; #2 - 1]], rds = Dimensions[#1][[#2 + 2 ;;]]}, {d = ds[[1]]},
            Map[
                If[ OddQ[d],
                    #,
                    Transpose @ Permute[Transpose @ Permute[#, Riffle[Range[d], Range[d] + d]], Riffle[Range[d] + d, Range[d]]]
                ] &,
                If[ EvenQ[d],
                    Block[{makeTensor},
                        makeTensor[t_] := TensorProduct[ConstantArray[1, lds, SparseArray], t, ConstantArray[1, rds, SparseArray]];
                        Join[
                            Join[#1, #1 makeTensor[SparseArray[{i_, _} :> (-1) ^ Mod[i + 1, 2], ds]], #2 + 1],
                            Join[#1 makeTensor[SparseArray[{_, j_} :> (-1) ^ Mod[j + 1, 2], ds]], #1 makeTensor[SparseArray[{i_, j_} :> (-1) ^ Mod[i + j, 2], ds]], #2 + 1],
                            #2
                        ]
                    ],
                    Join[
                        Join[ConstantArray[0, Dimensions[#1]], #1, #2 + 1],
                        Join[ConstantArray[0, Dimensions[#1]], ConstantArray[0, Dimensions[#1]], #2 + 1],
                        #2
                    ]
                ],
                {#2 - 1}
            ]
        ] &,
        ArrayReshape[qs["StateVector"], Catenate[{#, #} & /@ dims]],
        2 Range[qs["Qudits"]] - 1
    ]
]

QuantumStateProp[qs_, "PrimePhaseSpace"] /; qs["Picture"] === "PhaseSpace" := Enclose @ With[{dims = ConfirmBy[Sqrt[qs["Dimensions"]], AllTrue[IntegerQ]]},
    Chop @ ArrayReshape[qs["StateVector"], Catenate[{#, #} & /@ dims]]
]

QuantumStateProp[qs_, "PrimeTransitionPhaseSpace" | "PrimeTransitionQuasiProbability"] /; qs["Picture"] === "PhaseSpace" := Enclose @ With[{
    mat = ArrayReshape[
        Transpose[
            ArrayReshape[qs["StateVector"], Catenate[{#, #} & /@ ConfirmBy[Sqrt[qs["Dimensions"]], AllTrue[IntegerQ]]]],
            FindPermutation[Join[Range[1, 2 #, 2], Range[2, 2 # - 1, 2]]] & @ qs["Qudits"]
        ],
        {#, #} & @ Sqrt[qs["Dimension"]]
    ]
},
    SparseArray @ Chop @ Join[
        Join[ConstantArray[0, Dimensions[mat]], mat, 2],
        Join[ConstantArray[0, Dimensions[mat]], ConstantArray[0, Dimensions[mat]], 2],
        1
    ]
]

QuantumStateProp[qs_, prop : "PhaseSpace" | "TransitionPhaseSpace", opts___] := QuantumWignerTransform[qs, opts][prop]

QuantumStateProp[qs_, "PrimePhaseSpace"] :=
    QuantumPhaseSpaceTransform[qs["PrimeBasis"], If[qs["NumberQ"], N, Identity] @ QuantumBasis["Wootters"[qs["Dimension"]]]]["PrimePhaseSpace"]

QuantumStateProp[qs_, "PrimeTransitionPhaseSpace" | "PrimeTransitionQuasiProbability"] :=
    QuantumPhaseSpaceTransform[qs["PrimeBasis"], If[qs["NumberQ"], N, Identity] @ QuantumBasis["Wootters"[qs["Dimension"]]]]["PrimeTransitionPhaseSpace"]

QuantumStateProp[qs_, "QuasiProbability", opts___] :=
    ArrayReshape[
        QuantumWignerTransform[QuantumState[qs["Split", qs["Qudits"]], qs["Dimension"]], opts, "Exact" -> ! qs["NumberQ"]]["PhaseSpace"],
        If[EvenQ[qs["Dimension"]], 2, 1] {1, 1} qs["Dimension"]
    ]

QuantumStateProp[qs_, "PrimeQuasiProbability"] := With[{n = Total[FactorInteger[qs["Dimension"]][[All, 2]]]},
    ArrayReshape[
        Transpose[qs["PrimePhaseSpace"], FindPermutation[Join[Range[1, 2 n, 2], Range[2, 2 n - 1, 2]]]],
        {1, 1} qs["Dimension"]
    ]
]

QuantumStateProp[qs_, "TransitionQuasiProbability", opts___] :=
    ArrayReshape[
        QuantumWignerTransform[QuantumState[qs["Split", qs["Qudits"]], qs["Dimension"]], opts, "Exact" -> ! qs["NumberQ"]]["TransitionPhaseSpace"],
        2 {1, 1} qs["Dimension"]
    ]


QuantumStateProp[qs_, "TransitionGraph", opts : OptionsPattern[]] := With[{
    q = qs["Qudits"], dims = qs["Dimensions"]
}, {
    vs = Join[QuditBasis["X"[dims]]["Names"], QuditBasis[dims]["Names"]],
    mat = qs[If[TrueQ[Lookup[{opts, "Prime" -> True}, "Prime"]], "PrimeTransitionQuasiProbability", "TransitionQuasiProbability"]] //
        If[TrueQ[Lookup[{opts, "Positive" -> False}, "Positive"]], UnitStep[#] # - Transpose[UnitStep[- #] #], #] &
}, {
    g = WeightedAdjacencyGraph[
        vs,
        Replace[Normal[mat], 0 | 0. -> Infinity, {2}],
        FilterRules[{opts}, Options[Graph]],
        EdgeLabelStyle -> {_ -> Background -> White},
        VertexShapeFunction -> Catenate @ MapThread[{xs, color} |-> Thread[xs -> Function[Inset[Framed[Style[#2, Black], Background -> color], #1, #3]]], {Partition[vs, Times @@ dims], {LightBlue, LightRed}}],
        VertexStyle -> Catenate @ MapThread[{xs, color} |-> Thread[xs -> color], {Partition[vs, Times @@ dims], {LightBlue, LightRed}}],
        PerformanceGoal -> "Quality"
    ]
},
    Graph[g, EdgeStyle -> (# -> Replace[Sign[AnnotationValue[{g, #}, EdgeWeight]], {0 | 1 -> Red, -1 -> Blue}] & /@ EdgeList[g])]
]

QuantumStateProp[qs_, "FullTransitionGraph", opts : OptionsPattern[]] := With[{
    q = qs["Qudits"], dims = If[qs["Picture"] === "PhaseSpace", Sqrt, Identity] @ qs["Dimensions"]
}, {
    vs = QuantumTensorProduct /@ Tuples[Join[QuditBasis["X"[#]]["Names"], QuditBasis[#]["Names"]] & /@ dims],
    tensor = qs[If[TrueQ[Lookup[{opts, "Prime" -> True}, "Prime"]], "PrimeTransitionPhaseSpace", "TransitionPhaseSpace"]] //
        If[TrueQ[Lookup[{opts, "Positive" -> False}, "Positive"]], UnitStep[#] # - Transpose[UnitStep[-#] #, Catenate[Reverse /@ Partition[Range[TensorRank[#]], 2]]], #] &
},  {
    g = WeightedAdjacencyGraph[
        vs,
        Replace[
            Normal @ ArrayReshape[Transpose[tensor, Riffle[Range[q], Range[q] + q]], Length[vs] {1, 1}],
            0 -> Infinity,
            {2}
        ],
        FilterRules[{opts}, Options[Graph]],
        EdgeLabelStyle -> {_ -> Background -> White},
        VertexShapeFunction -> Join[
            {_ -> Function[Inset[Framed[Style[#2, Black], Background -> LightGray], #1, #3]]},
            Thread[QuditBasis["X"[dims]]["Names"] -> Function[Inset[Framed[Style[#2, Black], Background -> LightBlue], #1, #3]]],
            Thread[QuditBasis[dims]["Names"] -> Function[Inset[Framed[Style[#2, Black], Background -> LightRed], #1, #3]]]
        ],
        PerformanceGoal -> "Quality"
    ]
},
    Graph[g, EdgeStyle -> (# -> Replace[Sign[AnnotationValue[{g, #}, EdgeWeight]], {0 | 1 -> Red, -1 -> Blue}] & /@ EdgeList[g])]
]


QuantumStateProp[qs_, "Formula", OptionsPattern[]] /; qs["DegenerateStateQ"] := RawBoxes[0]

QuantumStateProp[qs_, "Formula", OptionsPattern[]] /; qs["EmptyStateQ"] := RawBoxes[""]

MinusBoxQ[boxes_] := MatchQ[boxes, RowBox[{"-" | _ ? MinusBoxQ, __}]]

QuantumStateProp[qs_, "Formula", OptionsPattern[{"Normalize" -> False, "Form" -> StandardForm}]] := RawBoxes @ With[{s = qs["Pure"], form = OptionValue["Form"]},
    If[ s["Dimension"] == 0, 0,
        With[{v = SparseArray @ s[If[TrueQ[OptionValue["Normalize"]], "NormalizedStateVector", "StateVector"]], d = s["InputDimension"]},
            RowBox @ MapThread[
                With[{coef = Replace[Parenthesize[#2, form, Plus], {
                    RowBox[{"-", "1"}] -> "-",
                    "1" :> If[#3 > 1, "+", Nothing],
                    x : Except[_ ? MinusBoxQ] :> If[#3 > 1, Splice[{"+", x}], x]
                }]},
                    RowBox[{coef, ToBoxes[#1, form]}]
                ] &,
                {
                    With[{pos = Catenate @ v["ExplicitPositions"]}, s["Names", Thread[{Quotient[pos - 1, d] + 1, Mod[pos - 1, d] + 1}]]],
                    v["ExplicitValues"],
                    Range[v["ExplicitLength"]]
                }
            ]
        ]
    ]
]

QuantumStateProp[qs_, "GraphRule"] := Block[{v = qs["Pure"]["StateVector"], pos},
    pos = QuotientRemainder[Catenate @ v["ExplicitPositions"] - 1, qs["InputDimension"]] + 1;
    MapThread[
        #2[[1]] -> GraphProduct[#1, #2[[2]]] &,
        {
            AdjacencyGraph @* QuditAdjacencyMatrix /@ (Sqrt[v["ExplicitLength"]] v["ExplicitValues"]),
            qs["Basis"]["GraphRules", pos]
        }
    ]
]

QuantumStateProp[qs_, prop : "Simplify" | "FullSimplify" | "Chop" | "ComplexExpand", args___] :=
    QuantumState[Map[Symbol[prop][#, args] &, qs["State"], {If[qs["VectorQ"], 1, 2]}], qs["Basis"][prop]]

(* these work on SparseArrays directly *)
QuantumStateProp[qs_, prop : "Chop" | "ComplexExpand", args___] := QuantumState[Symbol[prop][qs["State"], args], qs["Basis"][prop]]


(* normalization *)

QuantumStateProp[qs_, "Norm"] := Simplify @ If[qs["StateType"] === "Vector", Norm[qs["StateVector"]], Tr @ qs["DensityMatrix"]]

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

QuantumStateProp[qs_, "CanonicalState"] := If[qs["PureStateQ"], QuantumState[qs["CanonicalStateVector"], qs["Basis"]], qs]

QuantumStateProp[qs_, "NormalizedDensityMatrix"] := Quiet @ Enclose @ Confirm[normalizeMatrix @ qs["DensityMatrix"]]

QuantumStateProp[qs_, "Operator", args___] := QuantumOperator[qs["Projector"], args]

QuantumStateProp[qs_, "NormalizedOperator"] := qs["NormalizedProjector"]["Amplitudes"]

QuantumStateProp[qs_, "Table"] := TableForm[qs["StateMatrix"], TableHeadings -> {qs["Output"]["Names"], qs["Input"]["Names"]}, TableAlignments -> Center]


(* density matrix *)

QuantumStateProp[qs_, "DensityMatrix"] /; qs["StateType"] === "Vector" :=
    With[{state = qs["StateVector"]}, KroneckerProduct[state, Conjugate[state]]]

QuantumStateProp[qs_, "DensityMatrix"] /; qs["StateType"] === "Matrix" := qs["State"]

QuantumStateProp[qs_, "DensityTensor"] := ArrayReshape[qs["DensityMatrix"], Join[qs["Dimensions"], qs["Dimensions"]]]

QuantumStateProp[qs_, "DensityMatrixTensor"] := ArrayReshape[qs["DensityMatrix"], Join[#, #] & @ qs["MatrixNameDimensions"]]

QuantumStateProp[qs_, "DensityVectorMatrix"] := ArrayReshape[Transpose[qs["DensityMatrixTensor"], 2 <-> 3], qs["MatrixNameDimensions"] ^ 2]

QuantumStateProp[qs_, "DensityVector"] := SparseArrayFlatten @ Transpose[qs["DensityMatrixTensor"], 2 <-> 3]

QuantumStateProp[qs_, "Projector"] := QuantumState[Flatten @ qs["DensityMatrix"],
    QuantumBasis[qs["Basis"],
        "Output" -> QuantumTensorProduct[qs["Output"], qs["Input"]["Dual"]],
        "Input" -> QuantumTensorProduct[qs["Output"]["Dual"], qs["Input"]]]
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


QuantumStateProp[qs_, "LogicalEntropy"] := 1 - Tr[MatrixPower[qs["NormalizedDensityMatrix"], 2]]


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
    qs["ParameterArity"] > 0,
    "Parametric",
    qs["MatrixQ"] && qs["OutputDimension"] == qs["InputDimension"],
    "Superoperator",
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
    QuantumState[qs["State"], QuantumBasis[qs["OutputDimensions"], qs["InputDimensions"], "Label" -> qs["Label"]]],
    QuantumState[qs,
        "Output" -> QuditBasis[qs["OutputDimensions"]],
        "Input" -> QuditBasis[qs["InputDimensions"]]["Dual"]
    ]
]

QuantumStateProp[qs_, "Canonical"] := QuantumState[qs, qs["Basis"]["Canonical"]]


QuantumStateProp[qs_, "SchmidtBasis", dim : _Integer | Automatic : Automatic] /; qs["VectorQ"] && dim === Automatic || Divisible[qs["Dimension"], dim] := Module[{
    uMatrix, alphaValues, wMatrix,
    n = Replace[dim, Automatic :> If[
        qs["Qudits"] > 1,
        First @ qs["Dimensions"],
        Replace[Divisors[qs["Dimension"]], {{1, d_} :> d, {___, d_, _} :> d}]
    ]],
    m,
    state = qs["Computational"],
    mat
},
    m = qs["Dimension"] / n;
    mat = ArrayReshape[state["StateVector"], {n, m}];
    {uMatrix, alphaValues, wMatrix} = SingularValueDecomposition[mat];
    QuantumState[
        Flatten @ alphaValues,
        QuantumBasis[
            {
                QuditBasis[Association @ MapIndexed[Subscript["u", First @ #2] -> #1 &, Transpose[uMatrix]]],
                QuditBasis[Association @ MapIndexed[Subscript["v", First @ #2] -> #1 &, ConjugateTranspose[wMatrix]]]
            },
            Sequence @@ qs["Basis"]["Options"]
        ]
    ]
]

QuantumStateProp[qs_, "SchmidtBasis", dim : _Integer | Automatic : Automatic] /; qs["MatrixQ"] :=
    qs["Eigenvalues"] . (#["SchmidtBasis", dim]["MatrixState"] & /@ qs["Eigenstates"])


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

QuantumStateProp[qs_, "Bipartition", dim : _Integer | Automatic : Automatic] := Which[
    qs["Qudits"] == 2,
    qs,
    qs["ComputationalQ"],
    QuantumState[qs, With[{d = Replace[dim, Automatic :> If[qs["Qudits"] > 1, First @ qs["Dimensions"], Replace[Divisors[qs["Dimension"]], {{1, d_} :> d, {___, d_, _} :> d}]]]}, {d, qs["Dimension"] / d}]],
    True,
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

QuantumStateProp[qs_, "Decompose", dims : {_Integer ? Positive ...}] /; qs["VectorQ"] || qs["DegenerateStateQ"] || qs["EmptyStateQ"] := Enclose @ If[Length[dims] == 1, {{qs}},
Block[{s = Confirm @ qs["SchmidtBasis", First[dims]], basis},
    basis = s["Basis"]["Decompose"];
    If[s["DegenerateStateQ"], Return[{QuantumState[0, #] & /@ basis}]];
	MapIndexed[{v, idx} |->
		Splice[Catenate /@ Tuples[If[Length[basis] == 2, MapAt[Confirm @ #["Decompose", {}] &, 1], Identity] @
            MapAt[Confirm @ #["Decompose", Rest[dims]] &, -1] @ MapIndexed[QuantumState[SparseArray[idx -> If[#2 === {1}, v, 1], #["Dimension"]], #] &, basis]]],
		s["StateVector"]["ExplicitValues"]
	]
]]

QuantumStateProp[qs_, "Decompose", All] := MapThread[QuantumState, {#, qs["QuditBasis"]["Decompose"]}] & /@ qs["Decompose", qs["Dimensions"]]


decompose[qs_, {}] := {1 -> {qs}}

decompose[qs_, dims : {_Integer ? Positive ...}] := Enclose @ If[Length[dims] == 1, {1 -> {qs}}, Block[{s = Confirm @ qs["SchmidtBasis", First[dims]], basis},
    basis = s["Basis"]["Decompose"];
    If[s["DegenerateStateQ"], Return[{1 -> (QuantumState[0, #] & /@ basis)}]];
	MapIndexed[{v, idx} |->
		With[{decomp = If[Length[basis] == 2, MapAt[Confirm @ decompose[#, {}] &, 1], Identity] @
            MapAt[Confirm @ decompose[#, Rest[dims]] &, -1] @ Map[QuantumState[SparseArray[idx -> 1, #["Dimension"]], #] &, basis]
        },
            Splice @ Thread[v Times @@@ Tuples[decomp[[All, All, 1]]] -> Catenate /@ Tuples[decomp[[All, All, 2]]]]
        ],
		s["StateVector"]["ExplicitValues"]
	]
]]

decompose[qs_, All] := MapAt[MapThread[QuantumState, {#, qs["QuditBasis"]["Decompose"]}] &, {All, 2}] @ decompose[qs, qs["Dimensions"]]


QuantumStateProp[qs_, "DecomposeWithProbabilities", dims : {_Integer ? Positive ...} | All] /; qs["VectorQ"] || qs["DegenerateStateQ"] := SubsetMap[Normalize[#, Total] &, {All, 1}] @ decompose[qs, dims]

QuantumStateProp[qs_, "DecomposeWithAmplitudes", dims : {_Integer ? Positive ...} | All] /; qs["VectorQ"] || qs["DegenerateStateQ"] := decompose[qs, dims]

QuantumStateProp[qs_, prop : "DecomposeWithProbabilities" | "DecomposeWithAmplitudes", dims : {_Integer ? Positive ...}] /; qs["MatrixQ"] :=
    Enclose @ SubsetMap[Normalize[#, Total] &, {All, 1}] @ Catenate @ MapThread[
        {p, v} |-> If[p == 0,
            Nothing,
            (* MapAt[QuantumState[Partition[#, Sqrt[Length[#]]] & @ #["Computational"]["StateVector"]] &, {All, 2, All}] @ *)
                MapAt[p # &, Confirm @ QuantumState[v, qs["Basis"]][prop, dims], {All, 1}]
        ],
        qs["Eigensystem"]
    ]

QuantumStateProp[qs_, prop : "Decompose" | "DecomposeWithProbabilities" | "DecomposeWithAmplitudes"] := qs[prop, primeFactors[qs["Dimension"]]]

QuantumStateProp[qs_, "SchmidtDecompose"] := #1 Inactive[CircleTimes] @@ ##2 & @@@
		MapAt[MatrixForm @ ArrayReshape[#["Computational"]["StateVector"], If[qs["MatrixQ"], qs["MatrixNameDimensions"], qs["OutputDimensions"]]] &, {All, 2, All}] @
			If[qs["MatrixQ"], qs["Bend"], qs]["DecomposeWithProbabilities", If[qs["MatrixQ"], qs["Dimensions"], qs["OutputDimensions"]]] // Total // Chop


QuantumStateProp[qs_, "BasisDecompose"] := With[{tensor = qs["StateTensor"], basis = qs["QuditBasis"]["Decompose"]},
	Switch[
		TensorRank[tensor],
		1, Return[{1 -> {qs}}],
		2, Block[{u, s, v},
			{u, s, v} = SingularValueDecomposition[tensor];
			s = SparseArray[Diagonal[s]];
			MapThread[#1 -> {#2, #3} &, {
				s["ExplicitValues"],
				QuantumState[#, First[basis]] & /@ Extract[Transpose[u], s["ExplicitPositions"]],
				QuantumState[#, Last[basis]] & /@ Extract[ConjugateTranspose[v], s["ExplicitPositions"]]
			}]
        ]
		,
		_,
		With[{restBasis = QuantumTensorProduct[Rest[basis]]},
			With[{p = #[[1]], decompose = QuantumState[#[[2, 2]], restBasis]["BasisDecompose"]},
				Splice[MapAt[Prepend[#[[2, 1]]], {All, 2}] @ MapAt[p * # &, {All, 1}] @ decompose]
			] & /@ QuantumState[qs, {First[basis], Times @@ Rest[basis]}]["BasisDecompose"]
		]
	]
]

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


QuantumStateProp[qs_, "Bend"] := simplifyLabel @ QuantumState[qs["DensityVector"], qs["Basis"]["Bend"], "Label" -> CircleTimes[qs["Label"], SuperStar[qs["Label"]]]]

QuantumStateProp[qs_, "BendDual"] := simplifyLabel @ QuantumState[qs["DensityVector"], qs["Basis"]["BendDual"], "Label" -> CircleTimes[qs["Label"], SuperStar[qs["Label"]]]]


QuantumStateProp[qs_, "Double"] := With[{out = qs["OutputQudits"], in = qs["InputQudits"]},
    qs["Bend"]["PermuteOutput", FindPermutation @ Riffle[Range[out], Range[out + 1, 2 out]]]["PermuteInput", FindPermutation @ Riffle[Range[in], Range[in + 1, 2 in]]]
]

QuantumStateProp[qs_, "Unbend"] := Enclose @ With[{out = Sqrt[qs["OutputDimension"]], in = Sqrt[qs["InputDimension"]]},
    If[
        qs["VectorQ"] && IntegerQ[out] && IntegerQ[in],
        QuantumState[
            ArrayReshape[
                Transpose[ArrayReshape[qs["StateVector"], {out, out, in, in}], {1, 3, 2, 4}],
                Table[out * in, 2]
            ],
            QuantumBasis[
                "Output" -> ConfirmBy[qs["Output"]["TakeDimension", out], QuditBasisQ],
                "Input" -> ConfirmBy[qs["Input"]["TakeDimension", in], QuditBasisQ],
                qs["Basis"]["Options"]
            ]
        ],
        qs
    ]
]

QuantumStateProp[qs_, "Undouble"] := Enclose @ With[{out = ConfirmBy[qs["OutputQudits"] / 2, IntegerQ], in = ConfirmBy[qs["InputQudits"] / 2, IntegerQ]},
    qs["PermuteOutput", PermutationCycles @ Riffle[Range[out], Range[out + 1, 2 out]]]["PermuteInput", PermutationCycles @ Riffle[Range[in], Range[in + 1, 2 in]]]["Unbend"]
]

QuantumStateProp[qs_, "Purify"] := Sqrt[qs]["Bend"]

QuantumStateProp[qs_, "Unpurify"] := qs["Unbend"] ^ 2


QuantumStateProp[qs_, "Pure"] := If[qs["PureStateQ"] || qs["DegenerateStateQ"] || qs["EmptyStateQ"], qs, qs["Bend"]]

QuantumStateProp[qs_, "Mixed"] := If[qs["MixedStateQ"], qs, qs["Unbend"]]


QuantumStateProp[qs_, "VectorState" | "ToVector" | "Vector"] := If[qs["VectorQ"], qs, QuantumState[qs["StateVector"], qs["Basis"]]]

QuantumStateProp[qs_, "MatrixState" | "ToMatrix"] := If[qs["MatrixQ"], qs, QuantumState[qs["DensityMatrix"], qs["Basis"]]]

QuantumStateProp[qs_, "Transpose"] := With[{qb = qs["Basis"]["Transpose"]},
    QuantumState[If[qs["VectorQ"], SparseArrayFlatten @ Transpose[qs["StateMatrix"]], ArrayReshape[Transpose[qs["DensityMatrixTensor"], {2, 1, 4, 3}], qb["MatrixDimensions"]]], qb]
]

QuantumStateProp[qs_, "Transpose", qudits : {_Integer...}] := QuantumState[
    ArrayReshape[
        Transpose[ArrayReshape[qs["DensityMatrix"], Join[qs["Dimensions"], qs["Dimensions"]]], Cycles[{#, # + qs["Qudits"]} & /@ qudits]],
        qs["MatrixDimensions"]
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

QuantumStateProp[qs_, prop : "Dual" | "Conjugate", args___] := QuantumState[Conjugate[qs["State"]], qs["Basis"][prop, args]]

QuantumStateProp[qs_, "ConjugateTranspose" | "Dagger"] := simplifyLabel @ QuantumState[qs["Conjugate"]["Transpose"], "Label" -> SuperDagger[qs["Label"]]]

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
        If[ qs["VectorQ"],
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

QuantumStateProp[qs_, prop : "Permute" | "PermuteOutput" | "PermuteInput", list_List] := qs[prop, FindPermutation[list]]


QuantumStateProp[qs_, "Split", n_Integer : 0] := With[{basis = qs["Basis"]["Split", n]},
    QuantumState[qs["State"], basis]
    (* QuantumState[QuantumState[qs["Computational"]["State"], QuantumBasis[basis["OutputDimensions"], basis["InputDimensions"]]], basis] *)
]

QuantumStateProp[qs_, "SplitDual", n_Integer : 0] := With[{basis = qs["Basis"]["SplitDual", n]},
    QuantumState[qs["State"], basis]
]

QuantumStateProp[qs_, "Inverse"] := QuantumState[qs["State"], qs["Basis"]["Inverse"]]


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
    qs["DensityTensor"]
]

QuantumStateProp[qs_, "StateMatrix"] := If[
    qs["VectorQ"],
    ArrayReshape[qs["StateVector"], qs["MatrixNameDimensions"]],
    ArrayReshape[
        Transpose[ArrayReshape[{qs["StateTensor"]}, Join[#, #] & @ qs["MatrixNameDimensions"]], 2 <-> 3],
        qs["MatrixNameDimensions"] ^ 2
    ]
]


QuantumStateProp[qs_, "Matrix"] := qs["Computational"]["StateMatrix"]

QuantumStateProp[qs_, "VectorRepresentation"] := qs["Computational"]["StateVector"]

QuantumStateProp[qs_, "MatrixRepresentation"] := qs["Computational"]["DensityMatrix"]

QuantumStateProp[qs_, "Tensor" | "TensorRepresentation"] := qs["Computational"]["StateTensor"]

QuantumStateProp[qs_, "NormalizedMatrixRepresentation"] := normalizeMatrix @ qs["MatrixRepresentation"]


(* Bloch sphere *)

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

QuantumStateProp[qs_, "BlochVector"] := With[{d = qs["Dimension"], rho = qs["Computational"]["DensityMatrix"]},
    If[d > 1, Sqrt[d / 2 / (d - 1)], 1] (Chop[Tr[rho . #]] & /@ GellMannMatrices[d])
]

QuantumStateProp[qs_, "BlochVectorWithPhase"] /; qs["Dimension"] == 2 := Append[
    qs["BlochVector"],
    If[qs["VectorQ"], Arg[First[qs["StateVector"]]], 0]
]

QuantumStateProp[qs_, "BlochCartesianCoordinates"] /; qs["Dimension"] == 2 := With[{
    state = Simplify @ Normal[qs["Computational"]["NormalizedStateVector"]],
    matrix = Simplify @ Normal[qs["Computational"]["NormalizedDensityMatrix"]]
},
    If[
        qs["VectorQ"],

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


QuantumStateProp[qs_, "CircuitDiagram", opts___] := QuantumCircuitOperator[{qs}]["Diagram", opts]

QuantumStateProp[qs_, "BlochPlot" | "BlochSpherePlot", opts : OptionsPattern[BlochPlot]] /; qs["Dimension"] == 2 := BlochPlot[qs["BlochVectorWithPhase"], opts]

QuantumStateProp[qs_, "AmplitudePlot" | "AmplitudeChart", opts : OptionsPattern[AmplitudeChart]] := AmplitudeChart[qs["Amplitude"], opts]

QuantumStateProp[qs_, "ProbabilityPlot" | "ProbabilityChart", opts : OptionsPattern[ProbabilityChart]] := ProbabilityChart[qs["Probability"], opts]

QuantumStateProp[qs_, "AmplitudesPlot" | "AmplitudesChart", opts : OptionsPattern[AmplitudeChart]] := AmplitudeChart[qs["Amplitudes"], opts]

QuantumStateProp[qs_, "ProbabilitiesPlot" | "ProbabilitiesChart", opts : OptionsPattern[ProbabilityChart]] := ProbabilityChart[qs["Probabilities"], opts]

QuantumStateProp[qs_, "PieChart", args___] := QuditPieChart[qs, args]

QuantumStateProp[qs_, "SectorChart", args___] := QuditSectorChart[qs, args]


(* basis properties *)

QuantumStateProp[qs_, prop_ ? propQ, args___] /;
    MatchQ[prop, Alternatives @@ Intersection[QuantumBasis["Properties"], QuantumState["Properties"]]] := qs["Basis"][prop, args]

