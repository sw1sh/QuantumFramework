Package["Wolfram`QuantumFramework`"]



$QuantumStateProperties = {
     "StateType", "State", "Basis",
     "Amplitudes", "Weights", "Probabilities", "StateVector", "DensityMatrix",
     "NormalizedState", "NormalizedAmplitudes", "NormalizedStateVector", "NormalizedDensityMatrix",
     "Entropy", "VonNeumannEntropy",
     "Purity", "Type", "PureStateQ", "MixedStateQ",
     "Kind",
     "Norm", "TraceNorm", "NormalizedQ",
     "BlochSphericalCoordinates", "BlochCartesianCoordinates",
     "BlochPlot",
     "Projector", "NormalizedProjector",
     "Operator", "NormalizedOperator",
     "Eigenvalues", "Eigenvectors", "Eigenstates",
     "Computational", "SchmidtBasis", "SpectralBasis",
     "StateTensor", "StateMatrix",
     "VectorState", "MatrixState",
     "Tensor", "Matrix",
     "Purify", "Unpurify",
     "Bend", "BendDual", "Unbend", "Double",
     "Pure", "Mixed",
     "Trace", "Transpose", "Conjugate", "ConjugateTranspose", "Reverse",
     "Bipartition",
     "Formula"
};

QuantumState["Properties"] := Union @ Join[$QuantumStateProperties, QuantumBasis["Properties"]]


qs_QuantumState["ValidQ"] := QuantumStateQ[qs]


QuantumState::undefprop = "property `` is undefined for this state"

QuantumState::failprop = "property `` failed with ``"

(qs_QuantumState[prop_ ? propQ, args___]) /; QuantumStateQ[qs] := With[{
    result = QuantumStateProp[qs, prop, args]
},
    If[TrueQ[$QuantumFrameworkPropCache], QuantumStateProp[qs, prop, args] = result, result] /;
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

QuantumStateProp[qs_, "VectorQ"] := qs["StateType"] === "Vector"

QuantumStateProp[qs_, "MatrixQ"] := qs["StateType"] === "Matrix"

QuantumStateProp[qs_, "UnknownQ"] := qs["StateType"] === "UnknownType"


QuantumStateProp[qs_, "Kind"] := Which[
    qs["InputQudits"] === qs["OutputQudits"] === 0,
    "Number",
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
        SparseArray @ First @ Pick[qs["Eigenvectors", "Normalize" -> qs["NormalizedQ"]], ConfirmBy[Map[# =!= 0 &, Chop[qs["Eigenvalues"]]], Apply[Or]]],
        qs["Type"] === "Degenerate",
        SparseArray[{}, qs["Dimension"]],
        True,
        SparseArrayFlatten @ qs["Bend"]["State"]
    ];
    result /; !FailureQ[result]
]

QuantumStateProp[qs_, "Weights"] := If[qs["PureStateQ"],
    Abs[qs["StateVector"]] ^ 2,
    Diagonal @ qs["DensityMatrix"]
]

QuantumStateProp[qs_, "Probabilities"] := Simplify /@ Re @ (qs["Weights"] / Total[qs["Weights"]])

QuantumStateProp[qs_, "Distribution"] := CategoricalDistribution[qs["Names"], qs["Probabilities"]]

QuantumStateProp[qs_, "Formula", OptionsPattern["Normalize" -> False]] := With[{s = qs["Pure"]},
    With[{v = SparseArray @ s[If[TrueQ[OptionValue["Normalize"]], "NormalizedStateVector", "StateVector"]], d = s["InputDimension"]},
        With[{pos = Catenate @ v["ExplicitPositions"]}, s["Names", Thread[{Quotient[pos - 1, d] + 1, Mod[pos - 1, d] + 1}]]] . v["ExplicitValues"]
    ]
]


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

QuantumStateProp[qs_, "CanonicalStateVector"] := qs["StateVector"] / First[SparseArray[Chop @ qs["StateVector"]]["ExplicitValues"]]

QuantumStateProp[qs_, "NormalizedDensityMatrix"] := Quiet @ Enclose @ Confirm[normalizeMatrix @ qs["DensityMatrix"]]

QuantumStateProp[qs_, "Operator"] := QuantumOperator[qs["Projector"]]

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

QuantumStateProp[qs_, "MatrixDimensions"] := {qs["Dimension"], qs["Dimension"]}

QuantumStateProp[qs_, "Eigenvalues"] := Eigenvalues[qs["DensityMatrix"]]

QuantumStateProp[qs_, "Eigenvectors", opts___] := eigenvectors[qs["DensityMatrix"], opts]

QuantumStateProp[qs_, "Eigenstates", opts___] := QuantumState[#, qs["Basis"]] & /@ qs["Eigenvectors", opts]


(* entropy *)

QuantumStateProp[qs_, "VonNeumannEntropy" | "Entropy", logBase_ ? NumericQ] := Enclose[With[{
        matrix = qs["NormalizedDensityMatrix"]
    },
    If[
        qs["PureStateQ"],
        0,
        ConfirmAssert[qs["Dimension"] < 2 ^ 10];
        Enclose[Chop @ Simplify @ - Tr[matrix . ConfirmBy[MatrixLog[matrix], MatrixQ]] / Log[logBase], $Failed &]
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
    AllTrue[qs["State"], TrueQ[# == 0] &, If[qs["VectorQ"], 1, 2]],
    (* TrueQ[qs["Purity"] == Indeterminate], *)
    "Degenerate",
    TrueQ[qs["Purity"] == 1],
    "Pure",
    PositiveSemidefiniteMatrixQ[N @ qs["DensityMatrix"]],
    "Mixed",
    True,
    "Unknown"
]

QuantumStateProp[qs_, "PureStateQ"] := qs["Type"] === "Pure"

QuantumStateProp[qs_, "MixedStateQ"] := qs["Type"] === "Mixed"

QuantumStateProp[qs_, "DegenerateStateQ"] := qs["Type"] === "Degenerate"


(* transforms *)

QuantumStateProp[qs_, "Computational"] := QuantumState[qs, QuantumBasis[
    "Output" -> QuditBasis[DeleteCases[qs["OutputDimensions"], 1]],
    "Input" -> QuditBasis[DeleteCases[qs["InputDimensions"], 1]]["Dual"]
]
]

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
    mat = If[ qs["VectorQ"],
        ArrayReshape[state["StateVector"], {n, m}],
        ArrayReshape[
            Transpose[ArrayReshape[state["DensityMatrix"], {n, m, n, m}], 2 <-> 3],
            {n, m} ^ 2
        ]
    ];
    {uMatrix, alphaValues, wMatrix} = SingularValueDecomposition[mat];
    QuantumState[
        If[ qs["VectorQ"],
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
        (qs["Eigenvalues"] . (#["SchmidtBasis", dim]["MatrixState"] & /@ qs["Eigenstates"]))["Computational"]
    ]
]

QuantumStateProp[qs_, "SpectralBasis"] := QuantumState[
    qs["Eigenvalues"],
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


QuantumStateProp[qs_, "Normalized" | "NormalizedState"] :=
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


QuantumStateProp[qs_, "Pure"] := If[qs["PureStateQ"], qs, qs["Bend"]]

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

QuantumStateProp[qs_, "Reverse"] :=
    qs["PermuteOutput", FindPermutation[Reverse @ Range qs["OutputQudits"]]]["PermuteInput", FindPermutation[Reverse @ Range qs["InputQudits"]]]

QuantumStateProp[qs_, "Trace"] := QuantumPartialTrace[qs]

QuantumStateProp[qs_, "Trace", qudits : {_Integer...}] := QuantumPartialTrace[qs, qudits]

QuantumStateProp[qs_, "Conjugate" | "Dual"] := With[{qb = qs["Basis"]["Dual"]},
    QuantumState[If[qs["StateType"] === "Vector", SparseArrayFlatten, ArrayReshape[#, qb["MatrixDimensions"]] &] @ Conjugate[qs["StateMatrix"]], qb]
]

QuantumStateProp[qs_, "ConjugateTranspose" | "Dagger"] := With[{qb = qs["Basis"]["ConjugateTranspose"]},
    QuantumState[
        If[qs["VectorQ"], Flatten @ Conjugate @ qs["Transpose"]["State"], ConjugateTranspose @ qs["State"]],
        qb
    ]
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


QuantumStateProp[qs_, "Tensor"] := qs["Computational"]["StateTensor"]

QuantumStateProp[qs_, "Matrix"] := qs["Computational"]["StateMatrix"]

QuantumStateProp[qs_, "VectorRepresentation"] := qs["Computational"]["StateVector"]

QuantumStateProp[qs_, "MatrixRepresentation"] := qs["Computational"]["DensityMatrix"]

QuantumStateProp[qs_, "TensorRepresentation"] := qs["Computational"]["StateTensor"]

QuantumStateProp[qs_, "NormalizedMatrixRepresentation"] := normalizeMatrix @ qs["MatrixRepresentation"]


(* block sphere*)

QuantumStateProp[qs_, "BlochSphericalCoordinates"] /; qs["Dimension"] == 2 := With[{
    state = qs["Computational"]["NormalizedStateVector"], matrix = qs["Computational"]["NormalizedDensityMatrix"]
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
    state = qs["Computational"]["NormalizedStateVector"], matrix = qs["Computational"]["NormalizedDensityMatrix"]
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

QuantumStateProp[qs_, "BlochPlot" | "BlochSpherePlot"] /; qs["Dimension"] == 2 := Module[{
    greatCircles, referenceStates, u, v, w
},
    greatCircles = ParametricPlot3D[
        {{Cos[t], Sin[t], 0}, {0, Cos[t], Sin[t]}, {Cos[t], 0, Sin[t]}},
        {t, 0, 2 Pi},
        PlotStyle -> ConstantArray[{Black, Thin}, 3], Axes -> False, 
        Boxed -> False,
        ImageSize -> Medium,
        PlotRange -> {{-1.7, 1.7}, {-1.7, 1.7}, {-1.7, 1.7}}
    ];
    referenceStates = Graphics3D[{
        Opacity[0.4], Sphere[], Black, Thick, Opacity[1.0],
        Line[{{0, 1, 0}, {0, -1, 0}}], Line[{{0, 0, 1}, {0, 0, -1}}], Line[{{1, 0, 0}, {-1, 0, 0}}],
        Text[Ket[0], {0, 0, 1.3}],  Text[Ket[1], {0, 0, -1.3}],
        Text[Ket["R"], {0, 1.3, 0}], Text[Ket["L"], {0, -1.3, 0}],
        Text[Ket["+"], {1.3, 0, 0}], Text[Ket["-"], {-1.3, 0, 0}]
    },
        Boxed -> False,
        Axes -> False,
        PlotRange -> {{-1.7, 1.7}, {-1.7, 1.7}, {-1.7, 1.7}}
    ];
    {u, v, w} = qs["BlochCartesianCoordinates"];
    Show[Join[{greatCircles, referenceStates}, {
        Graphics3D[
            {Red, Arrowheads[0.05], Arrow[Tube[{{0, 0, 0}, {u, v, w}}, 0.03], {0, -0.01}]},
            Boxed -> False,
            PlotRange -> {{-1.7, 1.7}, {-1.7, 1.7}, {-1.7, 1.7}}
        ]}],
        PlotRange -> All,
        ViewPoint -> {1, 1, 1}
    ]
]


(* basis properties *)

QuantumStateProp[qs_, prop_ ? propQ, args___] /;
    MatchQ[prop, Alternatives @@ Intersection[QuantumBasis["Properties"], QuantumState["Properties"]]] := qs["Basis"][prop, args]

