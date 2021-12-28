Package["Wolfram`QuantumFramework`"]

PackageScope["QuantumStateProp"]



$QuantumStateProperties = {
     "StateType", "State", "Basis",
     "Amplitudes", "Weights", "Probabilities", "StateVector", "DensityMatrix",
     "NormalizedState", "NormalizedAmplitudes", "NormalizedStateVector", "NormalizedDensityMatrix",
     "Entropy", "VonNeumannEntropy",
     "Purity", "Type", "PureStateQ", "MixedStateQ",
     "BlochSphericalCoordinates", "BlochCartesianCoordinates",
     "BlochPlot",
     "Projector", "NormalizedProjector",
     "Operator", "NormalizedOperator",
     "Eigenvalues", "Eigenvectors", "Eigenstates",
     "Computational", "SchmidtBasis", "SpectralBasis",
     "StateTensor", "StateMatrix",
     "Tensor", "Matrix",
     "Pure", "Mixed",
     "Formula"
};

QuantumState["Properties"] := QuantumState["Properties"] = DeleteDuplicates @ Join[$QuantumStateProperties, QuantumBasis["Properties"]]


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


(* amplitudes are only defined for pure states *)

QuantumState::notpure = "is not a pure state";

QuantumStateProp[qs_, "Amplitudes"] := Module[{result},
    result = Enclose @ KeySort @ Association @ Thread[qs["ElementNames"] -> ConfirmBy[qs["StateVector"], VectorQ]];
    result /; !FailureQ[result]
]

QuantumStateProp[qs_, "StateVector"] := Module[{result},
    result = Enclose @ Which[
        qs["StateType"] === "Vector",
        qs["State"],
        qs["PureStateQ"],
        First @ Pick[qs["Eigenvectors"], ConfirmBy[Map[# =!= 0 &, Chop[qs["Eigenvalues"]]], Apply[Or]]],
        True,
        Message[QuantumState::notpure]; $Failed
    ];
    result /; !FailureQ[result]
]

QuantumStateProp[qs_, "Weights"] := If[qs["PureStateQ"],
    Abs[qs["StateVector"]] ^ 2,
    Diagonal @ qs["DensityMatrix"]
]

QuantumStateProp[qs_, "Probabilities"] := Simplify /@ (qs["Weights"] / Total[qs["Weights"]])

QuantumStateProp[qs_, "Distribution"] := CategoricalDistribution[qs["Names"], qs["Probabilities"]]

QuantumStateProp[qs_, "Formula"] := Total @ KeyValueMap[Times, qs["NormalizedAmplitudes"]]


(* normalization *)

QuantumStateProp[qs_, "NormalizedState"] := With[{state = qs["State"]},
    Switch[qs["StateType"], "Vector", Normalize[state], "Matrix", normalizeMatrix[state], _, state]
]

QuantumStateProp[qs_, "NormalizedAmplitudes"] := Enclose @ With[{amplitudes = qs["Amplitudes"]},
    ConfirmQuiet[amplitudes / Norm[Values[amplitudes]], Power::infy]
]

QuantumStateProp[qs_, "NormalizedStateVector"] := Normalize @ qs["StateVector"]

QuantumStateProp[qs_, "NormalizedDensityMatrix"] := Enclose @ Confirm[normalizeMatrix @ qs["DensityMatrix"]]

QuantumStateProp[qs_, "Operator"] := QuantumOperator[qs["Projector"]]

QuantumStateProp[qs_, "NormalizedOperator"] := qs["NormalizedProjector"]["Amplitudes"]


(* density matrix *)

QuantumStateProp[qs_, "DensityMatrix"] /; qs["StateType"] === "Vector" :=
    With[{state = qs["StateVector"]}, KroneckerProduct[state, Conjugate[state]]]

QuantumStateProp[qs_, "DensityMatrix"] /; qs["StateType"] === "Matrix" := qs["State"]

QuantumStateProp[qs_, "DensityTensor"] := ArrayReshape[qs["DensityMatrix"], Join[qs["Dimensions"], qs["Dimensions"]]]

QuantumStateProp[qs_, "Projector"] := QuantumState[Flatten @ qs["DensityMatrix"], QuantumBasis[qs["Basis"], "Input" -> qs["Output"]["Dual"]]]

QuantumStateProp[qs_, "NormalizedProjector"] := QuantumState[Flatten @ qs["NormalizedDensityMatrix"], QuantumBasis[qs["Basis"], "Input" -> qs["Output"]["Dual"]]]

QuantumStateProp[qs_, "MatrixDimensions"] := {qs["Dimension"], qs["Dimension"]}

QuantumStateProp[qs_, "Eigenvalues"] := Eigenvalues[qs["DensityMatrix"]]

QuantumStateProp[qs_, "Eigenvectors"] := eigenvectors[qs["DensityMatrix"]]

QuantumStateProp[qs_, "Eigenstates"] := QuantumState[#, qs["Basis"]] & /@ qs["Eigenvectors"]


(* entropy *)

QuantumStateProp[qs_, "VonNeumannEntropy" | "Entropy", logBase_ ? NumericQ] := Enclose[With[{
        matrix = qs["NormalizedDensityMatrix"]
    },
    If[
        qs["PureStateQ"],
        0,
        ConfirmAssert[qs["Dimension"] < 2 ^ 10];
        Enclose[TimeConstrained[Chop @ Simplify @ - Tr[matrix . ConfirmBy[MatrixLog[matrix], MatrixQ]] / Log[logBase], 1], $Failed &]
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
        TimeConstrained[Simplify @ Abs[Tr[MatrixPower[ConfirmBy[qs["NormalizedDensityMatrix"], MatrixQ], 2]]], 1]
    ],
    Indeterminate &
]

QuantumStateProp[qs_, "Type"] := Which[
    TrueQ[qs["Purity"] == 1],
    "Pure",
    PositiveSemidefiniteMatrixQ[qs["DensityMatrix"]],
    "Mixed",
    True,
    "Unknown"
]

QuantumStateProp[qs_, "PureStateQ"] := qs["Type"] === "Pure"

QuantumStateProp[qs_, "MixedStateQ"] := qs["Type"] === "Mixed"


(* transforms *)

QuantumStateProp[qs_, "Computational"] := QuantumState[qs, QuantumBasis[
    "Output" -> QuditBasis[DeleteCases[qs["OutputDimensions"], 1]],
    "Input" -> QuditBasis[DeleteCases[qs["InputDimensions"], 1]]["Dual"]
]
]

QuantumStateProp[qs_, "SchmidtBasis"] := Module[{
    uMatrix, alphaValues, wMatrix
},

    {uMatrix, alphaValues, wMatrix} = SingularValueDecomposition[qs["Matrix"]];

    QuantumState[Diagonal @ alphaValues,
        QuantumBasis[
            QuditBasis[Association @ MapIndexed[Subscript["u", First @ #2] -> #1 &, Transpose[uMatrix]]],
            QuditBasis[Association @ MapIndexed[Subscript["v", First @ #2] -> #1 &, Transpose[wMatrix]]]["Dual"]
        ]
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

QuantumStateProp[qs_, "Pure"] := If[qs["PureStateQ"],
    qs,
    QuantumState[Flatten @ qs["DensityMatrix"], QuantumTensorProduct[qs["Basis"], qs["Basis"]["Dagger"]]][{"Split", 2 qs["Qudits"]}]
]

QuantumStateProp[qs_, "Mixed"] := Which[
    qs["MixedStateQ"],
    qs,
    qs["PureStateQ"] && IntegerQ[Sqrt[qs["Dimension"]]],
    With[{dimension = Sqrt[qs["Dimension"]]},
        QuantumState[
            ArrayReshape[qs["StateVector"], Table[dimension, 2]],
            QuantumBasis @ qs["QuditBasis"][{"TakeDimension", dimension}]
        ]
    ],
    True,
    $Failed
]

QuantumStateProp[qs_, "MatrixState"] := If[qs["StateType"] === "Matrix", qs, QuantumState[qs["DensityMatrix"], qs["Basis"]]]

QuantumStateProp[qs_, "Transpose"] := QuantumState[If[qs["PureStateQ"], Flatten, Identity] @ Transpose[qs["StateMatrix"]], qs["Basis"]["Transpose"]]

QuantumStateProp[qs_, {"Transpose", qudits : {_Integer...}}] := QuantumState[
    ArrayReshape[
        Transpose[ArrayReshape[qs["DensityMatrix"], Join[qs["Dimensions"], qs["Dimensions"]]], Cycles[{#, # + qs["Qudits"]} & /@ qudits]],
        {qs["Dimension"], qs["Dimension"]}
    ],
    qs["Basis"]
]

QuantumStateProp[qs_, {"Trace", qudits : {_Integer...}}] := QuantumPartialTrace[qs, qudits]

QuantumStateProp[qs_, "Conjugate" | "Dual"] := QuantumState[If[qs["PureStateQ"], Flatten, Identity] @ Conjugate[qs["StateMatrix"]], qs["Basis"]["Dual"]]

QuantumStateProp[qs_, "ConjugateTranspose" | "Dagger"] := QuantumState[
    If[qs["PureStateQ"], Flatten, Identity] @ ConjugateTranspose[qs["StateMatrix"]],
    qs["Basis"]["ConjugateTranspose"]
]


QuantumStateProp[qs_, {"Permute", perm_Cycles}] := QuantumState[
    If[ qs["PureStateQ"],
        Flatten @ Transpose[qs["StateTensor"], perm],
        ArrayReshape[
            Transpose[qs["StateTensor"], PermutationCycles[With[{list = PermutationList[perm, qs["Qudits"]]}, Join[list, list + qs["Qudits"]]]]],
            Table[qs["Dimension"], 2]
        ]
    ],
    qs["Basis"][{"Permute", perm}]
]

QuantumStateProp[qs_, {"PermuteInput", perm_Cycles}] := profile["PermuteInput"] @ If[perm === Cycles[{}],
    qs,
    QuantumState[
        qs @ QuantumOperator[{"Permutation", Permute[qs["InputDimensions"], perm], InversePermutation @ perm}]["State"],
        qs["Basis"][{"PermuteInput", perm}]
    ]
]

QuantumStateProp[qs_, {"PermuteOutput", perm_Cycles}] := profile["PermuteOutput"] @ If[perm === Cycles[{}],
    qs,
    QuantumState[
        QuantumOperator[{"Permutation", qs["OutputDimensions"], perm}]["State"] @ qs,
        qs["Basis"][{"PermuteOutput", perm}]
    ]
]

QuantumStateProp[qs_, {"Split", n_Integer : 0}] := With[{basis = qs["Basis"][{"Split", n}]},
    QuantumState[qs["State"], basis]
    (* QuantumState[QuantumState[qs["Computational"]["State"], QuantumBasis[basis["OutputDimensions"], basis["InputDimensions"]]], basis] *)
]

QuantumStateProp[qs_, "Numeric"] := QuantumState[N @ qs["State"], qs["Basis"]["Numeric"]]

QuantumStateProp[qs_, "Chop"] := QuantumState[Chop @ qs["State"], qs["Basis"]]

(* representations *)

QuantumStateProp[qs_, "StateTensor"] := If[
    qs["PureStateQ"],
    ArrayReshape[qs["StateVector"], qs["Dimensions"]],
    ArrayReshape[qs["DensityMatrix"], Join[qs["Dimensions"], qs["Dimensions"]]]
]

QuantumStateProp[qs_, "StateMatrix"] := If[
    qs["PureStateQ"],
    ArrayReshape[qs["StateVector"], qs["MatrixNameDimensions"]],
    ArrayReshape[Transpose[qs["StateTensor"], InversePermutation @ FindPermutation[
        With[{input = Join[Range[qs["OutputQudits"]], qs["Qudits"] + Range[qs["OutputQudits"]]]},
            Join[input, Complement[Range[2 qs["Qudits"]], input]]
        ]
    ]],
        qs["MatrixNameDimensions"] ^ 2
    ]
]

QuantumStateProp[qs_, "Tensor"] := qs["Computational"]["StateTensor"]

QuantumStateProp[qs_, "Matrix"] := qs["Computational"]["StateMatrix"]

QuantumStateProp[qs_, "PureTensor"] := qs["Computational"]["Pure"]["StateTensor"]

QuantumStateProp[qs_, "PureMatrix"] := qs["Computational"]["Pure"]["StateMatrix"]

QuantumStateProp[qs_, "PureVector"] := qs["Computational"]["Pure"]["StateVector"]

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
            u = Re[matrix[[1, 2]]], v = Im[matrix[[2, 1]]], w = matrix[[1, 1]] - matrix[[2, 2]]
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
            u = Re[matrix[[1, 2]]], v = Im[matrix[[2, 1]]], w = matrix[[1, 1]] - matrix[[2, 2]]
        },
            {u, v, w}
        ]
    ]
]

QuantumStateProp[qs_, "BlochPlot"] /; qs["Dimension"] == 2 := Module[{
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

