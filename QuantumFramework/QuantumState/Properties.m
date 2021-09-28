Package["QuantumFramework`"]

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
     "Computational",
     "StateTensor", "StateMatrix",
     "Tensor", "Matrix", "Vector"
};

QuantumState["Properties"] := DeleteDuplicates @ Join[$QuantumStateProperties, QuantumBasis["Properties"]]


qs_QuantumState["ValidQ"] := QuantumStateQ[qs]


QuantumState::undefprop = "property `` is undefined for this state"

QuantumState::failprop = "property `` failed with ``"

(qs_QuantumState[prop_ ? propQ, args___]) /; QuantumStateQ[qs] := With[{
    result = QuantumStateProp[qs, prop, args]
},
    (QuantumStateProp[qs, prop, args] = result) /;
        (!FailureQ[Unevaluated @ result] || Message[QuantumState::failprop, prop, result]) &&
        (!MatchQ[Unevaluated @ result, _QuantumStateProp] || Message[QuantumState::undefprop, prop])
]


QuantumStateProp[_, "Properties"] := QuantumState["Properties"]


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
    result = Enclose @ Association @ Thread[qs["BasisElementNames"] -> ConfirmBy[qs["StateVector"], VectorQ]];
    result /; !FailureQ[result]
]

QuantumStateProp[qs_, "StateVector"] := Module[{result},
    result = Enclose @ Which[
        qs["StateType"] === "Vector",
        qs["State"],
        qs["PureStateQ"],
        First @ Pick[qs["Eigenvectors"], ConfirmBy[Thread[Chop[qs["Eigenvalues"]] =!= 0], Apply[Or]]],
        True,
        Message[QuantumState::notpure]; $Failed
    ];
    result /; !FailureQ[result]
]

QuantumStateProp[qs_, "Weights"] := If[qs["PureStateQ"], Abs[qs["StateVector"]] ^ 2, Inverse[qs["Eigenvectors"]] . qs["Eigenvalues"]]

QuantumStateProp[qs_, "Probabilities"] := Re @ Normalize[qs["Weights"], Total]

QuantumStateProp[qs_, "Formula"] := Total @ KeyValueMap[Times, qs["NormalizedAmplitudes"]]


(* normalization *)

QuantumStateProp[qs_, "NormalizedState"] := With[{state = qs["State"]},
    Switch[qs["StateType"], "Vector", Normalize[state], "Matrix", normalizeMatrix[state], _, state]
]

QuantumStateProp[qs_, "NormalizedAmplitudes"] := Enclose @ With[{amplitudes = qs["Amplitudes"]},
    ConfirmQuiet[amplitudes / Norm[Values[amplitudes]], Power::infy]
]

QuantumStateProp[qs_, "NormalizedStateVector"] := qs["NormalizedState"]

QuantumStateProp[qs_, "NormalizedDensityMatrix"] := Enclose @ Confirm[normalizeMatrix @ qs["DensityMatrix"]]

QuantumStateProp[qs_, "NormalizedMatrixRepresentation" | "NormalizedMatrix"] := normalizeMatrix @ qs["Matrix"]

QuantumStateProp[qs_, "Operator"] := qs["Projector"]["Amplitudes"]

QuantumStateProp[qs_, "NormalizedOperator"] := qs["NormalizedProjector"]["Amplitudes"]

(* density matrix *)

QuantumStateProp[qs_, "DensityMatrix"] /; qs["StateType"] === "Vector" :=
    With[{state = qs["StateVector"]}, KroneckerProduct[state, Conjugate[state]]]

QuantumStateProp[qs_, "DensityMatrix"] /; qs["StateType"] === "Matrix" := qs["State"]

QuantumStateProp[qs_, "Projector"] := QuantumState[Flatten @ qs["DensityMatrix"], QuantumBasis[qs["Basis"], "Input" -> qs["Output"]["Dual"]]]

QuantumStateProp[qs_, "NormalizedProjector"] := QuantumState[Flatten @ qs["NormalizedDensityMatrix"], QuantumBasis[qs["Basis"], "Input" -> qs["Output"]["Dual"]]]

QuantumStateProp[qs_, "MatrixRepresentation" | "Matrix"] := qs["Computational"]["DensityMatrix"]

QuantumStateProp[qs_, "MatrixDimensions"] := {qs["Dimension"], qs["Dimension"]}

QuantumStateProp[qs_, "Eigenvalues"] := Eigenvalues[qs["DensityMatrix"]]

QuantumStateProp[qs_, "Eigenvectors"] := Eigenvectors[qs["DensityMatrix"]]

QuantumStateProp[qs_, "NormalizedEigenvectors"] := Normalize /@ qs["Eigenvectors"]

QuantumStateProp[qs_, "Eigenstates"] := QuantumState[#, qs["Basis"]] & /@ qs["Eigenvectors"]


(* entropy *)

QuantumStateProp[qs_, "VonNeumannEntropy" | "Entropy", logBase_ ? NumericQ] := With[{
    matrix = qs["NormalizedDensityMatrix"]
},  Simplify @ If[
        qs["Type"] === "Pure",
        0,
        (* - Total @ Map[# Log[logBase, #] &, Select[Re @ Eigenvalues@qs[DensityMatrix"], Positive]] *)

        Enclose[- Tr[matrix . ConfirmBy[MatrixLog[matrix], MatrixQ]] / Log[logBase], $Failed &]
    ]
]

QuantumStateProp[qs_, "VonNeumannEntropy" | "Entropy"] := qs["VonNeumannEntropy", E]

QuantumStateProp[qs_, {"VonNeumannEntropy" | "Entropy", logBase_}] := qs["VonNeumannEntropy", logBase]


(* purity *)

QuantumStateProp[qs_, "Purity"] := Enclose @ Abs[Tr[MatrixPower[ConfirmBy[qs["NormalizedDensityMatrix"], MatrixQ], 2]]]

QuantumStateProp[qs_, "Type"] := Which[
    qs["StateType"] === "Vector" || TrueQ[qs["Purity"] == 1],
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

QuantumStateProp[qs_, "Normalized"] := QuantumState[If[qs["StateType"] === "Vector", qs["NormalizedStateVector"], qs["NormalizedStateMatrix"]], qs["Basis"]]

QuantumStateProp[qs_, "Pure"] := If[qs["PureStateQ"],
    qs,
    QuantumState[Flatten @ qs["DensityMatrix"], QuantumTensorProduct[qs["Basis"], qs["Basis"]["Dual"]]]
]

QuantumStateProp[qs_, "Transpose"] := QuantumState[Flatten @ Transpose[qs["StateMatrix"]], qs["Basis"]["Transpose"]]

QuantumStateProp[qs_, "Conjugate" | "Dual"] := QuantumState[Flatten @ Conjugate[qs["StateMatrix"]], qs["Basis"]["Dual"]]

QuantumStateProp[qs_, "ConjugateTranspose" | "Dagger"] := QuantumState[Flatten @ ConjugateTranspose[qs["StateMatrix"]], qs["Basis"]["ConjugateTranspose"]]



QuantumStateProp[qs_, "StateTensor"] := If[
    qs["PureStateQ"],
    ArrayReshape[qs["StateVector"], qs["Dimensions"]],
    ArrayReshape[qs["DensityMatrix"], Join[qs["Dimensions"], qs["Dimensions"]]]
]

QuantumStateProp[qs_, "StateMatrix"] := If[qs["PureStateQ"], ArrayReshape[qs["StateVector"], qs["MatrixNameDimensions"]], qs["DensityMatrix"]]

QuantumStateProp[qs_, "Tensor"] := qs["Computational"]["StateTensor"]

QuantumStateProp[qs_, "Matrix"] := qs["Computational"]["StateMatrix"]

QuantumStateProp[qs_, "PureTensor"] := qs["Computational"]["Pure"]["StateTensor"]

QuantumStateProp[qs_, "PureMatrix"] := qs["Computational"]["Pure"]["StateMatrix"]

QuantumStateProp[qs_, "PureVector"] := qs["Computational"]["Pure"]["StateVector"]

QuantumStateProp[qs_, "VectorRepresentation"] := qs["Computational"]["StateVector"]

QuantumStateProp[qs_, "MatrixRepresentation"] := qs["Computational"]["DensityMatrix"]

QuantumStateProp[qs_, "TensorRepresentation"] := qs["Computational"]["StateTensor"]


(* block sphere*)

QuantumStateProp[qs_, "BlochSphericalCoordinates"] /; qs["Dimension"] == 2 := With[{state = qs["StateVector"], matrix = qs["DensityMatrix"]},
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

QuantumStateProp[qs_, "BlochCartesianCoordinates"] /; qs["Dimension"] == 2 :=  With[{state = qs["StateVector"], matrix = qs["DensityMatrix"]},
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
    MatchQ[prop, Alternatives @@ Intersection[qs["Basis"]["Properties"], qs["Properties"]]] := qs["Basis"][prop, args]

