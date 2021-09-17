Package["QuantumFramework`"]

PackageScope["QuantumDiscreteStateProp"]



$QuantumDiscreteStateProperties = {
     "StateType", "State", "Basis",
     "Amplitudes", "StateVector", "DensityMatrix",
     "NormalizedState", "NormalizedAmplitudes", "NormalizedStateVector", "NormalizedDensityMatrix",
     "NormalizedMatrix",
     "MatrixRepresentation", "Matrix",
     "Entropy", "VonNeumannEntropy",
     "Purity", "Type", "PureStateQ", "MixedStateQ",
     "BlochSphericalCoordinates", "BlochCartesianCoordinates",
     "BlochPlot",
     "Operator",
     "Eigenvalues", "Eigenvectors", "Eigenstates"
};

$QuantumDiscreteStateProperties =  DeleteDuplicates @ Join[$QuantumDiscreteStateProperties, QuantumBasis["Properties"]];

QuantumDiscreteState["Properties"] := $QuantumDiscreteStateProperties


qds_QuantumDiscreteState["ValidQ"] := QuantumDiscreteStateQ[qds]


QuantumDiscreteState::undefprop = "property `` is undefined for this state";

(qds_QuantumDiscreteState[prop_ ? propQ, args___]) /; QuantumDiscreteStateQ[qds] := With[{
    result = Check[QuantumDiscreteStateProp[qds, prop, args], $Failed]
},
    (QuantumDiscreteStateProp[qds, prop, args] = result) /;
        !FailureQ[Unevaluated @ result] && (!MatchQ[Unevaluated @ result, _QuantumDiscreteStateProp] || Message[QuantumDiscreteState::undefprop, prop])
]


QuantumDiscreteStateProp[_, "Properties"] := QuantumDiscreteState["Properties"]


(* getters *)

QuantumDiscreteStateProp[QuantumDiscreteState[state_, _], "State"] := state

QuantumDiscreteStateProp[QuantumDiscreteState[_, basis_], "Basis"] := basis


(* two types of states *)

QuantumDiscreteStateProp[qds_, "StateType"] := Which[
    VectorQ[qds["State"]],
    "Vector",
    SquareMatrixQ[qds["State"]],
    "Matrix",
    True,
    "UnknownType"
]


(* amplitudes are only defined for pure states *)

QuantumDiscreteState::notpure = "is not a pure state";

QuantumDiscreteStateProp[qds_, "Amplitudes"] := Module[{result},
    result = Enclose @ Association @ Thread[qds["BasisElementNames"] -> ConfirmBy[qds["StateVector"], VectorQ]];
    result /; !FailureQ[result]
]

QuantumDiscreteStateProp[qds_, "StateVector"] := Module[{result},
    result = Enclose @ Which[
        qds["StateType"] === "Vector",
        qds["State"],
        qds["PureStateQ"],
        First @ Pick[qds["Eigenvectors"], ConfirmBy[Thread[Chop[qds["Eigenvalues"]] != 0], Apply[Or]]],
        True,
        Message[QuantumDiscreteState::notpure]; $Failed
    ];
    result /; !FailureQ[result]
]


(* normalization *)

QuantumDiscreteStateProp[qds_, "NormalizedState"] := With[{state = qds["State"]},
    Switch[qds["StateType"], "Vector", Normalize[state], "Matrix", normalizeMatrix[state], _, state]
]

QuantumDiscreteStateProp[qds_, "NormalizedAmplitudes"] := With[{amplitudes = qds["Amplitudes"]},
    amplitudes / Norm[Values[amplitudes]]
]

QuantumDiscreteStateProp[qds_, "NormalizedStateVector"] := qds["NormalizedState"]

QuantumDiscreteStateProp[qds_, "NormalizedDensityMatrix"] := normalizeMatrix @ qds["DensityMatrix"]

QuantumDiscreteStateProp[qds_, "NormalizedMatrixRepresentation" | "NormalizedMatrix"] := normalizeMatrix @ qds["Matrix"]


(* density matrix *)

QuantumDiscreteStateProp[qds_, "DensityMatrix"] /; qds["StateType"] === "Vector" :=
    With[{state = qds["StateVector"]}, ConjugateTranspose[{state}] . {state}]

QuantumDiscreteStateProp[qds_, "DensityMatrix"] /; qds["StateType"] === "Matrix" := qds["State"]

QuantumDiscreteStateProp[qds_, "Operator"] := Association @ Thread[
    Flatten[#, 1, CircleTimes] & /@ CircleTimes @@@ MapAt[ReplaceAll[Ket -> Bra], Tuples[qds["BasisElementNames"], 2], {All, 2}] ->
    Flatten @ qds["DensityMatrix"]
]

QuantumDiscreteStateProp[qds_, "MatrixRepresentation" | "Matrix"] := QuantumDiscreteState[qds, QuantumBasis[qds["Dimension"]]]["DensityMatrix"]


QuantumDiscreteStateProp[qds_, "Eigenvalues"] := Eigenvalues[qds["DensityMatrix"]]

QuantumDiscreteStateProp[qds_, "Eigenvectors"] := Eigenvectors[qds["DensityMatrix"]]

QuantumDiscreteStateProp[qds_, "Eigenstates"] := QuantumDiscreteState[#, qds["Basis"]] & /@ qds["Eigenvectors"]


(* entropy *)

QuantumDiscreteStateProp[qds_, "VonNeumannEntropy" | "Entropy", logBase_ ? NumericQ] := With[{
    matrix = qds["NormalizedDensityMatrix"]
},  Simplify @ If[
        qds["Type"] === "Pure",
        0,
        (* - Total @ Map[# Log[logBase, #] &, Select[Re @ Eigenvalues@qds[DensityMatrix"], Positive]] *)

        Enclose[- Tr[matrix . ConfirmBy[MatrixLog[matrix], MatrixQ]] / Log[logBase], $Failed &]
    ]
]

QuantumDiscreteStateProp[qds_, "VonNeumannEntropy" | "Entropy"] := qds["VonNeumannEntropy", E]

QuantumDiscreteStateProp[qds_, {"VonNeumannEntropy" | "Entropy", logBase_}] := qds["VonNeumannEntropy", logBase]


(* purity *)

QuantumDiscreteStateProp[qds_, "Purity"] := Simplify @ Abs[Tr[MatrixPower[qds["NormalizedDensityMatrix"], 2]]]

QuantumDiscreteStateProp[qds_, "Type"] := Which[
    qds["StateType"] === "Vector" || TrueQ[qds["Purity"] == 1],
    "Pure",
    PositiveSemidefiniteMatrixQ[qds["DensityMatrix"]],
    "Mixed",
    True,
    "Unknown"
]

QuantumDiscreteStateProp[qds_, "PureStateQ"] := qds["Type"] === "Pure"

QuantumDiscreteStateProp[qds_, "MixedStateQ"] := qds["Type"] === "Mixed"


(* block sphere*)

QuantumDiscreteStateProp[qds_, "BlochSphericalCoordinates"] /; qds["Dimension"] == 2 := With[{state = qds["NormalizedState"]},
    Switch[
        qds["StateType"],

        "Vector",
        With[{
            alpha = state[[1]], beta = state[[2]]
        },
            {1, 2 ArcCos[Abs[alpha]], Arg[beta] - Arg[alpha]}
        ],

        "Matrix",
        With[{
            u = Re[state[[1, 2]]], v = Im[state[[2, 1]]], w = state[[1, 1]] - state[[2, 2]]
        },
            If[ u == v == w == 0,
                {0, 0, 0},
                Map[Abs, CoordinateTransformData["Cartesian" -> "Spherical", "Mapping", {u, v, w}]]
            ]
        ],

        (* unimplemented*)
        True,
        $Failed
    ]
]

QuantumDiscreteStateProp[qds_, "BlochCartesianCoordinates"] /; qds["Dimension"] == 2 :=  With[{state = qds["NormalizedState"]},
    Switch[
        qds["StateType"],

        "Vector",
        Module[{
            alpha = state[[1]], beta = state[[2]], theta, phi
        },
            {theta, phi} = {2 ArcCos[Abs[alpha]], Arg[beta] - Arg[alpha]};
            {Sin[theta] Cos[phi], Sin[theta] Sin[phi], Cos[theta]}
        ],

        "Matrix",
        With[{
            u = Re[state[[1, 2]]], v = Im[state[[2, 1]]], w = state[[1, 1]] - state[[2, 2]]
        },
            {u, v, w}
        ],

        (* unimplemented*)
        True,
        $Failed
    ]
]

QuantumDiscreteStateProp[qds_, "BlochPlot"] /; qds["Dimension"] == 2 := Module[{
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
    {u, v, w} = qds["BlochCartesianCoordinates"];
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

QuantumDiscreteStateProp[qds_, prop_ ? propQ, args___] /;
    MatchQ[prop, Alternatives @@ Intersection[qds["Basis"]["Properties"], qds["Properties"]]] := qds["Basis"][prop, args]

