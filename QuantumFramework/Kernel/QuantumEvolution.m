Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumEvolve"]
PackageExport["HamiltonianTransitionRate"]



QuantumEvolve::error = "Differential Solver failed to find a solution"

Options[QuantumEvolve] = DeleteDuplicatesBy[First] @ Join[
    {"AdditionalEquations" -> {}, "ReturnEquations" -> False, "MergeInterpolatingFunctions" -> True},
    Options[NDSolveValue], Options[DSolveValue]
]

QuantumEvolve[
    hamiltonian_ ? QuantumOperatorQ,
    defaultState : _ ? QuantumStateQ | Automatic | None : Automatic,
    defaultParameter : _Symbol | {_Symbol, _ ? NumericQ, _ ? NumericQ} | Automatic : Automatic,
    opts : OptionsPattern[]
] := Enclose @ Block[{
    state = None, basis,
    matrix,
    parameter, parameterSpec,
    numericQ, solution,
    rhs, init,
    equations, return, param,
    phaseSpaceQ = False,
    mergeQ = TrueQ[OptionValue["MergeInterpolatingFunctions"]]
},
    If[ defaultState =!= None,
        state = Replace[defaultState, Automatic :> QuantumState[{"Register", hamiltonian["InputDimensions"]}]];
        phaseSpaceQ = state["Picture"] === "PhaseSpace" && AllTrue[Sqrt[state["Dimensions"]], IntegerQ]
    ];
    ConfirmAssert[hamiltonian["OutputDimensions"] == hamiltonian["InputDimensions"]];
    ConfirmAssert[state === None || hamiltonian["InputDimension"] == state["Dimension"]];
    If[ defaultParameter === Automatic,
        parameter = First[hamiltonian["Parameters"], \[FormalT]];
        parameterSpec = Replace[First[hamiltonian["ParameterSpec"], {}], {} :> {parameter, 0, 1}],

        parameter = Replace[defaultParameter, {p_Symbol, _, _} :> p];
        parameterSpec = Replace[defaultParameter, _Symbol :> {parameter, 0, 1}]
    ];
    ConfirmAssert[Developer`SymbolQ[parameter]];
    numericQ = MatchQ[defaultParameter, {_Symbol, _ ? NumericQ, _ ? NumericQ}];
    (* TrigToExp helps with some examples *)
    matrix = Progress`EvaluateWithProgress[
        TrigToExp @ Confirm @ If[mergeQ, MergeInterpolatingFunctions, Identity][
            If[ phaseSpaceQ,
                HamiltonianTransitionRate[hamiltonian],
                hamiltonian["Matrix"] / I
            ]
        ],
        <|"Text" -> "Preparing Hamiltonian"|>
    ];
    rhs = If[
        state === None || state["VectorQ"],
        matrix . \[FormalS][parameter],
        matrix . \[FormalS][parameter] - \[FormalS][parameter] . matrix
    ];
    basis = If[state =!= None && phaseSpaceQ, state["Basis"], hamiltonian["OutputBasis"]];
    If[ state =!= None,
        If[ phaseSpaceQ,
            (* always treat state as a single system in phase-space *)
            state = QuantumWignerTransform[QuantumState[QuantumWeylTransform[state], Sqrt[state["Dimension"]]]],
            state = QuantumState[state["Split", state["Qudits"]], hamiltonian["Input"]]
        ]
    ];
    init = If[defaultState === None, IdentityMatrix[hamiltonian["InputDimension"], SparseArray], state["State"]];
    equations = Join[
        {
            \[FormalS]'[parameter] == rhs,
            \[FormalS][0] == init
        },
        Flatten[{OptionValue["AdditionalEquations"]}]
    ];
    return = If[ numericQ,
        \[FormalS],
        Element[
            \[FormalS][parameter],
            If[ state === None,
                Matrices[hamiltonian["MatrixNameDimensions"]],
                If[state["VectorQ"], Vectors[state["Dimension"], If[phaseSpaceQ, Reals, Complexes]], Matrices[state["MatrixDimensions"]]]
            ]
        ]
    ];
    param = If[numericQ, parameterSpec, parameter];
    If[TrueQ[OptionValue["ReturnEquations"]], Return[{equations, return, param}]];

    (* hide sparse arrays over delayed definitions and inject numeric paratemer, initial condition is always dense *)
    If[ numericQ && ! mergeQ,
        Module[{frhs},
            Block[{s, t},
                With[{
                    def = rhs /. {
                        \[FormalS][_] -> s,
                        f_InterpolatingFunction[_] :> f[t],
                        parameter -> t
                    }
                },
                    SetDelayed @@ Hold[frhs[s_ ? ArrayQ, t_], Unevaluated[def] /. sa_SparseArray ? SparseArrayQ :> MapSparseArray[ReplaceAll[parameter -> t], sa]]
                ]
            ];
            equations = Join[
                {
                    \[FormalS]'[parameter] == frhs[\[FormalS][parameter], parameter],
                    \[FormalS][0] == Normal[init]
                },
                Flatten[{OptionValue["AdditionalEquations"]}]
            ]
        ],
        equations = equations /. sa_SparseArray ? SparseArrayQ :> Normal[sa]
    ];

    solution = If[numericQ,
        Module[{time},
            WithCleanup[
                Unprotect[\[FormalS], Evaluate[parameter]],
                Progress`EvaluateWithProgress[
                    NDSolveValue[
                        equations,
                        return,
                        param,
                        FilterRules[{opts}, Options[NDSolveValue]],
                        StepMonitor :> (time = parameter)
                    ],
                    <|"Text" -> "Integrating", "Progress" :> time / Last[param], "Percentage" :> time / Last[param], "ElapsedTime" -> Automatic, "RemainingTime" -> Automatic|>
                ],
                Protect[\[FormalS], Evaluate[parameter]]
            ]
        ],
        DSolveValue[
            equations,
            return,
            param,
            FilterRules[{opts}, Options[NDSolveValue]]
        ]
    ];
    If[ MatchQ[solution, _InterpolatingFunction],
        solution = ExpandInterpolatingFunction[solution, parameter]
    ];
    Which[
        state === None && SquareMatrixQ[solution],
        QuantumOperator[
            solution,
            hamiltonian["Order"],
            hamiltonian["Basis"],
            "ParameterSpec" -> parameterSpec
        ],
        stateQ[solution],
        If[ state =!= None && phaseSpaceQ,
            ExpandInterpolatingFunction @ QuantumState[
                QuantumState[
                    QuantumWeylTransform[QuantumState[solution, "Wigner"[Sqrt[state["Dimension"]]], "Picture" -> "PhaseSpace"]],
                    Sqrt[basis["Dimensions"]]
                ]["Double"],
                basis,
                "ParameterSpec" -> parameterSpec
            ],
            QuantumState[solution, basis, "ParameterSpec" -> parameterSpec]
        ],
        True,
        Message[QuantumEvolve::error];
        solution
    ]
]

MapSparseArray[f_, sa_] := SparseArray[Thread[sa["ExplicitPositions"] -> f /@ sa["ExplicitValues"]], Dimensions[sa]]

MergeInterpolatingFunctions[array_ ? SparseArrayQ] := Enclose @ Block[{pos, values, ifs, grid, dims, dim, params, param},
    pos = array["ExplicitPositions"];
    values = array["ExplicitValues"];
	ifs = Cases[values, _InterpolatingFunction[_], {ArrayDepth[array], Infinity}, Heads -> True];
	If[Length[ifs] == 0, Return[array]];
    params = First /@ ifs;
    ConfirmAssert[SameQ @@ params];
    ifs = Head /@ ifs;
	dims = Through[ifs["OutputDimensions"]];
	ConfirmAssert[SameQ @@ dims];
    param = First[params];
	dim = First[dims];
	ConfirmAssert[dim === {}];
	grid = Catenate[Intersection @@ Through[ifs["Grid"]]];
    (* make custom linear interpolation supporting sparse arrays *)
	(* Interpolation[{#, Normal @ SparseArray[Thread[pos -> (values /. param -> #)], Dimensions[array]]} & /@ grid, InterpolationOrder -> 1][param] *)
	With[{arrays = SparseArray[Thread[pos -> (values /. param -> #)], Dimensions[array]] & /@ grid},
        Module[{f, getArray},
            Block[{t},
                With[{rhs = Piecewise @ MapIndexed[
                    With[{a = (#[[2]] - t) / (#[[2]] - #[[1]]), b = (t - #[[1]]) / (#[[2]] - #[[1]])},
                        {a getArray[#2[[1]]] + b getArray[#2[[1]] + 1], Between[t, #]}
                    ] &,
                    Partition[grid, 2, 1]
                ]},
                    SetDelayed @@ {f[t_ ? NumericQ], rhs}
                ];
                Format[f] ^:= Interpretation["SparseInterpolationFunction", f];
                getArray[i_] := arrays[[i]];
                f[param]
            ]
        ]
    ]
]

MergeInterpolatingFunctions[array_ ? ArrayQ] := MergeInterpolatingFunctions[SparseArray[array]]


ExpandInterpolatingFunction[f_InterpolatingFunction, parameter_] := Map[
    Interpolation[Thread[{f["Grid"], #}], InterpolationOrder -> f["InterpolationOrder"]][parameter] &,
    Transpose[f["ValuesOnGrid"], InversePermutation[Cycles[{Range[Length[f["OutputDimensions"]] + 1]}]]],
    {-2}
]

ExpandInterpolatingFunction[f_InterpolatingFunction[parameter_]] := ExpandInterpolatingFunction[f, parameter]

ExpandInterpolatingFunction[array_ ? ArrayQ] := ExpandInterpolatingFunction[MergeInterpolatingFunctions[array]]

ExpandInterpolatingFunction[qs_QuantumState] := QuantumState[ExpandInterpolatingFunction @ qs["State"], qs["Basis"]]


HamiltonianTransitionRate[H_QuantumOperator] /; H["OutputDimension"] == H["InputDimension"] :=
    Block[{d = H["OutputDimension"], A, G},
        A = QuditBasis["Wigner"[d, "Exact" -> ! H["NumberQ"]]]["Elements"];
        G = Simplify[2 Im[Map[Tr, Outer[Dot, A, A, A, 1], {3}] / d]] / If[EvenQ[d], 4, 1];
        SymmetrizedArray[Transpose[G . QuantumWignerTransform[QuantumState[H["MatrixRepresentation"], d]]["StateVector"]], Automatic, Antisymmetric[{1, 2}]]
    ]

