Package["Wolfram`QuantumFramework`"]

PackageExport[QuantumEvolve]
PackageExport[HamiltonianTransitionRate]
PackageExport[LindbladTransitionRates]



QuantumEvolve::error = "Differential Solver failed to find a solution"

Options[QuantumEvolve] = DeleteDuplicatesBy[First] @ Join[
    {"AdditionalEquations" -> {}, "ReturnEquations" -> False, "ReturnSolution" -> False, "MergeInterpolatingFunctions" -> True},
    Options[NDSolveValue], Options[DSolveValue]
]

QuantumEvolve[hamiltonian_QuantumOperator, lindblad : _QuantumOperator | {___QuantumOperator}, args___] := QuantumEvolve[hamiltonian, ToList[lindblad] -> {}, args]

QuantumEvolve[hamiltonian_QuantumOperator, (lindblad : Except[_List] -> gamma_) | (lindblad_ -> gamma : Except[_List]), args___] := QuantumEvolve[hamiltonian, ToList[lindblad] -> ToList[gamma], args]

QuantumEvolve[
    hamiltonian_ ? QuantumOperatorQ,
    lindblad : {___ ? QuantumOperatorQ} -> gammas_List,
    defaultState : _ ? QuantumStateQ | Automatic | None : Automatic,
    defaultParameter : _Symbol | {_Symbol, _ ? NumericQ, _ ? NumericQ} | Automatic : Automatic,
    opts : OptionsPattern[]
] := Enclose @ Block[{
    state = None, basis,
    matrix, jumps,
    parameter, parameterSpec,
    numericQ, solution,
    rhs, init,
    equations, return, param,
    phaseSpaceQ = False,
    mergeQ = TrueQ[OptionValue["MergeInterpolatingFunctions"]]
},
    If[ defaultState =!= None,
        state = Replace[defaultState, Automatic :> QuantumState[{"Register", hamiltonian["InputDimensions"]}]];
        phaseSpaceQ = state["Picture"] === "PhaseSpace" && state["VectorQ"] && AllTrue[Sqrt[state["Dimensions"]], IntegerQ]
    ];
    ConfirmAssert[hamiltonian["OutputDimensions"] == hamiltonian["InputDimensions"]];
    ConfirmAssert[AllTrue[lindblad, #["OutputDimensions"] == #["InputDimensions"] &]];
    ConfirmAssert[state === None || phaseSpaceQ || hamiltonian["InputDimension"] == state["Dimension"]];
    If[ defaultParameter === Automatic,
        parameter = First[Join[hamiltonian["Parameters"], Catenate[Through[lindblad["Parameters"]]]], \[FormalT]];
        parameterSpec = Replace[First[Join[hamiltonian["ParameterSpec"], Catenate[Through[lindblad["ParameterSpec"]]]], {}], {} :> {parameter, 0, 1}],

        parameter = Replace[defaultParameter, {p_Symbol, _, _} :> p];
        parameterSpec = Replace[defaultParameter, _Symbol :> {parameter, 0, 1}]
    ];
    ConfirmAssert[Developer`SymbolQ[parameter]];
    numericQ = MatchQ[defaultParameter, {_Symbol, _ ? NumericQ, _ ? NumericQ}];
    basis = If[state =!= None && phaseSpaceQ, state["Basis"], hamiltonian["OutputBasis"]];
    matrix = Progress`EvaluateWithProgress[
        (* TrigToExp helps with some examples *)
        TrigToExp @ Confirm @ If[mergeQ, MergeInterpolatingFunctions, Identity][
            If[ (phaseSpaceQ || state === None && lindblad =!= {}) && ! hamiltonian["MatrixQ"],
                HamiltonianTransitionRate[hamiltonian, basis],
                QuantumOperator[If[phaseSpaceQ, hamiltonian["Double"], hamiltonian], basis]["Matrix"] / I
            ]
        ],
        <|"Text" -> "Preparing Hamiltonian"|>
    ];
    jumps = Progress`EvaluateWithProgress[
        TrigToExp @ Confirm @ If[mergeQ, Map[MergeInterpolatingFunctions], Identity][
            If[ (phaseSpaceQ || state === None) && lindblad =!= {} && ! First[lindblad]["MatrixQ"],
                PadRight[gammas, Length[lindblad], 1] * LindbladTransitionRates[lindblad, basis],
                PadRight[Sqrt[gammas], Length[lindblad], 1] * (QuantumOperator[#, basis]["Matrix"] & /@ lindblad)
            ]
        ],
        <|"Text" -> "Preparing Lindblad jumps"|>
    ];
    If[ jumps =!= {},
        If[ phaseSpaceQ || state === None,
            matrix += Total[jumps],
            state = state["MatrixState"]
        ]
    ];
    rhs = With[{s = \[FormalS][parameter]}, If[
        phaseSpaceQ || state === None || jumps === {} && (state["VectorQ"] || hamiltonian["MatrixQ"]),
        matrix . s,
        matrix . s - s . matrix + Total[With[{L = #, Ldg = ConjugateTranspose[#]}, L . s . Ldg - 1 / 2 (Ldg . L . s + s . Ldg . L)] & /@ jumps]
    ]];
    If[ state =!= None,
        If[ phaseSpaceQ,
            (* always treat state as a single system in phase-space *)
            state = QuantumPhaseSpaceTransform[QuantumState[QuantumWeylTransform[state], Sqrt[state["Dimension"]]], basis],

            state = state["Split", state["Qudits"]];
            If[ hamiltonian["MatrixQ"],
                state = QuantumState[state["Double"], hamiltonian["QuditBasis"]],
                state = QuantumState[state, hamiltonian["Input"]["Dual"]]
            ]
        ]
    ];
    init = If[defaultState === None, IdentityMatrix[Length[matrix], SparseArray], state["State"]];
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
                Matrices[Dimensions[matrix]],
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
        Module[{time, protectedQ = MemberQ[Attributes[Evaluate[parameter]], Protected]},
            WithCleanup[
                Unprotect[\[FormalS]];
                If[protectedQ, Unprotect[Evaluate[parameter]]]
                ,
                Progress`EvaluateWithProgress[
                    NDSolveValue[
                        equations,
                        return,
                        param,
                        FilterRules[{opts}, Options[NDSolveValue]],
                        StepMonitor :> (time = parameter)
                    ],
                    <|"Text" -> "Integrating", "Progress" :> time / Last[param], "Percentage" :> time / Last[param], "ElapsedTime" -> Automatic, "RemainingTime" -> Automatic|>
                ]
                ,
                Protect[\[FormalS]];
                If[protectedQ, Protect[Evaluate[parameter]]]
            ]
        ],
        DSolveValue[
            equations,
            return,
            param,
            FilterRules[{opts}, Options[NDSolveValue]]
        ]
    ];
    If[ TrueQ[OptionValue["ReturnSolution"]], Return[solution]];
    If[ MatchQ[solution, _InterpolatingFunction],
        solution = ExpandInterpolatingFunction[solution, parameter]
    ];
    Which[
        state === None && SquareMatrixQ[solution],
        QuantumOperator[
            QuantumState[
                If[ Dimensions[solution] == hamiltonian["MatrixNameDimensions"] ^ 2,
                    ArrayReshape[
                        Transpose[ArrayReshape[solution, Join[#, #] & @ hamiltonian["MatrixNameDimensions"]], 2 <-> 3],
                        hamiltonian["MatrixNameDimensions"] ^ 2
                    ],
                    Flatten @ solution
                ],
                hamiltonian["Basis"]
            ],
            hamiltonian["Order"],
            "ParameterSpec" -> DeleteDuplicatesBy[Append[hamiltonian["ParameterSpec"], parameterSpec], First]
        ],
        stateQ[solution],
        If[ state =!= None && phaseSpaceQ,
            QuantumState[
                QuantumState[
                    QuantumWeylTransform[QuantumState[solution, basis, "Picture" -> "PhaseSpace"]],
                    Sqrt[basis["Dimensions"]]
                ]["Double"],
                basis,
                "ParameterSpec" -> DeleteDuplicatesBy[Append[basis["ParameterSpec"], parameterSpec], First]
            ],
            If[hamiltonian["MatrixQ"], #["Undouble"], #] & @ QuantumState[solution, basis, "ParameterSpec" -> DeleteDuplicatesBy[Append[basis["ParameterSpec"], parameterSpec], First]]
        ],
        True,
        Message[QuantumEvolve::error];
        solution
    ]
]

QuantumEvolve[hamiltonian_QuantumOperator, args___] := QuantumEvolve[hamiltonian, {} -> {}, args]


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


HamiltonianTransitionRate[h_QuantumOperator, args___] := Chop[QuantumPhaseSpaceTransform[QuantumOperator["Hamiltonian"[h]], args]["Matrix"] / I]

LindbladTransitionRates[ls : {___QuantumOperator}, args___] := Chop[QuantumPhaseSpaceTransform[QuantumOperator["Hamiltonian"[{#}]], args]["Matrix"] / I] & /@ ls

