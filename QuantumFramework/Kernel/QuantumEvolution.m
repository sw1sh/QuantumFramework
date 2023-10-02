Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumEvolve"]



QuantumEvolve::error = "Differential Solver failed to find a solution"

Options[QuantumEvolve] = DeleteDuplicatesBy[First] @ Join[
    {"AdditionalEquations" -> {}, "ReturnEquations" -> False},
    Options[NDSolveValue], Options[DSolveValue]
]

QuantumEvolve[
    hamiltonian_ ? QuantumOperatorQ,
    defaultState : _ ? QuantumStateQ | Automatic | None : Automatic,
    defaultParameter : _Symbol | {_Symbol, _ ? NumericQ, _ ? NumericQ} | Automatic : Automatic,
    opts : OptionsPattern[]
] := Enclose @ Block[{
    state = None,
    matrix,
    parameter, parameterSpec,
    numericQ, solution,
    method,
    equations, return, param
},
    If[ defaultState =!= None,
        state = Replace[defaultState, Automatic :> QuantumState[{"Register", hamiltonian["InputDimensions"]}]];
        ConfirmAssert[state["OutputDimensions"] == hamiltonian["InputDimensions"]]
    ];
    If[ defaultParameter === Automatic,
        parameter = First[hamiltonian["Parameters"], \[FormalT]];
        parameterSpec = Replace[First[hamiltonian["ParameterSpec"], {}], {} :> {parameter, 0, 1}],

        parameter = Replace[defaultParameter, {p_Symbol, _, _} :> p];
        parameterSpec = Replace[defaultParameter, _Symbol :> {parameter, 0, 1}]
    ];
    numericQ = MatchQ[defaultParameter, {_Symbol, _ ? NumericQ, _ ? NumericQ}];
    matrix = Normal @ Confirm @ MergeInterpolatingFunctions @ TrigToExp[hamiltonian["Matrix"]];
    method = If[numericQ, NDSolveValue, DSolveValue];
    equations = Join[
        {
            \[FormalS]'[parameter] == 1 / I If[
                defaultState === None || state["VectorQ"],
                matrix . \[FormalS][parameter],
                matrix . \[FormalS][parameter] - \[FormalS][parameter] . matrix
            ],
            \[FormalS][0] ==
                If[defaultState === None, IdentityMatrix[hamiltonian["InputDimension"]], Normal @ state["State"]]
        },
        Flatten[{OptionValue["AdditionalEquations"]}]
    ];
    return = If[ numericQ,
        \[FormalS],
        Element[
            \[FormalS][parameter],
            If[ defaultState === None,
                Matrices[hamiltonian["MatrixNameDimensions"]],
                If[state["VectorQ"], Vectors[state["Dimension"]], Matrices[state["MatrixDimensions"]]]
            ]
        ]
    ];
    param = If[numericQ, parameterSpec, parameter];
    If[TrueQ[OptionValue["ReturnEquations"]], Return[{equations, return, param}]];
    Unprotect[\[FormalS]];
    solution = method[
        equations,
        return,
        param,
        FilterRules[{opts}, Options[method]]
    ];
    Protect[\[FormalS]];
    If[ MatchQ[solution, _InterpolatingFunction],
        solution =
            Map[
                Interpolation[Thread[{solution["Grid"], #}], InterpolationOrder -> 1][parameter] &,
                Transpose[solution["ValuesOnGrid"], InversePermutation[Cycles[{Range[Length[solution["OutputDimensions"]] + 1]}]]],
                {-2}
            ]
    ];
    Which[
        defaultState === None && SquareMatrixQ[solution],
        QuantumOperator[
            solution,
            hamiltonian["Order"],
            hamiltonian["Basis"],
            "ParameterSpec" -> parameterSpec
        ],
        stateQ[solution],
        QuantumState[
            solution,
            hamiltonian["OutputBasis"],
            "ParameterSpec" -> parameterSpec
        ],
        True,
        Message[QuantumEvolve::error];
        solution
    ]
]


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
	grid = Intersection @@ Through[ifs["Grid"]];
	Interpolation[{#, Normal @ SparseArray[Thread[pos -> (values /. param -> #)], Dimensions[array]]} & /@ Catenate[grid], InterpolationOrder -> 1][param]
]

MergeInterpolatingFunctions[array_ ? ArrayQ] := MergeInterpolatingFunctions[SparseArray[array]]

