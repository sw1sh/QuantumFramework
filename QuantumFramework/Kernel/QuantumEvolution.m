Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumEvolve"]



QuantumEvolve::error = "Differential Solver failed to find a solution"

Options[QuantumEvolve] = DeleteDuplicatesBy[First] @ Join[{"AdditionalEquations" -> {}}, Options[NDSolveValue], Options[DSolveValue]]

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
    method
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
    matrix = If[numericQ, Normal, Identity] @ TrigToExp[hamiltonian["Matrix"]];
    method = If[numericQ, NDSolveValue, DSolveValue];
    solution = method[
        {
            \[FormalS]'[parameter] == 1 / I If[
                defaultState === None || state["VectorQ"],
                matrix . \[FormalS][parameter],
                matrix . \[FormalS][parameter] - \[FormalS][parameter] . matrix
            ],
            \[FormalS][0] == If[numericQ, Normal, Identity] @
                If[defaultState === None, IdentityMatrix[hamiltonian["InputDimension"], SparseArray], state["State"]]
        } ~Join~ Flatten[{OptionValue["AdditionalEquations"]}],
        If[ numericQ,
            \[FormalS],
            Element[
                \[FormalS][parameter],
                If[ defaultState === None,
                    Matrices[hamiltonian["MatrixNameDimensions"]],
                    If[state["VectorQ"], Vectors[state["Dimension"]], Matrices[state["MatrixDimensions"]]]
                ]
            ]
        ],
        If[numericQ, parameterSpec, parameter],
        FilterRules[{opts}, Options[method]]
    ];
    If[ MatchQ[solution, _InterpolatingFunction],
        solution =
            Map[
                Interpolation[Thread[{solution["Grid"], #}], InterpolationOrder -> solution["InterpolationOrder"]][parameter] &,
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

