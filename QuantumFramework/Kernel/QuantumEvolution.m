Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumEvolve"]



QuantumEvolve::error = "Differential Solver failed to find a solution"

QuantumEvolve[
    hamiltonian_ ? QuantumOperatorQ,
    defaultState : _ ? QuantumStateQ | Automatic : Automatic,
    defaultParameter : _Symbol | {_Symbol, _ ? NumericQ, _ ? NumericQ} | Automatic : Automatic,
    args___
] := Enclose @ Block[{
    state = Replace[defaultState, Automatic :> QuantumState[{"Register", hamiltonian["InputDimensions"]}]],
    parameter,
    parameterSpec,
    numericQ,
    solution
},
    ConfirmAssert[state["OutputDimensions"] == hamiltonian["InputDimensions"]];
    If[ defaultParameter === Automatic,
        parameter = First[hamiltonian["Parameters"], \[FormalT]];
        parameterSpec = Replace[First[hamiltonian["ParameterSpec"], {}], {} :> {parameter, 0, 1}],

        parameter = Replace[defaultParameter, {p_Symbol, _, _} :> p];
        parameterSpec = Replace[defaultParameter, _Symbol :> {parameter, 0, 1}]
    ];
    numericQ = MatchQ[defaultParameter, {_Symbol, _ ? NumericQ, _ ? NumericQ}];
    solution = If[numericQ, NDSolveValue, DSolveValue][
        {
            \[FormalS]'[parameter] == 1 / I If[numericQ, Normal, Identity] @ TrigToExp[hamiltonian["Matrix"]] . \[FormalS][parameter],
            \[FormalS][0] == If[numericQ, Normal, Identity] @ state["State"]
        },
        If[ numericQ,
            \[FormalS],
            Element[\[FormalS][parameter], If[state["VectorQ"], Vectors[state["Dimension"]], Matrices[state["MatrixDimensions"]]]]
        ],
        If[numericQ, parameterSpec, parameter],
        args
    ];
    If[ MatchQ[solution, _InterpolatingFunction],
        solution = With[{data = ResourceFunction["InterpolatingFunctionData"][solution]},
            Interpolation[Thread[{data["Grid"], #}], InterpolationOrder -> data["InterpolationOrder"]][parameter] & /@ Transpose[data["ValuesOnGrid"]]
        ]
    ];
    If[ stateQ[solution],
        QuantumState[
            solution,
            hamiltonian["OutputBasis"],
            "ParameterSpec" -> parameterSpec
        ],
        Message[QuantumEvolve::error];
        solution
    ]
]

