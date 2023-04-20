Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumEvolve"]



QuantumEvolve[
    hamiltonian_ ? QuantumOperatorQ,
    defaultState : _ ? QuantumStateQ | Automatic : Automatic,
    defaultParameter : _Symbol | {_Symbol, _ ? NumericQ, _ ? NumericQ} | Automatic : Automatic,
    args___
] := Enclose @ Block[{
    state = Replace[defaultState, Automatic :> QuantumState[{"Register", hamiltonian["InputDimensions"]}]],
    parameter,
    parameterSpec,
    numericQ
},
    ConfirmAssert[state["OutputDimensions"] == hamiltonian["InputDimensions"]];
    If[ defaultParameter === Automatic,
        parameter = First[hamiltonian["Parameters"], \[FormalT]];
        parameterSpec = Replace[First[hamiltonian["ParameterSpec"], {}], {} :> {parameter, 0, 1}],

        parameter = Replace[defaultParameter, {p_Symbol, _, _} :> p];
        parameterSpec = Replace[defaultParameter, _Symbol :> {parameter, 0, 1}]
    ];
    numericQ = MatchQ[defaultParameter, {_Symbol, _ ? NumericQ, _ ? NumericQ}];
    QuantumState[
        If[numericQ, NDSolveValue, DSolveValue][
            {
                \[FormalS]'[parameter] == 1 / I TrigToExp[hamiltonian["Matrix"]] . \[FormalS][parameter],
                \[FormalS][0] == If[numericQ, Normal, Identity] @ state["State"]
            },
            If[ numericQ,
                \[FormalS][parameterSpec[[-1]]],
                Element[\[FormalS][parameter], If[state["VectorQ"], Vectors[state["Dimension"]], Matrices[state["MatrixDimensions"]]]]
            ],
            If[numericQ, parameterSpec, parameter],
            args
        ],
        hamiltonian["OutputBasis"],
        If[numericQ, {}, "ParameterSpec" -> parameterSpec]
    ]
]

