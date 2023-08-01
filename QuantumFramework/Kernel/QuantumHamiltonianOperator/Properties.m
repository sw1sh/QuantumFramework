Package["Wolfram`QuantumFramework`"]



$QuantumHamiltonianOperatorProperties = {
    "QuantumOperator",
    "ParameterSpec", "Parameter", "InitialParameter", "FinalParameter",
    "InitialMatrixRepresentation",
    "FinalMatrixRepresentation",
    "InitialOrderedMatrixRepresentation",
    "FinalOrderedMatrixRepresentation",
    "MinimumGap", "InitialGap", "FinalGap", "Gap", "EvolutionOperator",
    "InitialEvolutionOperator", "FinalEvolutionOperator"
};

QuantumHamiltonianOperator["Properties"] := DeleteDuplicates @ Join[
    $QuantumHamiltonianOperatorProperties,
    $QuantumOperatorProperties
]

qho_QuantumHamiltonianOperator["ValidQ"] := QuantumHamiltonianOperatorQ[qho]


QuantumHamiltonianOperatorProp[qho_, "Properties"] :=
    Union @ Join[QuantumHamiltonianOperator["Properties"], qho["QuantumOperator"]["Properties"]]


QuantumHamiltonianOperator::undefprop = "QuantumHamiltonianOperator property `` is undefined for this operator";


(qho_QuantumHamiltonianOperator[prop_ ? propQ, args___]) /; QuantumHamiltonianOperatorQ[qho] := With[{
    result = QuantumHamiltonianOperatorProp[qho, prop, args]
    },
    If[ TrueQ[$QuantumFrameworkPropCache] && QuantumHamiltonianOperatorProp[qho, "Basis"]["ParameterArity"] == 0,
        QuantumHamiltonianOperatorProp[qho, prop, args] = result,
        result
    ] /; !MatchQ[result, _QuantumHamiltonianOperatorProp] || Message[QuantumHamiltonianOperator::undefprop, prop]
]


(* getters *)

QuantumHamiltonianOperatorProp[_[op_], "QuantumOperator"] := op

QuantumHamiltonianOperatorProp[qho_, "ParameterSpec"] := First @ qho["Basis"]["ParameterSpec"]

QuantumHamiltonianOperatorProp[qho_, "Parameter"] := qho["ParameterSpec"][[1]]

QuantumHamiltonianOperatorProp[qho_, "InitialParameter"] := qho["ParameterSpec"][[2]]

QuantumHamiltonianOperatorProp[qho_, "FinalParameter"] := qho["ParameterSpec"][[3]]


QuantumHamiltonianOperatorProp[qho_, "Gap"] := qho["Gap", 100]

QuantumHamiltonianOperatorProp[qho_, {"Gap", n_Integer ? Positive}] :=
    Module[ {eigenvalues, gap},
        eigenvalues = Sort[qho["Eigenvalues"]][[1 ;; 2]];
        gap = Abs[Differences[eigenvalues /. qho["Parameter"] -> #]] &;
        First[gap[#]] & /@ Range[qho["InitialParameter"], qho["FinalParameter"], (qho["FinalParameter"] - qho["InitialParameter"]) / n]
    ]

QuantumHamiltonianOperatorProp[qho_, {"Gap", parameterValue_}] :=
    Module[ {eigenvalues, gap},
        eigenvalues = Sort[qho["Eigenvalues"]][[1 ;; 2]];
        gap = Abs[Differences[eigenvalues /. qho["Parameter"] -> #]] &;
        First[gap[parameterValue]]
    ]

QuantumHamiltonianOperatorProp[qho_, "MinimumGap"] := FindMinimum[
     qho["Gap", qho["Parameter"]], qho["ParameterSpec"]
]

QuantumHamiltonianOperatorProp[qho_, "InitialGap"] := qho["Gap", qho["InitialParameter"]]

QuantumHamiltonianOperatorProp[qho_, "FinalGap"] :=  qho["Gap", qho["FinalParameter"]]

QuantumHamiltonianOperatorProp[qho_, "MatrixRepresentation", param_] := Normal[qho["MatrixRepresentation"]] /. qho["Parameter"] -> param

QuantumHamiltonianOperatorProp[qho_, "InitialMatrixRepresentation"] := qho["MatrixRepresentation", qho["InitialParameter"]]

QuantumHamiltonianOperatorProp[qho_, "FinalMatrixRepresentation"] := qho["MatrixRepresentation", qho["FinalParameter"]]

QuantumHamiltonianOperatorProp[qho_, "EvolutionOperator"] := Module[{
    leftEquations, rightEquations, initialEquations, equations
},
        leftEquations = Flatten[
            (1 / I) qho["MatrixRepresentation"] . Partition[
                    Subscript["u", #][qho["Parameter"]] & /@ Range[qho["OutputDimension"] ^ 2],
                    qho["OutputDimension"]
                ]
        ];
        rightEquations = Subscript["u", #]'[qho["Parameter"]] & /@ Range[qho["OutputDimension"] ^ 2];
        equations = Equal @@@ Transpose[{rightEquations, leftEquations}];
        initialEquations = Equal @@@
            Transpose @ {
                Subscript["u", #][qho["InitialParameter"]] & /@ Range[qho["OutputDimension"] ^ 2],
                Flatten[IdentityMatrix[qho["OutputDimension"]]]
            };
        QuantumHamiltonianOperator[
            QuantumOperator[
                Values @ Partition[Flatten @
                    DSolve[
                        Join[equations, initialEquations],
                        Subscript["u", #][qho["Parameter"]] & /@ Range[qho["OutputDimension"] ^ 2],
                        Evaluate @ qho["Parameter"]
                    ],
                    qho["OutputDimension"]
                ],
                qho["Order"]
            ],
            qho["ParameterSpec"]
        ]
    ]

QuantumHamiltonianOperatorProp[qho_, "EvolutionOperator", param_] := qho["EvolutionOperator"] /. qho["Parameter"] -> param

QuantumHamiltonianOperatorProp[qho_, "InitialEvolutionOperator"] := qho["EvolutionOperator", qho["InitialParameter"]]

QuantumHamiltonianOperatorProp[qho_, "FinalEvolutionOperator"] := qho["EvolutionOperator", qho["FinalParameter"]]


(* operator properties *)

QuantumHamiltonianOperatorProp[qho_, args : PatternSequence[prop_String, ___]] /;
    MemberQ[Intersection[qho["QuantumOperator"]["Properties"], qho["Properties"]], prop] := qho["QuantumOperator"][args]

