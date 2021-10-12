Package["Wolfram`QuantumFramework`"]

PackageScope["QuantumHamiltonianOperatorProp"]



$QuantumHamiltonianOperatorProperties = {
    "QuantumOperator",
    "Parameter", "InitialParameter", "FinalParameter", "ParameterStep",
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
    DeleteDuplicates @ Join[QuantumHamiltonianOperator["Properties"], qho["QuantumOperator"]["Properties"]]


QuantumHamiltonianOperator::undefprop = "QuantumHamiltonianOperator property `` is undefined for this operator";


(qho_QuantumHamiltonianOperator[prop_ ? propQ, args___]) /; QuantumHamiltonianOperatorQ[qho] := With[{
    result = QuantumHamiltonianOperatorProp[qho, prop, args]
    },
    If[TrueQ[$QuantumFrameworkPropCache], QuantumHamiltonianOperatorProp[qho, prop, args] = result, result]
        /; !MatchQ[result, _QuantumHamiltonianOperatorProp] || Message[QuantumHamiltonianOperator::undefprop, prop]
]


(* getters *)

QuantumHamiltonianOperatorProp[_[op_, _], "QuantumOperator"] := op

QuantumHamiltonianOperatorProp[_[_, {param_, _, _, _}], "Parameter"] := param

QuantumHamiltonianOperatorProp[_[_, {_, init_, _, _}], "InitialParameter"] := init

QuantumHamiltonianOperatorProp[_[_, {_, _, final_, _}], "FinalParameter"] := final

QuantumHamiltonianOperatorProp[_[_, {_, _, _, step_}], "ParameterStep"] := step


QuantumHamiltonianOperatorProp[_[_, {param_, init_, final_, _}], "Range"] := {param, init, final}



QuantumHamiltonianOperatorProp[qho_, "Gap"] :=
    Module[ {eigenvalues, gap},
        eigenvalues = Sort[qho["Eigenvalues"]][[1 ;; 2]];
        gap = Abs[Differences[eigenvalues /. qho["Parameter"] -> #]] &;
        First[gap[#]] & /@ Range[qho["InitialParameter"], qho["FinalParameter"], qho["ParameterStep"]]
    ]

QuantumHamiltonianOperatorProp[qho_, {"Gap", parameterValue_}] :=
    Module[ {eigenvalues, gap},
        eigenvalues = Sort[qho["Eigenvalues"]][[1 ;; 2]];
        gap = Abs[Differences[eigenvalues /. qho["Parameter"] -> #]] &;
        First[gap[parameterValue]]
    ]

QuantumHamiltonianOperatorProp[qho_, "MinimumGap"] := FindMinimum[
     qho[{"Gap", qho["Parameter"]}], {qho["Parameter"], qho["InitialParameter"], qho["FinalParameter"]}
]

QuantumHamiltonianOperatorProp[qho_, "InitialGap"] := qho[{"Gap", qho["InitialParameter"]}]

QuantumHamiltonianOperatorProp[qho_, "FinalGap"] :=  qho[{"Gap", qho["FinalParameter"]}]

QuantumHamiltonianOperatorProp[qho_, {"MatrixRepresentation", param_}] := qho["MatrixRepresentation"] /. qho["Parameter"] -> param

QuantumHamiltonianOperatorProp[qho_, "InitialMatrixRepresentation"] := qho[{"MatrixRepresentation", qho["InitialParameter"]}]

QuantumHamiltonianOperatorProp[qho_, "FinalMatrixRepresentation"] := qho[{"MatrixRepresentation", qho["FinalParameter"]}]

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
        QuantumOperator[Values @
            Partition[Flatten @
                NDSolve[
                    Join[equations, initialEquations],
                    Subscript["u", #][qho["Parameter"]] & /@ Range[qho["OutputDimension"] ^ 2],
                    Evaluate @ qho["Range"]
                ],
                qho["OutputDimension"]
            ],
            qho["Order"]
        ]
    ]

QuantumHamiltonianOperatorProp[qho_, {"EvolutionOperator", param_}] := qho["EvolutionOperator"] /. qho["Parameter"] -> param

QuantumHamiltonianOperatorProp[qho_, "InitialEvolutionOperator"] := qho[{"EvolutionOperator", qho["InitialParameter"]}]

QuantumHamiltonianOperatorProp[qho_, "FinalEvolutionOperator"] := qho[{"EvolutionOperator", qho["FinalParameter"]}]


(* operator properties *)

QuantumHamiltonianOperatorProp[qho_, args : PatternSequence[prop_String, ___] | PatternSequence[{prop_String, ___}, ___]] /;
    MemberQ[Intersection[qho["QuantumOperator"]["Properties"], qho["Properties"]], prop] := qho["QuantumOperator"][args]

