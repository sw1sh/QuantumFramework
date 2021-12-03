Package["Wolfram`QuantumFramework`"]


QuantumHamiltonianOperator[initialMatrixRepresentation_ -> finalMatrixRepresentation_, args___] :=
    QuantumHamiltonianOperator[{"LinearInterpolation", initialMatrixRepresentation, finalMatrixRepresentation}, args]

QuantumHamiltonianOperator[{"LinearInterpolation", initialMatrixRepresentation_, finalMatrixRepresentation_},
   {parameter_, initialParameter_, finalParameter_, parameterStep_}, args___] :=
    QuantumHamiltonianOperator[
        ((finalParameter - parameter) / (finalParameter - initialParameter)) *
            initialMatrixRepresentation + ((parameter - initialParameter) / (finalParameter - initialParameter)) *
        finalMatrixRepresentation,
        {parameter, initialParameter, finalParameter, parameterStep},
        args
    ]

QuantumHamiltonianOperator[{"EvolutionOperator", matrixRepresentation_ ? MatrixQ},
    {parameter_Symbol, initialParameter_, finalParameter_, parameterStep_}, args___] := Module[{
    matrixSize, hamiltonianMatrix, matrixDerivative
},
    matrixSize = Length[matrixRepresentation];
    hamiltonianMatrix = Partition[
        Subscript[\[FormalC], #] & /@ Range[matrixSize ^ 2],
        matrixSize
    ];
    matrixDerivative = (D[# /. parameter -> \[FormalX], \[FormalX]] & /@ Flatten[matrixRepresentation]) /. \[FormalX] -> parameter;
    hamiltonianMatrix = hamiltonianMatrix /.
        Cases[
            Reduce[
                And @@ (
                    Equal @@ # & /@ Transpose[{Flatten[I matrixDerivative], Flatten[hamiltonianMatrix . matrixRepresentation]}]
                ) // Simplify
            ] /. {And -> List, Equal -> Rule} // Flatten,
            _Rule
        ];
    QuantumHamiltonianOperator[hamiltonianMatrix, {parameter, initialParameter, finalParameter, parameterStep}, args]
]

QuantumHamiltonianOperator[{qo_ ? QuantumOperatorQ, params___}, args___] :=
    QuantumHamiltonianOperator[{"EvolutionOperator", qo["MatrixRepresentation"]}, {params}, args]


QuantumHamiltonianOperator[{"Ising", coefficientMatrix_List},
    {parameter_Symbol, initialParameter_, finalParameter_, parameterStep_},
    order : (_ ? orderQ) : {1, 2}] := Module[{
    qubitCount, couplingOrders, couplingTerms, individualTerms, hamiltonianMatrix
},
    qubitCount = Length[order];
    couplingOrders = Subsequences[Range[qubitCount], {2}];
    couplingTerms = QuantumTensorProduct[
         QuantumOperator["PauliZ", {First[#]}],
         QuantumOperator["PauliZ", {Last[#]}]]["OrderedMatrixRepresentation"] & /@ couplingOrders;
    couplingTerms = Table[
        If[ Max[couplingOrders[[i]]] < qubitCount,
            KroneckerProduct[
                couplingTerms[[i]],
                IdentityMatrix[2 ^ (qubitCount - Max[couplingOrders[[i]]])]
            ],
            couplingTerms[[i]]
          ],
        {i, Length[couplingOrders]}
    ];
    couplingTerms = Total @
        Table[With[{n = First[couplingOrders[[k]]],
            m = Last[couplingOrders[[k]]]},
           coefficientMatrix[[n, m]] couplingTerms[[k]]
        ], {k, Length[couplingOrders]}
    ];
    individualTerms = QuantumTensorProduct[
         QuantumOperator["PauliZ", {#}],
         QuantumOperator[IdentityMatrix[2 ^ (qubitCount - 1)], Complement[Range[qubitCount], {#}]]
    ]["OrderedMatrixRepresentation"] & /@ Range[qubitCount];
    individualTerms = Total @ MapThread[Times, {Diagonal[coefficientMatrix], individualTerms}];
    hamiltonianMatrix = individualTerms + couplingTerms;
    QuantumHamiltonianOperator[QuantumOperator[hamiltonianMatrix, order], {parameter, initialParameter, finalParameter, parameterStep}]
]


QuantumHamiltonianOperator[{"TransverseIsing", coefficientMatrix_List},
    {parameter_Symbol, initialParameter_, finalParameter_, parameterStep_},
    order : (_ ? orderQ) : {1, 2}] := Module[{
    qubitCount, couplingOrders, couplingTerms, individualTerms, hamiltonianMatrix
},
    qubitCount = Length[order];
    couplingOrders = Subsequences[Range[qubitCount], {2}];
    couplingTerms = QuantumTensorProduct[
        QuantumOperator["PauliZ", {First[#]}],
        QuantumOperator["PauliZ", {Last[#]}]]["OrderedMatrixRepresentation"] & /@ couplingOrders;
    couplingTerms = Table[
        If[ Max[couplingOrders[[i]]] < qubitCount,
            KroneckerProduct[
                couplingTerms[[i]],
                IdentityMatrix[2^(qubitCount - Max[couplingOrders[[i]]])]
            ],
            couplingTerms[[i]]
        ],
        {i, Length[couplingOrders]}
    ];
    couplingTerms = Total @ Table[
        With[{
            n = First[couplingOrders[[k]]],
            m = Last[couplingOrders[[k]]]
        },
           (coefficientMatrix[[n, m]] + coefficientMatrix[[m, n]]) couplingTerms[[k]] / 2
       ],
       {k, Length[couplingOrders]}
    ];
    individualTerms = QuantumTensorProduct[
         QuantumOperator["PauliX", {#}],
         QuantumOperator[
            IdentityMatrix[2 ^ (qubitCount - 1)],
            Complement[Range[qubitCount], {#}]]
        ]["OrderedMatrixRepresentation"] & /@ Range[qubitCount];
    individualTerms = Total @ MapThread[Times, {Diagonal[coefficientMatrix], individualTerms}];
    hamiltonianMatrix = individualTerms + couplingTerms;
    QuantumHamiltonianOperator[QuantumOperator[hamiltonianMatrix, order], {parameter, initialParameter, finalParameter, parameterStep}]
]


QuantumHamiltonianOperator[args : Except[_ ? QuantumOperatorQ], range : {_Symbol, _, _, _}] :=
    Enclose @ QuantumHamiltonianOperator[ConfirmBy[QuantumOperator[args], QuantumOperatorQ], range]

