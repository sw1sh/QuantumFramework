Package["QuantumFramework`"]

PackageExport["QuantumDiscreteOperator"]

PackageScope["QuantumDiscreteOperatorQ"]



QuantumDiscreteOperatorQ[QuantumDiscreteOperator[qds_, order_]] :=
    QuantumDiscreteStateQ[qds] &&
    orderQ[order]

QuantumDiscreteOperatorQ[___] := False


(* constructors *)

QuantumDiscreteOperator[qds_QuantumDiscreteState ? QuantumDiscreteStateQ] :=
    QuantumDiscreteOperator[qds, {1}]


QuantumDiscreteOperator[assoc_Association, args___] := With[{
    dimensions = basisElementNamesDimensions[Keys[assoc]]
},
    QuantumDiscreteOperator[
        ArrayReshape[Values[assoc], dimensions],
        AssociationThread[Keys[assoc], QuantumBasis[dimensions]["BasisElements"]],
        args
    ]
]


QuantumDiscreteOperator::invalidState = "invalid state specification";

QuantumDiscreteOperator[matrix_ ? MatrixQ, args___, order : (_ ? orderQ) : {1}] := Module[{result},
    result = Enclose @ With[{state = ConfirmBy[
            With[{state = QuantumDiscreteState[Flatten[matrix], args]},
                QuantumDiscreteState[state, "InputQudits" -> Length[order]]
            ],
            QuantumDiscreteStateQ,
            Message[QuantumDiscreteOperator::invalidState]
        ]},
        QuantumDiscreteOperator[
            QuantumDiscreteState[
                state,
                QuantumBasis[
                    state["Basis"],
                    MapAt[ReplaceAll[Ket -> Bra], state["BasisElementNames"], {All, - state["InputQudits"] ;;}]
                ]
            ],
            order
        ]
    ];
    result /; !FailureQ[result]
]


(* Mutation *)

QuantumDiscreteOperator[qdo_ ? QuantumDiscreteOperatorQ] := qdo

QuantumDiscreteOperator[qdo_ ? QuantumDiscreteOperatorQ, order_ ? orderQ] :=
    QuantumDiscreteOperator[qdo["State"], order]

QuantumDiscreteOperator[qdo_ ? QuantumDiscreteOperatorQ, args__, order_ ? orderQ] := Enclose @
    QuantumDiscreteOperator[qdo, ConfirmBy[QuantumBasis[qdo["Basis"], args], QuantumBasisQ], order]

QuantumDiscreteOperator[qdo_ ? QuantumDiscreteOperatorQ, args__] := Enclose @
    QuantumDiscreteOperator[qdo, ConfirmBy[QuantumBasis[qdo["Basis"], args], QuantumBasisQ]]


(* change of basis *)

QuantumDiscreteOperator[qdo_ ? QuantumDiscreteOperatorQ, qb_ ? QuantumBasisQ] := Module[{result},
    result = Enclose @ QuantumDiscreteOperator[
        ConfirmBy[
            QuantumDiscreteState[
                Flatten @ If[ qb["Size"] > qdo["Dimension"] ^ 2,
                    ResourceFunction["BlockDiagonalMatrix"][qdo["Matrix"], IdentityMatrix[qb["Size"] - qdo["Dimension"] ^ 2]],
                    qdo["Matrix"]
                ],
                qb
            ],
            QuantumDiscreteStateQ,
            Message[QuantumDiscreteOperator::invalidState]
        ],
        qdo["Order"]
    ];
    result /; !FailureQ[result]
]


(* composition *)

(qdo_QuantumDiscreteOperator ? QuantumDiscreteOperatorQ)[qds_ ? QuantumDiscreteStateQ] /;
qdo["Picture"] === qdo["Picture"] && qds["Picture"] =!= "Heisenberg" := With[{
    matrix = qdo[{"OrderedMatrixRepresentation", qds["Qudits"]}]
},
    If[ qds["StateType"] === "Vector",
        QuantumDiscreteState[matrix . qds["StateVector"], qds["Basis"]],
        QuantumDiscreteState[matrix . qds["DensityMatrix"] . ConjugateTranspose[matrix], qds["Basis"]]
    ]
]

(qdo_QuantumDiscreteOperator ? QuantumDiscreteOperatorQ)[op_ ? QuantumDiscreteOperatorQ] /;
qdo["Picture"] === op["Picture"] && MemberQ[{"Heisenberg", "Interaction"}, op["Picture"]] :=
    QuantumDiscreteOperator[qdo["OrderedMatrixRepresentation"] . op["OrderedMatrixRepresentation"], op["Order"]]

