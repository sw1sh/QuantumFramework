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


QuantumDiscreteOperator[tensor_ ? TensorQ /; TensorRank[tensor] > 2, args___, order : (_ ? orderQ) : {1}] := Module[{
    dimensions = TensorDimensions[tensor],
    rank,
    targetOrder
},
    rank = Max[Length[dimensions], Max[order]];
    targetOrder = Reverse[rank - order + 1];
    tensor = Transpose[tensor, Join[Complement[Range[rank], targetOrder], targetOrder]];
    QuantumDiscreteOperator[
        ArrayReshape[tensor, Times @@@ TakeDrop[Dimensions[tensor], - Length[targetOrder]]],
        args,
        order
    ]
]

QuantumDiscreteOperator[assoc_Association, args___] := With[{
    dimensions = basisElementNamesDimensions[Keys[assoc]]
},
    QuantumDiscreteOperator[
        ArrayReshape[Values[assoc], dimensions],
        args
    ]
]


QuantumDiscreteOperator::invalidState = "invalid state specification";

QuantumDiscreteOperator[matrix_ ? MatrixQ, args___, order : (_ ? orderQ) : {1}] := Module[{
    result, outputQudits, inputQudits
},
    result = Enclose[Module[{state, basis},
        basis = QuantumBasis[args];
        (* if no inputs assume same input/output basis *)
        {outputQudits, inputQudits} = Log[basis["MatrixNameDimensions"] /. {x_, 1} :> {x, x}, Dimensions[matrix]];
        ConfirmAssert[IntegerQ[outputQudits] && IntegerQ[inputQudits], Message[QuantumDiscreteOperator::invalidState]];
        state = ConfirmBy[
            QuantumDiscreteState[
                Flatten[matrix],
                QuantumBasis[QuantumBasis[basis, outputQudits], "Input" -> QuantumBasis[basis, inputQudits]["Output"]]
            ],
            QuantumDiscreteStateQ,
            Message[QuantumDiscreteOperator::invalidState]
        ];
        QuantumDiscreteOperator[state, order]
    ],
        $Failed &
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
                Flatten[
                ResourceFunction["BlockDiagonalMatrix"] @@
                    {
                        If[ qb["OutputDimension"] > qdo["OutputDimension"], IdentityMatrix[qb["OutputDimension"] - qdo["OutputDimension"]], Nothing],
                        qdo["Matrix"],
                        If[ qb["InputDimension"] > qdo["InputDimension"], IdentityMatrix[qb["InputDimension"] - qdo["InputDimension"]], Nothing]
                    }
                ],

                If[qb["Dimension"] === qdo["Dimension"], qb, QuantumBasis[qb, "Input" -> qb["Output"]]]
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
    matrix = qdo[{"OrderedMatrixRepresentation", qds["OutputQudits"]}]
},
    If[ qds["StateType"] === "Vector",
        QuantumDiscreteState[matrix . qds["StateVector"], qds["Basis"]],
        QuantumDiscreteState[matrix . qds["DensityMatrix"] . ConjugateTranspose[matrix], qds["Basis"]]
    ]
]

(qdo_QuantumDiscreteOperator ? QuantumDiscreteOperatorQ)[op_ ? QuantumDiscreteOperatorQ] /;
qdo["Picture"] === op["Picture"] && MemberQ[{"Heisenberg", "Interaction"}, op["Picture"]] :=
    QuantumDiscreteOperator[
        qdo[{"OrderedMatrixRepresentation", op["OutputQudits"]}] . op["OrderedMatrixRepresentation"],
        QuantumBasis[op["InputBasis"], "Output" -> qdo["Output"]],
        op["Order"]
    ]

