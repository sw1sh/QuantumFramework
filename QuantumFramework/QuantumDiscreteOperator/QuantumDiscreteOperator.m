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
    targetOrder, newTensor
},
    rank = Max[Length[dimensions], Max[order]];
    targetOrder = Reverse[rank - order + 1];
    newTensor = Transpose[tensor, Join[Complement[Range[rank], targetOrder], targetOrder]];
    QuantumDiscreteOperator[
        ArrayReshape[newTensor, Times @@@ TakeDrop[TensorDimensions[newTensor], rank - Length[targetOrder]]],
        args,
        order
    ]
]

QuantumDiscreteOperator[assoc_Association, args___, order : (_ ? orderQ) : {1}] := Enclose @ Module[{
    dimensions = basisElementNamesDimensions[Keys[assoc]],
    tensorDimensions
},
    ConfirmAssert[Length[assoc] > 0 && Equal @@ TensorDimensions /@ assoc];
    tensorDimensions = TensorDimensions @ First[assoc];
    QuantumDiscreteOperator[
        ArrayReshape[Values[assoc], Join[dimensions, tensorDimensions]],
        args,
        Join[Complement[Range[Length[tensorDimensions]], order], order]
    ]
]


QuantumDiscreteOperator::invalidState = "invalid state specification";

QuantumDiscreteOperator[matrix_ ? MatrixQ, args___, order : (_ ? orderQ) : {1}] := Module[{
    result
},
    result = Enclose[Module[{outputs, inputs, quditBasis, outputQudits, inputQudits, basis, state},
        {outputs, inputs} = Dimensions[matrix];
        quditBasis = QuantumBasis[args];
        (* if no inputs assume same input/output basis *)
        {outputQudits, inputQudits} = Log[quditBasis["MatrixNameDimensions"] /. {x_, 1} :> {x, x}, {outputs, inputs}];
        basis = If[ IntegerQ[outputQudits],
            QuantumBasis[quditBasis, outputQudits],
            QuantumTensorProduct[QuantumBasis[ConfirmBy[outputs / quditBasis["OutputDimension"], IntegerQ]], quditBasis]
        ];
        basis = If[ IntegerQ[inputQudits],
            QuantumBasis[basis,
                "Input" -> QuantumBasis[quditBasis, inputQudits]["Output"]],
            QuantumBasis[basis,
                "Input" -> QuantumTensorProduct[QuantumBasis[ConfirmBy[inputs / quditBasis["InputDimension"], IntegerQ]], quditBasis]["Output"]]
        ];
        state = ConfirmBy[
            QuantumDiscreteState[
                Flatten[matrix],
                basis
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

