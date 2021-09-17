Package["QuantumFramework`"]

PackageExport["QuantumOperator"]

PackageScope["QuantumOperatorQ"]



QuantumOperator::invalidOrder = "order should be a list of distinct input qudit positions"

QuantumOperatorQ[QuantumOperator[qs_, order_]] :=
    QuantumStateQ[qs] &&
    (orderQ[order] (*&& ContainsAll[Range[qs["InputQudits"]], order]*) || (Message[QuantumOperator::invalidOrder]; False))

QuantumOperatorQ[___] := False


(* constructors *)

QuantumOperator[qs_QuantumState ? QuantumStateQ] :=
    QuantumOperator[qs, {1}]


QuantumOperator[tensor_ ? TensorQ /; TensorRank[tensor] > 2, args___, order : (_ ? orderQ) : {1}] := Module[{
    dimensions = TensorDimensions[tensor],
    rank,
    targetOrder, newTensor
},
    rank = Max[Length[dimensions], Max[order]];
    targetOrder = Reverse[rank - order + 1];
    newTensor = Transpose[tensor, Join[Complement[Range[rank], targetOrder], targetOrder]];
    QuantumOperator[
        ArrayReshape[newTensor, Times @@@ TakeDrop[TensorDimensions[newTensor], rank - Length[targetOrder]]],
        args,
        order
    ]
]

QuantumOperator[assoc_Association, args___, order : (_ ? orderQ) : {1}] := Enclose @ Module[{
    dimensions = basisElementNamesDimensions[Keys[assoc]],
    tensorDimensions
},
    ConfirmAssert[Length[assoc] > 0 && Equal @@ TensorDimensions /@ assoc];
    tensorDimensions = TensorDimensions @ First[assoc];
    QuantumOperator[
        ArrayReshape[Values[assoc], Join[dimensions, tensorDimensions]],
        args,
        Join[Complement[Range[Length[tensorDimensions]], order], order]
    ]
]


QuantumOperator::invalidState = "invalid state specification";

QuantumOperator[matrix_ ? MatrixQ, args___, order : (_ ? orderQ) : {1}] := Module[{
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
            QuantumState[
                Flatten[matrix],
                basis
            ],
            QuantumStateQ,
            Message[QuantumOperator::invalidState]
        ];
        QuantumOperator[state, order]
    ],
        $Failed &
    ];
    result /; !FailureQ[result]
]


(* Mutation *)

QuantumOperator[qo_ ? QuantumOperatorQ] := qo

QuantumOperator[qo_ ? QuantumOperatorQ, order_ ? orderQ] :=
    QuantumOperator[qo["State"], order]

QuantumOperator[qo_ ? QuantumOperatorQ, args__, order_ ? orderQ] := Enclose @
    QuantumOperator[qo, ConfirmBy[QuantumBasis[qo["Basis"], args], QuantumBasisQ], order]

QuantumOperator[qo_ ? QuantumOperatorQ, args__] := Enclose @
    QuantumOperator[qo, ConfirmBy[QuantumBasis[qo["Basis"], args], QuantumBasisQ]]


(* change of basis *)

QuantumOperator[qo_ ? QuantumOperatorQ, qb_ ? QuantumBasisQ] := Module[{result},
    result = Enclose @ QuantumOperator[
        ConfirmBy[
            QuantumState[
                Flatten[
                ResourceFunction["BlockDiagonalMatrix"] @@
                    {
                        If[ qb["OutputDimension"] > qo["OutputDimension"], IdentityMatrix[qb["OutputDimension"] - qo["OutputDimension"]], Nothing],
                        qo["Matrix"],
                        If[ qb["InputDimension"] > qo["InputDimension"], IdentityMatrix[qb["InputDimension"] - qo["InputDimension"]], Nothing]
                    }
                ],

                If[qb["Dimension"] === qo["Dimension"], qb, QuantumBasis[qb, "Input" -> qb["Output"]]]
            ],
            QuantumStateQ,
            Message[QuantumOperator::invalidState]
        ],
        qo["Order"]
    ];
    result /; !FailureQ[result]
]


(* composition *)

QuantumOperator::incompatiblePictures = "Pictures `` and `` are incompatible with this operation"

(qo_QuantumOperator ? QuantumOperatorQ)[qs_ ? QuantumStateQ] /;
qo["Picture"] === qo["Picture"] && (
    qs["Picture"] =!= "Heisenberg" || Message[QuantumOperator::incompatiblePictures, qo["Picture"], op["Picture"]]) := With[{
    matrix = qo[{"OrderedMatrixRepresentation", qs["OutputQudits"]}]
},
    QuantumState[
        If[ qs["StateType"] === "Vector",
            matrix . qs["StateVector"],
            matrix . qs["DensityMatrix"] . ConjugateTranspose[matrix]
        ],
        QuantumBasis[qs["Basis"], "Label" -> qo["Label"] @* qs["Label"]]
    ]
]

(qo_QuantumOperator ? QuantumOperatorQ)[op_ ? QuantumFrameworkOperatorQ] /; qo["Picture"] === op["Picture"] :=
    QuantumOperator[
        qo[{"OrderedMatrixRepresentation", Max[op["InputQudits"], qo["MaxArity"]]}] . op["OrderedMatrixRepresentation"],
        QuantumBasis[op["InputBasis"], "Label" -> qo["Label"] @* op["Label"], "Output" -> qo["Output"]],
        Join[op["Order"], Complement[Range[Max[op["InputQudits"], qo["MaxArity"]]], op["Order"]]]
    ]


(* equality *)

QuantumOperator /: (qo1_QuantumOperator ? QuantumOperatorQ) == (qo2_QuantumOperator ? QuantumOperatorQ) :=
    qo1["Picture"] == qo2["Picture"] && qo1["Matrix"] == qo2["Matrix"]

