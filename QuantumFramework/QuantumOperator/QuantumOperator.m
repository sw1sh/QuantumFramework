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

(*QuantumOperator[arg_, multiplicity_Integer, args___, order : (_ ? orderQ) : {1}] :=
    QuantumOperator[QuantumTensorProduct[Table[QuantumOperator[arg, args], multiplicity]], order]*)

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
    result = Enclose @ Module[{newMatrix = matrix, outputs, inputs,
        basis, newOutputQuditBasis, newInputQuditBasis, state},
        {outputs, inputs} = Dimensions[newMatrix];
        basis = QuantumBasis[args];
        If[basis["InputDimension"] == 1,
            basis = QuantumBasis[basis, "Input" -> basis["Output"]]
        ];

        newOutputQuditBasis = QuantumTensorProduct[basis["Output"], QuditBasis[Ceiling[outputs / basis["OutputDimension"]] /. 1 -> Sequence[]]];
        newInputQuditBasis = QuantumTensorProduct[basis["Input"], QuditBasis[Ceiling[inputs / basis["InputDimension"]] /. 1 -> Sequence[]]];

        newMatrix = KroneckerProduct[
            newMatrix,
            IdentityMatrix[Max[Ceiling[outputs / newOutputQuditBasis["Dimension"]], Ceiling[inputs / newInputQuditBasis["Dimension"]]]][[
                ;; Ceiling[outputs / newOutputQuditBasis["Dimension"]], ;; Ceiling[inputs / newInputQuditBasis["Dimension"]]
            ]]
        ];
        basis = QuantumBasis[basis, "Output" -> newOutputQuditBasis, "Input" -> newInputQuditBasis["Dual"]];
        state = ConfirmBy[
            QuantumState[
                Flatten[newMatrix],
                basis
            ],
            QuantumStateQ,
            Message[QuantumOperator::invalidState]
        ];
        QuantumOperator[state, order]
    ];
    result /; !FailureQ[Unevaluted @ result]
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

                If[qb["Dimension"] === qo["Dimension"], qb, QuantumBasis[qb, "Input" -> qb["Output"]["Dual"]]]
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

(qo_QuantumOperator ? QuantumOperatorQ)[op_ ? QuantumFrameworkOperatorQ] /; qo["Picture"] === op["Picture"] := Module[{
    arity = Max[qo["MaxArity"], op["MaxArity"]], ordered1, ordered2
},
    ordered1 = op[{"Ordered", arity}];
    ordered2 = qo[{"Ordered", arity}];
    QuantumOperator[
        ordered2["Matrix"] . ordered1["Matrix"],
        QuantumBasis[
            ordered1["InputBasis"],
            "Output" -> ordered2["Output"],
            "Label" -> qo["Label"] @* op["Label"]
        ],
        ordered1["Order"]
    ]
]


(* equality *)

QuantumOperator /: (qo1_QuantumOperator ? QuantumOperatorQ) == (qo2_QuantumOperator ? QuantumOperatorQ) :=
    qo1["Picture"] == qo2["Picture"] && qo1["OrderedMatrix"] == qo2["OrderedMatrix"]

