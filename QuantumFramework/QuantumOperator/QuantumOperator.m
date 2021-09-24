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
        QuantumBasis[args, "Input" -> QuditBasis[dimensions[[- Length[order] ;;]]], "Output" -> QuditBasis[dimensions[[;; - Length[order] - 1]]]],
        order
    ]
]

QuantumOperator[assoc_Association, args___, order : (_ ? orderQ) : {1}] := Enclose @ Module[{
    quditBasis = QuditBasis[QuditBasisName /@ Keys[assoc], IdentityMatrix[Length[assoc]], args],
    basis,
    tensorDimensions
},
    ConfirmAssert[Length[assoc] > 0 && Equal @@ TensorDimensions /@ assoc];
    ConfirmAssert[EvenQ @ quditBasis["Qudits"]];
    basis = QuantumBasis[
        "Output" -> QuantumPartialTrace[quditBasis, Range[quditBasis["Qudits"] / 2 + 1, quditBasis["Qudits"]]],
        "Input" -> QuantumPartialTrace[quditBasis, Range[quditBasis["Qudits"] / 2]]["Dual"]
    ];
    tensorDimensions = TensorDimensions @ First[assoc];
    QuantumOperator[
        ArrayReshape[Values[assoc], Join[basis["Dimensions"], tensorDimensions]],
        basis,
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
        basis = ConfirmBy[QuantumBasis[args], QuantumBasisQ, "Invalid basis"];
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
        basis = QuantumBasis[basis,
            "Output" -> newOutputQuditBasis,
            "Input" -> If[newInputQuditBasis["DualQ"], newInputQuditBasis, newInputQuditBasis["Dual"]]
        ];
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

QuantumOperator[qo_ ? QuantumOperatorQ, order_ ? orderQ] :=
    QuantumOperator[qo["State"], order]

QuantumOperator[qo_ ? QuantumOperatorQ, args__, order_ ? orderQ] := Enclose @
    QuantumOperator[qo, ConfirmBy[QuantumBasis[qo["Basis"], args], QuantumBasisQ], order]

QuantumOperator[qo_ ? QuantumOperatorQ, args__] := Enclose @
    QuantumOperator[qo, ConfirmBy[QuantumBasis[qo["Basis"], args], QuantumBasisQ]]


(* change of basis *)

QuantumOperator[qo_ ? QuantumOperatorQ] := qo["Computational"]

QuantumOperator[qo_ ? QuantumOperatorQ, qb_ ? QuantumBasisQ] := Enclose @ Module[{},
    ConfirmAssert[qo["Dimension"] == qb["Dimension"] || qo["Dimension"] == qb["Dimension"] ^ 2 && qb["InputDimension"] == 1];
    QuantumOperator[
        ConfirmBy[
            QuantumState[
                qo["State"]["Computational"],
                If[qo["Dimension"] == qb["Dimension"], qb, QuantumBasis[qb, "Input" -> qb["Output"]["Dual"]]]
            ],
            QuantumStateQ,
            Message[QuantumOperator::invalidState]
        ],
        qo["Order"]
    ]
]


(* composition *)

QuantumOperator::incompatiblePictures = "Pictures `` and `` are incompatible with this operation"

(qo_QuantumOperator ? QuantumOperatorQ)[qs_ ? QuantumStateQ] /;
qo["Picture"] === qo["Picture"] && (
    qs["Picture"] =!= "Heisenberg" || Message[QuantumOperator::incompatiblePictures, qo["Picture"], qs["Picture"]]) := Enclose @ With[{
    matrix = qo[{"OrderedMatrixRepresentation", qs["OutputQudits"]}]
},
    ConfirmAssert[Dimensions[matrix][[-1]] == qs["Dimension"], "Operator input dimension should be equal state output dimension"];
    QuantumState[
        QuantumState[
            If[ qs["StateType"] === "Vector",
                matrix . qs["VectorRepresentation"],
                matrix . qs["MatrixReprsentation"] . ConjugateTranspose[matrix]
            ],
            QuantumBasis[qs["Dimensions"]]
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
        QuantumOperator[
            ordered2["MatrixRepresentation"] . ordered1["MatrixRepresentation"],
            QuantumBasis[<|"Output" -> QuditBasis[ordered2["OutputDimensions"]], "Input" -> QuditBasis[ordered1["InputDimensions"]]|>],
            Union[ordered1["TotalOrder"], ordered2["TotalOrder"]]
        ],
        QuantumBasis[
            "Output" -> ordered2["Output"],
            "Input" -> ordered1["Input"],
            "Label" -> qo["Label"] @* op["Label"]
        ]
    ]
]


(* equality *)

QuantumOperator /: (qo1_QuantumOperator ? QuantumOperatorQ) == (qo2_QuantumOperator ? QuantumOperatorQ) :=
    qo1["Picture"] == qo2["Picture"] && qo1["OrderedMatrixRepresentation"] == qo2["OrderedMatrixRepresentation"]

