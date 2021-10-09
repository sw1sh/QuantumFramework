Package["Wolfram`QuantumFramework`"]

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
    basis,
    inputDimension, outputDimension
},
    rank = Max[Length[dimensions], Max[order] - Min[order] + 1];
    {outputDimension, inputDimension} = Times @@@ TakeDrop[TensorDimensions[tensor], rank - Length[order]];
    basis = QuantumBasis[args];
    If[ basis["OutputDimension"] != outputDimension,
        basis = QuantumBasis[basis, "Output" -> QuditBasis[dimensions[[;; - Length[order] - 1]]]]
    ];
    If[ basis["InputDimension"] != inputDimension,
        basis = QuantumBasis[basis, "Input" -> QuditBasis[dimensions[[- Length[order] ;;]]]]
    ];
    QuantumOperator[
        ArrayReshape[tensor, Times @@@ TakeDrop[TensorDimensions[tensor], rank - Length[order]]],
        basis,
        order
    ]
]

QuantumOperator[assoc_Association, args___, order : (_ ? orderQ) : {1}] := Enclose @ Module[{
    quditBasis,
    basis,
    tensorDimensions
},
    quditBasis = QuditBasis[
        QuditBasisName /@ Keys[assoc],
        Association @ Catenate @ MapIndexed[
            With[{counts = #1, i = First[#2]}, MapIndexed[{#1, i} -> UnitVector[Max[counts], First[#2]] &, Keys @ counts]] &,
            Counts /@ Transpose[ConfirmBy[Keys[assoc], MatrixQ]]
        ],
        args
    ];
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
        If[ basis["InputDimension"] == 1,
            basis = QuantumBasis[basis, "Input" -> basis["Output"]["Dual"]]
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

QuantumOperator[qo_ ? QuantumOperatorQ, args : Except[_ ? QuantumBasisQ], order_ ? orderQ] := Enclose @
    QuantumOperator[qo, ConfirmBy[QuantumBasis[qo["Basis"], args], QuantumBasisQ], order]

QuantumOperator[qo_ ? QuantumOperatorQ, args : Except[_ ? QuantumBasisQ]] := Enclose @
    QuantumOperator[qo, ConfirmBy[QuantumBasis[qo["Basis"], args], QuantumBasisQ]]


(* change of basis *)

QuantumOperator[qo_ ? QuantumOperatorQ] := qo["Computational"]

QuantumOperator[qo_ ? QuantumOperatorQ, qb_ ? QuantumBasisQ] := QuantumOperator[qo, qb, qo["Order"]]

QuantumOperator[qo_ ? QuantumOperatorQ, qb_ ? QuantumBasisQ, order_ ? orderQ] := Enclose @ Module[{
    newBasis
},
    newBasis = If[
        qb["InputDimension"] == 1 && qo["InputDimension"] > 1,
        QuantumBasis[qb, "Input" -> qb["Output"]["Dual"]],
        qb
    ];

    newBasis = QuantumBasis[qb,
        "Output" -> QuditBasis[qo["Output"]["Reverse"], newBasis["Output"]["Reverse"]]["Reverse"],
        "Input" -> QuditBasis[qo["Input"]["Reverse"], newBasis["Input"]["Reverse"]]["Reverse"]
    ];

    ConfirmAssert[qo["Dimension"] == newBasis["Dimension"], "Basis dimensions are inconsistent"];
    QuantumOperator[
        ConfirmBy[
            QuantumState[
                qo["State"]["Computational"],
                newBasis
            ],
            QuantumStateQ,
            Message[QuantumOperator::invalidState]
        ],
        order
    ]
]


(* composition *)

QuantumOperator::incompatiblePictures = "Pictures `` and `` are incompatible with this operation"

(qo_QuantumOperator ? QuantumOperatorQ)[qs_ ? QuantumStateQ] /;
qo["Picture"] === qo["Picture"] && (
    qs["Picture"] =!= "Heisenberg" || Message[QuantumOperator::incompatiblePictures, qo["Picture"], qs["Picture"]]) := Enclose @ With[{
    ordered = qo[{"Ordered", qo["FirstQudit"], qo["FirstQudit"] + qs["OutputQudits"] - 1, qs["Output"]}]
},
    ConfirmAssert[ordered["InputDimension"] == qs["OutputDimension"], "Operator input dimension should be equal to state output dimension"];
    QuantumState[
        QuantumState[
            If[ qs["StateType"] === "Vector",
                Flatten[ordered["MatrixRepresentation"] . qs["PureMatrix"]],
                ordered["MatrixRepresentation"] . qs["MatrixRepresentation"] . ConjugateTranspose[ordered["MatrixRepresentation"]]
            ],
            QuantumBasis["Output" -> QuditBasis[ordered["OutputDimensions"]], "Input" -> QuditBasis[qs["InputDimensions"]]]
        ],
        QuantumBasis[
            "Output" -> ordered["Output"],
            "Input" -> qs["Input"],
            "Label" -> ordered["Label"] @* qs["Label"] /. None -> Sequence[]
        ]
    ]
]

(qo_QuantumOperator ? QuantumOperatorQ)[op_ ? QuantumFrameworkOperatorQ] /; qo["Picture"] === op["Picture"] := Enclose @ Module[{
    top, bottom,
    outputOrder, inputOrder,
    outputBasis, inputBasis,
    basis
},
    top = qo;
    bottom = op;

    outputOrder = Union[top["OutputOrder"], Complement[bottom["OutputOrder"], top["InputOrder"]]];
    inputOrder = Union[bottom["InputOrder"], Complement[top["InputOrder"], bottom["OutputOrder"]]];
    outputBasis = QuantumTensorProduct @@ ReplaceAll[outputOrder,
        Join[Thread[top["OutputOrder"] -> top["Output"]["Decompose"]], Thread[bottom["OutputOrder"] -> bottom["Output"]["Decompose"]]]
    ];
    inputBasis = QuantumTensorProduct @@ ReplaceAll[inputOrder,
        Join[Thread[bottom["InputOrder"] -> bottom["Input"]["Decompose"]], Thread[top["InputOrder"] -> top["Input"]["Decompose"]]]
    ];
    basis = QuantumBasis[
        outputBasis,
        inputBasis,
        "Label" -> qo["Label"] @* op["Label"] /. None -> Sequence[]
    ];

    bottom = bottom[{"Ordered", inputOrder, inputBasis}];
    top = top[{"Ordered", bottom["OutputOrder"], bottom["Output"]}];
    ConfirmAssert[top["InputDimension"] == bottom["OutputDimension"], "Applied operator input dimension should be equal to argument operator output dimension"];
    QuantumOperator[
        QuantumOperator[
            top["MatrixRepresentation"] . bottom["MatrixRepresentation"],
            QuantumBasis[basis["OutputDimensions"], basis["InputDimensions"]]
        ],
        basis,
        bottom["Order"]
    ]
]


(* equality *)

QuantumOperator /: (qo1_QuantumOperator ? QuantumOperatorQ) == (qo2_QuantumOperator ? QuantumOperatorQ) :=
    qo1["Picture"] == qo2["Picture"] && qo1["OrderedMatrixRepresentation"] == qo2["OrderedMatrixRepresentation"]

