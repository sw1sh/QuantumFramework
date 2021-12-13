Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumOperator"]

PackageScope["QuantumOperatorQ"]



QuantumOperator::invalidInputOrder = "input order should be a list of distinct input qudit positions"
QuantumOperator::invalidOutputOrder = "output order should be a list of distinct output qudit positions"

QuantumOperatorQ[QuantumOperator[qs_, {outputOrder_, inputOrder_}]] :=
    QuantumStateQ[qs] &&
    (orderQ[outputOrder] || (Message[QuantumOperator::invalidOutputOrder]; False)) &&
    (orderQ[inputOrder] || (Message[QuantumOperator::invalidInputOrder]; False))


QuantumOperatorQ[___] := False


(* constructors *)

QuantumOperator[qs_ ? QuantumStateQ] := QuantumOperator[qs,
    If[qs["InputDimension"] > 1, {Automatic, Range[qs["InputQudits"]]}, {Range[qs["OutputQudits"]], Automatic}]
]

QuantumOperator[arg : _ ? QuantumStateQ, order : _ ? orderQ | Automatic] := QuantumOperator[arg, {Automatic, order}]

QuantumOperator[arg : _ ? QuantumStateQ, outputOrder_, inputOrder_] := QuantumOperator[arg, {outputOrder, inputOrder}]

QuantumOperator[qs_ ? QuantumStateQ, {Automatic, order_ ? orderQ}] := QuantumOperator[qs,
    {
        Reverse @ Take[Join[Reverse @ order, Min[order] - Range[qs["OutputQudits"] - qs["InputQudits"]]], UpTo @ qs["OutputQudits"]],
        order
    }
]

QuantumOperator[qs_ ? QuantumStateQ, {order_ ? orderQ, Automatic}] := QuantumOperator[qs,
    {
        order,
        Reverse @ Take[Join[Reverse @ order, Min[order] - Range[qs["InputQudits"] - qs["OutputQudits"]]], UpTo @ qs["InputQudits"]]
    }
]


QuantumOperator[tensor_ ? TensorQ /; TensorRank[tensor] > 2, args___, order : (_ ? orderQ) : {1}] := Module[{
    dimensions = TensorDimensions[tensor],
    rank,
    basis,
    inputDimension, outputDimension
},
    rank = Max[Length[dimensions], Max[order] - Min[order] + 1];
    {outputDimension, inputDimension} = Times @@@ TakeDrop[dimensions, rank - Length[order]];
    basis = QuantumBasis[args];
    If[ basis["OutputDimension"] != outputDimension,
        basis = QuantumBasis[basis, "Output" -> QuditBasis[dimensions[[;; - Length[order] - 1]]]]
    ];
    If[ basis["InputDimension"] != inputDimension,
        basis = QuantumBasis[basis, "Input" -> QuditBasis[dimensions[[- Length[order] ;;]]]]
    ];
    QuantumOperator[
        ArrayReshape[tensor, {outputDimension, inputDimension}],
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
        QuditName /@ Keys[assoc],
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

QuantumOperator[matrix_ ? MatrixQ, args___, order : {_ ? orderQ | Automatic, _ ? orderQ | Automatic}] :=
    QuantumOperator[QuantumOperator[matrix, args]["State"], order]

QuantumOperator[matrix_ ? MatrixQ, args___, order : _ ? orderQ | Automatic] := Enclose @ Module[{
    op = ConfirmBy[QuantumOperator[matrix, args], QuantumOperatorQ],
    newOrder
},
    newOrder = order /. Automatic -> op["FullInputOrder"];
    If[ op["InputQudits"] < Length[newOrder],
        QuantumOperator[{op, Ceiling[Length[newOrder], op["InputQudits"]] / op["InputQudits"]}, {newOrder, newOrder}],
        QuantumOperator[op["State"], {Automatic, newOrder}]
    ]
]

QuantumOperator[matrix_ ? MatrixQ, args : PatternSequence[] | PatternSequence[___, Except[Automatic | _ ? orderQ]]] := Module[{
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
        QuantumOperator[state]
    ];
    result /; !FailureQ[Unevaluted @ result]
]


(* Mutation *)

QuantumOperator[qo_ ? QuantumOperatorQ, order : (_ ? orderQ | Automatic)] :=
    QuantumOperator[qo["State"], {qo["OutputOrder"], order}]

QuantumOperator[qo_ ? QuantumOperatorQ, outputOrder : (_ ? orderQ | Automatic), inputOrder : (_ ? orderQ | Automatic)] :=
    QuantumOperator[qo["State"], {outputOrder, inputOrder}]

QuantumOperator[qo_ ? QuantumOperatorQ, order : {_ ? orderQ | Automatic, _ ? orderQ | Automatic}] :=
    QuantumOperator[qo["State"], order]


QuantumOperator[qo_ ? QuantumOperatorQ, args : PatternSequence[Except[_ ? QuantumBasisQ], ___],
    outputOrder : (_ ? orderQ | Automatic), inputOrder : (_ ? orderQ | Automatic)] := Enclose @
    QuantumOperator[qo, ConfirmBy[QuantumBasis[qo["Basis"], args], QuantumBasisQ], {outputOrder, inputOrder}]

QuantumOperator[qo_ ? QuantumOperatorQ, args : PatternSequence[Except[_ ? QuantumBasisQ], ___], order : (_ ? orderQ | Automatic)] := Enclose @
    QuantumOperator[qo, ConfirmBy[QuantumBasis[qo["Basis"], args], QuantumBasisQ], {qo["OutputOrder"], order}]

QuantumOperator[qo_ ? QuantumOperatorQ, args : PatternSequence[Except[_ ? QuantumBasisQ], ___]] := Enclose @
    QuantumOperator[qo, ConfirmBy[QuantumBasis[qo["Basis"], args], QuantumBasisQ]]

QuantumOperator[{qo : _ ? QuantumOperatorQ, multiplicity_Integer ? Positive}] := QuantumOperator[{qo, multiplicity}, Range[multiplicity]]

QuantumOperator[{qo : _ ? QuantumOperatorQ, multiplicity_Integer ? Positive}, args__] :=
    QuantumOperator[QuantumTensorProduct @ Table[qo, multiplicity], args]

(* change of basis *)

QuantumOperator[qo_ ? QuantumOperatorQ] := qo["Computational"]

QuantumOperator[qo_ ? QuantumOperatorQ, name_ ? nameQ, args___] := QuantumOperator[qo, QuantumBasis[name], args]


QuantumOperator[qo_ ? QuantumOperatorQ, qb_ ? QuantumBasisQ, args___,
    order : _ ? orderQ | Automatic | {_ ? orderQ | Automatic, _ ? orderQ | Automatic}] :=
Enclose @ Module[{
    newBasis
},
    newBasis = If[
        qb["InputDimension"] == 1 && qo["InputDimension"] > 1,
        QuantumBasis[qb, "Input" -> qb["Output"]["Dual"], args],
        QuantumBasis[qb, args]
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

QuantumOperator[qo_ ? QuantumOperatorQ, qb_ ? QuantumBasisQ,
    args : PatternSequence[] | PatternSequence[___, Except[_ ? orderQ | Automatic | {_ ? orderQ | Automatic, _ ? orderQ | Automatic}]]] :=
    QuantumOperator[qo, qb, args, qo["Order"]]

QuantumOperator[qo_ ? QuantumOperatorQ, qb_ ? QuantumBasisQ, args___, outputOrder_ ? orderQ | Automatic, inputOrder_ ? orderQ | Automatic] :=
    QuantumOperator[qo, qb, args, {outputOrder, inputOrder}]

(* composition *)

QuantumOperator::incompatiblePictures = "Pictures `` and `` are incompatible with this operation"

(qo_QuantumOperator ? QuantumOperatorQ)[qs_ ? QuantumStateQ] /; qo["Picture"] === qo["Picture"] && (
    qs["Picture"] =!= "Heisenberg" || Message[QuantumOperator::incompatiblePictures, qo["Picture"], qs["Picture"]]) :=
    If[ qs["PureStateQ"],
        qo[QuantumOperator[qs]]["Sort"]["State"],
        qo[QuantumOperator[qs["Pure"][{"Split", qs["Qudits"]}]] @ qo["Dagger"]]["Sort"]["Mixed"]
    ]


(qo_QuantumOperator ? QuantumOperatorQ)[op_ ? QuantumOperatorQ] /; qo["Picture"] === op["Picture"] := Enclose @ Module[{
    top, bottom
},
    top = qo;
    bottom = op;
    ConfirmAssert[ContainsNone[top["OutputOrder"], Complement[bottom["OutputOrder"], top["InputOrder"]]], "Ambiguous output orders for operator composition"];
    ConfirmAssert[ContainsNone[bottom["InputOrder"], Complement[top["InputOrder"], bottom["OutputOrder"]]], "Ambiguous input orders for operator composition"];

    If[ bottom["OutputOrder"] != top["InputOrder"],
        Module[{
            (* order = Union[bottom["OutputOrder"], top["InputOrder"]], *)
            order = Join[bottom["OutputOrder"], Complement[top["InputOrder"], bottom["OutputOrder"]]],
            basis
        },
            basis = If[Length[order] > 0,
                QuantumTensorProduct @@ ReplaceAll[order,
                    Join[Thread[bottom["OutputOrder"] -> bottom["Output"]["Decompose"]], Thread[top["InputOrder"] -> top["Input"]["Dual"]["Decompose"]]]
                ],
                QuditBasis[]
            ];
            If[ bottom["OutputOrder"] != order,
                bottom = bottom[{"OrderedOutput", order, basis}]
            ];
            top = top[{"OrderedInput", order, basis}];
        ]
    ];

    ConfirmAssert[top["InputDimension"] == bottom["OutputDimension"], "Applied operator input dimension should be equal to argument operator output dimension"];

    QuantumOperator[top["State"] @ bottom["State"], top["OutputOrder"], bottom["InputOrder"]]
]


(qo_QuantumOperator ? QuantumOperatorQ)[qmo_ ? QuantumMeasurementOperatorQ] /; qo["Picture"] == qmo["Picture"] :=
    QuantumMeasurementOperator[qo @ qmo["Operator"], qmo["Target"]]

(qo_QuantumOperator ? QuantumOperatorQ)[qm_ ? QuantumMeasurementQ] /; qo["Picture"] == qm["Picture"] :=
    QuantumMeasurement[qm["Operator"][qo]["State"], qm["Target"]]

(qo_QuantumOperator ? QuantumOperatorQ)[qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ] :=
    QuantumCircuitOperator[Append[qco["Operators"], qo]]


expandQuditBasis[qb_QuditBasis, order1_ ? orderQ, order2_ ? orderQ, defaultDim_Integer : 2] := Enclose @ (
    ConfirmAssert[Length[order1] == qb["Qudits"]];
    QuantumTensorProduct[order2 /. Append[Thread[order1 -> qb["Decompose"]], _Integer -> QuditBasis[defaultDim]]]
)

QuantumOperator /: (qo1_QuantumOperator ? QuantumOperatorQ) + (qo2_QuantumOperator ? QuantumOperatorQ) := Enclose @ With[{
    ordered1 = qo1[
        With[{order = Union[qo1["InputOrder"], qo2["InputOrder"]]}, {"OrderedInput", order, expandQuditBasis[qo1["Input"], qo1["InputOrder"], order]}]][
        With[{order = Union[qo1["OutputOrder"], qo2["OutputOrder"]]}, {"OrderedOutput", order, expandQuditBasis[qo1["Output"], qo1["OutputOrder"], order]}]
    ],
    ordered2 = qo2[
        With[{order = Union[qo1["InputOrder"], qo2["InputOrder"]]}, {"OrderedInput", order, expandQuditBasis[qo2["Input"], qo2["InputOrder"], order]}]][
        With[{order = Union[qo1["OutputOrder"], qo2["OutputOrder"]]}, {"OrderedOutput", order, expandQuditBasis[qo2["Output"], qo2["OutputOrder"], order]}]
    ]
},
    ConfirmAssert[ordered1["Dimensions"] == ordered2["Dimensions"]];
    QuantumOperator[
        QuantumOperator[
            ordered1["MatrixRepresentation"] + ordered2["MatrixRepresentation"],
            QuantumBasis[ordered1["OutputDimensions"], ordered2["InputDimensions"]]
        ],
        ordered1["Basis"],
        ordered1["Order"]
    ]
]

QuantumOperator /: f_Symbol[left : Except[_QuantumOperator] ..., qo_QuantumOperator, right : Except[_QuantumOperator] ...] /; MemberQ[Attributes[f], NumericFunction] :=
    Enclose @ QuantumOperator[ConfirmBy[MatrixFunction[f[left, #, right] &, qo["Matrix"]], MatrixQ], qo["Basis"], "Label" -> f[left, qo["Label"], right], qo["Order"]]


(* equality *)

QuantumOperator /: Equal[qo : _QuantumOperator ... ] :=
    Equal @@ (#["Picture"] & /@ {qo}) && Equal @@ (#["Sort"]["MatrixRepresentation"] & /@ {qo})


(* parameterization *)

(qo_QuantumOperator ? QuantumOperatorQ)[ps___] /; Length[{ps}] <= qo["ParameterArity"] :=
    QuantumOperator[qo["State"][ps], qo["Order"]]

