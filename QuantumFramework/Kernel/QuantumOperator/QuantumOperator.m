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

QuantumOperator[qs_ ? QuantumStateQ, order_ ? orderQ] := QuantumOperator[qs, {Automatic, order}]

QuantumOperator[qs_ ? QuantumStateQ, outputOrder_, inputOrder_] := QuantumOperator[qs, {outputOrder, inputOrder}]

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

QuantumOperator[matrix_ ? MatrixQ, args___, order : {_ ? orderQ, _ ? orderQ}] :=
    QuantumOperator[QuantumOperator[matrix, args]["State"], order]

QuantumOperator[matrix_ ? MatrixQ, args___, order_ ? orderQ] := QuantumOperator[QuantumOperator[matrix, args]["State"], {Automatic, order}]

QuantumOperator[matrix_ ? MatrixQ, args : PatternSequence[] | PatternSequence[___, Except[_ ? orderQ]]] := Module[{
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

QuantumOperator[{qo : _ ? QuantumOperatorQ, multiplicity_Integer ? Positive}, order_ ? orderQ] :=
    QuantumOperator[QuantumTensorProduct @ Table[qo, multiplicity], order]

(* change of basis *)

QuantumOperator[qo_ ? QuantumOperatorQ] := qo["Computational"]

QuantumOperator[qo_ ? QuantumOperatorQ, name_ ? nameQ, args___] := QuantumOperator[qo, QuantumBasis[name], args]


QuantumOperator[qo_ ? QuantumOperatorQ, qb_ ? QuantumBasisQ, args___, order : _ ? orderQ | {_ ? orderQ, _ ? orderQ}] := Enclose @ Module[{
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

QuantumOperator[qo_ ? QuantumOperatorQ, qb_ ? QuantumBasisQ, args : PatternSequence[] | PatternSequence[___, Except[{_ ? orderQ, _ ? orderQ}]]] :=
    QuantumOperator[qo, qb, args, qo["Order"]]

QuantumOperator[qo_ ? QuantumOperatorQ, qb_ ? QuantumBasisQ, args___, outputOrder_ ? orderQ, inputOrder_ ? orderQ] :=
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
    top, bottom,
    order,
    basis,
    state
},
    top = qo;
    bottom = op;
    ConfirmAssert[ContainsNone[top["OutputOrder"], Complement[bottom["OutputOrder"], top["InputOrder"]]], "Ambiguous output orders for operator composition"];
    ConfirmAssert[ContainsNone[bottom["InputOrder"], Complement[top["InputOrder"], bottom["OutputOrder"]]], "Ambiguous input orders for operator composition"];
    order = Union[bottom["OutputOrder"], top["InputOrder"]];

    basis = If[Length[order] > 0,
        QuantumTensorProduct @@ ReplaceAll[order,
            Join[Thread[bottom["OutputOrder"] -> bottom["Output"]["Decompose"]], Thread[top["InputOrder"] -> top["Input"]["Dual"]["Decompose"]]]
        ],
        QuditBasis[]
    ];
    bottom = bottom[{"OrderedOutput", order, basis}];
    top = top[{"OrderedInput", order, basis}];

    ConfirmAssert[top["InputDimension"] == bottom["OutputDimension"], "Applied operator input dimension should be equal to argument operator output dimension"];

    state = QuantumState[
        QuantumState[
            Flatten[top["MatrixRepresentation"] . bottom["MatrixRepresentation"]],
            QuantumBasis[top["OutputDimensions"], bottom["InputDimensions"]]
        ],
        QuantumBasis[
            top["Output"],
            bottom["Input"]["Dual"],
            "Label" -> qo["Label"] @* op["Label"] /. None -> Sequence[]
        ]
    ];

    QuantumOperator[state, top["OutputOrder"], bottom["InputOrder"]]
]


(qo_QuantumOperator ? QuantumOperatorQ)[qmo_ ? QuantumMeasurementOperatorQ] /; qo["Picture"] == qmo["Picture"] :=
    QuantumMeasurementOperator[qo @ qmo["Operator"], qmo["Target"]]

(qo_QuantumOperator ? QuantumOperatorQ)[qm_ ? QuantumMeasurementQ] /; qo["Picture"] == qm["Picture"] :=
    QuantumMeasurement[qm["Operator"][qo]["State"], qm["Target"]]


(* equality *)

QuantumOperator /: Equal[qo : _QuantumOperator ... ] :=
    Equal @@ (#["Picture"] & /@ {qo}) && Equal @@ (#["Sort"]["MatrixRepresentation"] & /@ {qo})

