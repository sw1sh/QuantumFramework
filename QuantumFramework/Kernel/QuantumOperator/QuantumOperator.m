Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumOperator"]

PackageScope["QuantumOperatorQ"]
PackageScope["StackQuantumOperators"]


QuantumOperator::invalidInputOrder = "input order should be a list of distinct input qudit positions"
QuantumOperator::invalidOutputOrder = "output order should be a list of distinct output qudit positions"

QuantumOperatorQ[QuantumOperator[qs_, {outputOrder_ ? orderQ, inputOrder_ ? orderQ}]] :=
    QuantumStateQ[qs] &&
    (orderQ[outputOrder] || (Message[QuantumOperator::invalidOutputOrder]; False)) &&
    (orderQ[inputOrder] || (Message[QuantumOperator::invalidInputOrder]; False))


QuantumOperatorQ[___] := False


(* constructors *)

SetAttributes[QuantumOperator, NHoldRest]

QuantumOperator[arg : _ ? QuantumStateQ, order : {_ ? orderQ, _ ? orderQ}, opts__] :=
    QuantumOperator[QuantumState[arg, opts], order]

QuantumOperator[qs_ ? QuantumStateQ] :=
    QuantumOperator[
        qs,
        If[qs["InputDimension"] > 1, {Automatic, Range[qs["InputQudits"]]}, {Range[qs["OutputQudits"]], Automatic}]
    ]

QuantumOperator[arg : _ ? QuantumStateQ, outputOrder : _ ? orderQ | Automatic, inputOrder : _ ? orderQ | Automatic, opts___] :=
    QuantumOperator[QuantumState[arg, opts], {outputOrder, inputOrder}]

QuantumOperator[arg : _ ? QuantumStateQ, order : _ ? orderQ | Automatic] :=
    QuantumOperator[arg, {Automatic, order}]



QuantumOperator[qs_ ? QuantumStateQ, {Automatic, order_ ? orderQ}, opts___] :=
    QuantumOperator[
        qs,
        {
            Reverse @ Take[Join[Reverse @ order, Min[order] - Range[qs["OutputQudits"] - qs["InputQudits"]]], UpTo @ qs["OutputQudits"]],
            order
        },
        opts
    ]

QuantumOperator[qs_ ? QuantumStateQ, {order_ ? orderQ, Automatic}, opts___] :=
    QuantumOperator[
        qs,
        {
            order,
            Reverse @ Take[Join[Reverse @ order, Min[order] - Range[qs["InputQudits"] - qs["OutputQudits"]]], UpTo @ qs["InputQudits"]]
        },
        opts
    ]


QuantumOperator[tensor_ ? TensorQ /; TensorRank[tensor] > 2, order : (_ ? autoOrderQ) : {1}, args___, opts : OptionsPattern[]] := Module[{
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
        basis = QuantumBasis[basis, "Input" -> QuditBasis[dimensions[[- Length[order] ;;]]]["Dual"]]
    ];
    QuantumOperator[
        ArrayReshape[tensor, {outputDimension, inputDimension}],
        order,
        basis,
        opts
    ]
]

QuantumOperator[assoc_Association, order : (_ ? orderQ) : {1}, args___, opts : OptionsPattern[]] := Enclose @ Module[{
    quditBasis,
    basis,
    tensorDimensions
},
    quditBasis = QuditBasis[
        Association @ Catenate @ MapIndexed[
            With[{counts = #1, i = First[#2]}, MapIndexed[{QuditName[#1], i} -> UnitVector[Length[counts], First[#2]] &, Keys @ counts]] &,
            Counts /@ Transpose[ConfirmBy[List @@@ Keys[assoc], MatrixQ]]
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
        ArrayReshape[Lookup[KeyMap[QuditName[List @@ #] &, assoc], quditBasis["Names"], 0], Join[basis["Dimensions"], tensorDimensions]],
        Join[Complement[Range[Length[tensorDimensions]], order], order],
        basis,
        opts
    ]
]


QuantumOperator::invalidState = "invalid state specification";


QuantumOperator[matrix_ ? MatrixQ, order : _ ? autoOrderQ, args___, opts : OptionsPattern[]] := Enclose @ Module[{
    op = ConfirmBy[QuantumOperator[matrix, args], QuantumOperatorQ],
    newOutputOrder, newInputOrder
},
    {newOutputOrder, newInputOrder} = Replace[order, {
        o : Except[{_ ? orderQ | Automatic, _ ? orderQ | Automatic}] :> {op["OutputOrder"], Replace[o, Automatic -> op["InputOrder"]]},
        Automatic -> op["Order"]
    }];
    newInputOrder = PadRight[newInputOrder, op["InputQudits"], Drop[op["FullInputOrder"], UpTo @ Length[newInputOrder]]];
    newOutputOrder = newOutputOrder - Min[newOutputOrder] + Min[newInputOrder];
    If[ op["InputQudits"] < Length[newInputOrder],
        QuantumOperator[{op, Ceiling[Length[newInputOrder], op["InputQudits"]] / op["InputQudits"]}, {Automatic, newInputOrder}, opts],
        QuantumOperator[op["State"], {newOutputOrder, newInputOrder}, opts]
    ]
]

QuantumOperator[matrix_ ? MatrixQ, args___, opts : OptionsPattern[]] := Module[{
    outMultiplicity, inMultiplicity, result
},
    result = Enclose @ Module[{newMatrix = matrix, outputs, inputs,
        basis, newOutputQuditBasis, newInputQuditBasis, state},
        {outputs, inputs} = Dimensions[newMatrix];
        basis = ConfirmBy[QuantumBasis[args], QuantumBasisQ, "Invalid basis"];
        If[ basis["InputDimension"] == 1,
            basis = QuantumBasis[basis, "Input" -> basis["Output"]["Dual"]]
        ];
        outMultiplicity = Quiet @ Log[basis["OutputDimension"], outputs];
        inMultiplicity = Quiet @ Log[basis["InputDimension"], inputs];
        If[
            IntegerQ[outMultiplicity] && IntegerQ[inMultiplicity],
            (* multiply existing basis *)

            basis = QuantumBasis[basis,
                "Output" -> QuditBasis[basis["Output"], outMultiplicity],
                "Input" -> QuditBasis[basis["Input"], inMultiplicity]
            ],

            (* add one extra qudit *)
            newOutputQuditBasis = QuantumTensorProduct[basis["Output"], QuditBasis[Ceiling[outputs / basis["OutputDimension"]] /. 1 -> Sequence[]]];
            newInputQuditBasis = QuantumTensorProduct[basis["Input"], QuditBasis[Ceiling[inputs / basis["InputDimension"]] /. 1 -> Sequence[]]];

            newMatrix = kroneckerProduct[
                newMatrix,
                Replace[{} -> {{1}}] @ identityMatrix[Max[newOutputQuditBasis["Dimension"] - outputs, newInputQuditBasis["Dimension"] - inputs, 0]][[
                    ;; Max[newOutputQuditBasis["Dimension"] - outputs, 0], ;; Max[newInputQuditBasis["Dimension"] - inputs, 0]
                ]]
            ];
            basis = QuantumBasis[basis,
                "Output" -> newOutputQuditBasis,
                "Input" -> If[newInputQuditBasis["DualQ"], newInputQuditBasis, newInputQuditBasis["Dual"]]
            ];
        ];
        state = ConfirmBy[
            QuantumState[
                Flatten[newMatrix],
                basis
            ],
            QuantumStateQ,
            Message[QuantumOperator::invalidState]
        ];
        QuantumOperator[state, {Range[basis["OutputQudits"]], Range[basis["InputQudits"]]}, opts]
    ];
    result /; !FailureQ[Unevaluated @ result]
]

QuantumOperator[array_ ? NumericArrayQ, args___] := QuantumOperator[Normal @ array, args]


(* Mutation *)

QuantumOperator[qo_ ? QuantumOperatorQ, order : (_ ? orderQ | Automatic)] :=
    QuantumOperator[qo["State"], {order, order}]

QuantumOperator[qo_ ? QuantumOperatorQ, outputOrder : (_ ? orderQ | Automatic), inputOrder : (_ ? orderQ | Automatic)] :=
    QuantumOperator[qo["State"], {outputOrder, inputOrder}]

QuantumOperator[qo_ ? QuantumOperatorQ, order : {_ ? orderQ | Automatic, _ ? orderQ | Automatic}] :=
    QuantumOperator[qo["State"], order]


QuantumOperator[qo_ ? QuantumOperatorQ, opts : PatternSequence[Except[_ ? QuantumBasisQ], ___],
    outputOrder : (_ ? orderQ | Automatic), inputOrder : (_ ? orderQ | Automatic)] := Enclose @
    QuantumOperator[qo, {outputOrder, inputOrder}, ConfirmBy[QuantumBasis[qo["Basis"], opts], QuantumBasisQ]]

QuantumOperator[qo_ ? QuantumOperatorQ, order : (_ ? orderQ | Automatic), opts : PatternSequence[Except[_ ? QuantumBasisQ], ___]] := Enclose @
    QuantumOperator[qo, {order, order}, ConfirmBy[QuantumBasis[qo["Basis"], opts], QuantumBasisQ]]

QuantumOperator[qo_ ? QuantumOperatorQ, order : _ ? autoOrderQ, opts : PatternSequence[Except[_ ? QuantumBasisQ], ___]] := Enclose @
    QuantumOperator[qo, order, ConfirmBy[QuantumBasis[qo["Basis"], opts], QuantumBasisQ]]

QuantumOperator[qo_ ? QuantumOperatorQ, opts : PatternSequence[Except[_ ? autoOrderQ | _ ? QuantumBasisQ], ___]] := Enclose @
    QuantumOperator[qo, qo["Order"], ConfirmBy[QuantumBasis[qo["Basis"], opts], QuantumBasisQ]]

QuantumOperator[{qo : _ ? QuantumOperatorQ, multiplicity_Integer ? Positive}] := QuantumOperator[{qo, multiplicity}, Range[multiplicity]]

QuantumOperator[{qo : _ ? QuantumOperatorQ, multiplicity_Integer ? Positive}, opts__] :=
    QuantumOperator[QuantumTensorProduct @ Table[qo, multiplicity], opts]

(* change of basis *)

QuantumOperator[qo_ ? QuantumOperatorQ] := qo["Computational"]

QuantumOperator[qo_ ? QuantumOperatorQ, name_ ? nameQ, opts___] := QuantumOperator[qo, QuantumBasis[name], opts]

QuantumOperator[qo_ ? QuantumOperatorQ, qb_ ? QuantumBasisQ] := QuantumOperator[qo, qo["Order"], qb]

QuantumOperator[qo_ ? QuantumOperatorQ, order : _ ? autoOrderQ, qb_ ? QuantumBasisQ, opts___] :=
Enclose @ Module[{
    newBasis
},
    newBasis = If[
        qb["InputDimension"] == 1 && qo["InputDimension"] > 1,
        QuantumBasis[qb, "Input" -> qb["Output"]["Dual"], opts],
        QuantumBasis[qb, opts]
    ];

    newBasis = QuantumBasis[qb,
        "Output" -> QuditBasis[qo["Output"]["Reverse"], newBasis["Output"]["Reverse"]]["Reverse"],
        "Input" -> QuditBasis[qo["Input"]["Reverse"], newBasis["Input"]["Reverse"]]["Reverse"]
    ];

    ConfirmAssert[qo["Dimension"] == newBasis["Dimension"], "Basis dimensions are inconsistent"];
    QuantumOperator[
        ConfirmBy[
            QuantumState[
                qo["State"],
                newBasis
            ],
            QuantumStateQ,
            Message[QuantumOperator::invalidState]
        ],
        order
    ]
]

QuantumOperator[qo_ ? QuantumOperatorQ,
    outputOrder : _ ? orderQ | Automatic : Automatic, inputOrder : _ ? orderQ | Automatic : Automatic, qb_ ? QuantumBasisQ, opts___] :=
    QuantumOperator[qo, {outputOrder, inputOrder}, qb, opts]

(* composition *)

QuantumOperator::incompatiblePictures = "Pictures `` and `` are incompatible with this operation"

(qo_QuantumOperator ? QuantumOperatorQ)[qs_ ? QuantumStateQ] /; qo["Picture"] === qo["Picture"] && (
    qs["Picture"] =!= "Heisenberg" || Message[QuantumOperator::incompatiblePictures, qo["Picture"], qs["Picture"]]) :=
With[{order = Range[qs["OutputQudits"]] + Max[Max[qo["FullInputOrder"]] - qs["OutputQudits"], 0]},
    qo[QuantumOperator[qs, order]]["Sort"]["State"]
]

(qo_QuantumOperator ? QuantumOperatorQ)[op_ ? QuantumOperatorQ] /; qo["Picture"] === op["Picture"] &&
    ! IntersectingQ[qo["InputOrder"], op["OutputOrder"]] &&
    ! IntersectingQ[qo["OutputOrder"], op["OutputOrder"]] && ! IntersectingQ[qo["InputOrder"], op["InputOrder"]] :=
    QuantumTensorProduct[qo, op]

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
                bottom = bottom["OrderedOutput", order, basis]
            ];
            top = top["OrderedInput", order, basis];
        ]
    ];
    ConfirmAssert[top["InputDimension"] == bottom["OutputDimension"], "Applied operator input dimension should be equal to argument operator output dimension"];
    (* top = top["SortOutput"];
    bottom = bottom["SortInput"]; *)
    QuantumOperator[top["State"] @ bottom["State"], {top["OutputOrder"], bottom["InputOrder"]}]
]


(qo_QuantumOperator ? QuantumOperatorQ)[qmo_ ? QuantumMeasurementOperatorQ] /; qo["Picture"] == qmo["Picture"] :=
    QuantumMeasurementOperator[qo @ qmo["Operator"], qmo["Target"]]

(qo_QuantumOperator ? QuantumOperatorQ)[qm_ ? QuantumMeasurementQ] /; qo["Picture"] == qm["Picture"] :=
    QuantumMeasurement[qo[qm["QuantumOperator"]]["Sort"]]

(qo_QuantumOperator ? QuantumOperatorQ)[qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ] :=
    QuantumCircuitOperator[Append[qco["Operators"], qo]]


expandQuditBasis[qb_QuditBasis, order1_ ? orderQ, order2_ ? orderQ, defaultDim_Integer : 2] := Enclose @ (
    ConfirmAssert[Length[order1] == qb["Qudits"]];
    QuantumTensorProduct[order2 /. Append[Thread[order1 -> qb["Decompose"]], _Integer -> QuditBasis[defaultDim]]]
)


QuantumOperator /: f_Symbol[left : Except[_QuantumOperator] ..., qo_QuantumOperator, right : Except[_QuantumOperator] ...] /; MemberQ[Attributes[f], NumericFunction] :=
    Enclose @ QuantumOperator[ConfirmBy[MatrixFunction[f[left, #, right] &, qo["Sort"]["Matrix"]], MatrixQ], Sort /@ qo["Order"], qo["Basis"], "Label" -> f[left, qo["Label"], right]]


QuantumOperator /: Plus[ops : _QuantumOperator...] := Fold[addQuantumOperators, {ops}]

addQuantumOperators[qo1_QuantumOperator ? QuantumOperatorQ, qo2_QuantumOperator ? QuantumOperatorQ] := Enclose @ With[{
    ordered1 = qo1[
        Sequence @@ With[{order = Union[qo1["InputOrder"], qo2["InputOrder"]]}, {"OrderedInput", order, expandQuditBasis[qo1["Input"], qo1["InputOrder"], order]}]][
        Sequence @@ With[{order = Union[qo1["OutputOrder"], qo2["OutputOrder"]]}, {"OrderedOutput", order, expandQuditBasis[qo1["Output"], qo1["OutputOrder"], order]}]
    ]["Sort"],
    ordered2 = qo2[
        Sequence @@ With[{order = Union[qo1["InputOrder"], qo2["InputOrder"]]}, {"OrderedInput", order, expandQuditBasis[qo2["Input"], qo2["InputOrder"], order]}][
        Sequence @@ With[{order = Union[qo1["OutputOrder"], qo2["OutputOrder"]]}, {"OrderedOutput", order, expandQuditBasis[qo2["Output"], qo2["OutputOrder"], order]}]]
    ]["Sort"]
},
    ConfirmAssert[ordered1["Dimensions"] == ordered2["Dimensions"]];
    QuantumOperator[
        QuantumOperator[
            ordered1["MatrixRepresentation"] + ordered2["MatrixRepresentation"],
            QuantumBasis[ordered1["OutputDimensions"], ordered2["InputDimensions"]]
        ],
        ordered1["Order"],
        ordered1["Basis"]
    ]
]


(* equality *)

QuantumOperator /: Equal[qo : _QuantumOperator ... ] :=
    Equal @@ (#["Picture"] & /@ {qo}) && Equal @@ (#["Sort"]["MatrixRepresentation"] & /@ {qo})


(* parameterization *)

(qo_QuantumOperator ? QuantumOperatorQ)[ps___] /; Length[{ps}] <= qo["ParameterArity"] :=
    QuantumOperator[qo["State"][ps], qo["Order"]]



StackQuantumOperators[ops : {_ ? QuantumOperatorQ ..}, name_ : "E"] := With[{
    basis = First[ops]["Basis"],
    order = MapAt[Prepend[#, Min[#] - 1] &, First[ops]["Order"], {1}]
},
    QuantumOperator[
        QuantumOperator[
            QuantumOperator[#, basis]["Matrix"] & /@ ops,
            "Output" -> QuantumTensorProduct[QuditBasis[Subscript[name, #] & /@ Range @ Length @ ops], basis["Output"]],
            "Input" -> basis["Input"]
        ],
        order
    ]
]

