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
    QuantumOperator[arg, {order, Automatic}]


QuantumOperator[qs_ ? QuantumStateQ, {Automatic, order_ ? orderQ}, opts___] :=
    QuantumOperator[
        qs,
        {
            Reverse @ Take[Join[Reverse @ order, Min[order] - Range[Max[1, qs["OutputQudits"] - qs["InputQudits"]]]], UpTo @ qs["OutputQudits"]],
            order
        },
        opts
    ]

QuantumOperator[qs_ ? QuantumStateQ, {order_ ? orderQ, Automatic}, opts___] :=
    QuantumOperator[
        qs,
        {
            order,
            Reverse @ Take[Join[Reverse @ order, Min[order] - Range[Max[1, qs["InputQudits"] - qs["OutputQudits"]]]], UpTo @ qs["InputQudits"]]
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
    newInputOrder = Join[newInputOrder, Take[DeleteElements[op["FullInputOrder"], newInputOrder], UpTo[op["FullInputQudits"] - Length[newInputOrder]]]];
    newOutputOrder = newOutputOrder - Min[newOutputOrder] + Min[newInputOrder];
    If[ op["FullInputQudits"] < Length[newInputOrder],
        QuantumOperator[{op, Ceiling[Length[newInputOrder], op["FullInputQudits"]] / op["FullInputQudits"]}, {Automatic, newInputOrder}, opts],
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
                With[{outs = Ceiling[newOutputQuditBasis["Dimension"] / outputs], ins = Ceiling[newInputQuditBasis["Dimension"] / inputs]},
                    Replace[{} -> {{1}}] @ identityMatrix[Max[outs, ins]][[;; outs, ;; ins]]
                ]
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
        QuantumOperator[state, {Range[basis["FullOutputQudits"]], Range[basis["FullInputQudits"]]}, opts]
    ];
    result /; !FailureQ[Unevaluated @ result]
]

QuantumOperator[array_ ? NumericArrayQ, args___] := QuantumOperator[Normal @ array, args]


QuantumOperator[n_Integer, args___] := QuantumOperator[{"PhaseShift", n}, args]


QuantumOperator[arg_, order1 : _ ? orderQ -> order2 : _ ? orderQ, opts___] :=
    QuantumOperator[arg, {order2, order1}, opts]


(* Mutation *)

QuantumOperator[qo_ ? QuantumOperatorQ, order : (_ ? orderQ | Automatic)] :=
    QuantumOperator[qo, {order, order}]

QuantumOperator[qo_ ? QuantumOperatorQ, outputOrder : (_ ? orderQ | Automatic), inputOrder : (_ ? orderQ | Automatic)] :=
    QuantumOperator[qo, {outputOrder, inputOrder}]

QuantumOperator[qo_ ? QuantumOperatorQ, order : {order1 : _ ? orderQ | Automatic, order2 : _ ? orderQ | Automatic}, opts___] := Block[{
    outputOrder = Replace[order1, Automatic -> qo["OutputOrder"]],
    inputOrder = Replace[order2, Automatic -> qo["InputOrder"]],
    outputQudits = qo["FullOutputQudits"],
    inputQudits = qo["FullInputQudits"]
},
    QuantumOperator[
        {qo, LCM[Quotient[Length[outputOrder], outputQudits], Quotient[Length[inputOrder], inputQudits]]},
        {outputOrder, inputOrder},
        opts
     ] /;
        Length[outputOrder] > outputQudits && Divisible[Length[outputOrder], outputQudits] &&
        Length[inputOrder] > inputQudits && Divisible[Length[inputOrder], inputQudits]
]


QuantumOperator[qo_ ? QuantumOperatorQ, order : {_ ? orderQ | Automatic, _ ? orderQ | Automatic}] := With[{
    inputRepl =
        Thread[Take[Join[qo["ControlOrder"], qo["TargetOrder"]], UpTo[Length[order[[2]]]]] -> Take[Replace[order[[2]], Automatic -> qo["InputOrder"]], UpTo[Length[qo["InputOrder"]]]]],
    outputRepl =
        Thread[Take[qo["OutputOrder"], UpTo[Length[order[[1]]]]] -> Take[Replace[order[[1]], Automatic -> qo["OutputOrder"]], UpTo[Length[qo["OutputOrder"]]]]]
},
    QuantumOperator[
        qo["State"],
        {qo["OutputOrder"] /. outputRepl, qo["InputOrder"] /. inputRepl},
        "Label" -> Replace[qo["Label"],
            Subscript["C", name_][c1_, c0_] :> Subscript["C", name][c1 /. inputRepl, c0 /. inputRepl]
        ]
    ]
]

QuantumOperator[qo_ ? QuantumOperatorQ, order1 : _ ? orderQ | Automatic -> order2 : _ ? orderQ | Automatic, opts___] :=
    QuantumOperator[qo, {order2, order1}, opts]

QuantumOperator[qo_ ? QuantumOperatorQ, opts : PatternSequence[Except[_ ? QuantumBasisQ], ___],
    outputOrder : (_ ? orderQ | Automatic), inputOrder : (_ ? orderQ | Automatic)] := Enclose @
    QuantumOperator[qo, {outputOrder, inputOrder}, ConfirmBy[QuantumBasis[qo["Basis"], opts], QuantumBasisQ]]

QuantumOperator[qo_ ? QuantumOperatorQ, order : (_ ? orderQ | Automatic), opts : PatternSequence[Except[_ ? QuantumBasisQ], ___]] := Enclose @
    QuantumOperator[qo, {order, order}, ConfirmBy[QuantumBasis[qo["Basis"], opts], QuantumBasisQ]]

QuantumOperator[qo_ ? QuantumOperatorQ, order : _ ? autoOrderQ, opts : PatternSequence[Except[_ ? QuantumBasisQ], ___]] := Enclose @
    QuantumOperator[qo, order, ConfirmBy[QuantumBasis[qo["Basis"], opts], QuantumBasisQ]]

QuantumOperator[qo_ ? QuantumOperatorQ, opts : PatternSequence[Except[_ ? autoOrderQ | _ ? QuantumBasisQ | _ ? QuantumOperatorQ], ___]] := Enclose @
    QuantumOperator[qo, qo["Order"], ConfirmBy[QuantumBasis[qo["Basis"], opts], QuantumBasisQ]]

QuantumOperator[{qo : _ ? QuantumOperatorQ, multiplicity_Integer ? Positive}] := QuantumOperator[{qo, multiplicity}, Range[multiplicity]]

QuantumOperator[{qo : _ ? QuantumOperatorQ, multiplicity_Integer ? Positive}, opts__] :=
    QuantumOperator[QuantumTensorProduct @ Table[qo, multiplicity], opts]


(* change of basis *)

QuantumOperator[qo_ ? QuantumOperatorQ, name_ ? nameQ, opts___] := QuantumOperator[qo, QuantumBasis[name], opts]

QuantumOperator[qo_ ? QuantumOperatorQ, qb_ ? QuantumBasisQ, opts___] := QuantumOperator[qo, qo["Order"], qb, opts]

QuantumOperator[qo_ ? QuantumOperatorQ, order : _ ? autoOrderQ, qb_ ? QuantumBasisQ, opts___] :=
Enclose @ Module[{
    newBasis
},
    newBasis = If[
        qb["InputDimension"] == 1 && qo["InputDimension"] > 1,
        QuantumBasis[qb, "Input" -> qb["Output"]["Dual"]],
        QuantumBasis[qb]
    ];

    newBasis = QuantumBasis[qb,
        "Output" -> QuditBasis[qo["Output"]["Reverse"], newBasis["Output"]["Reverse"]]["Reverse"],
        "Input" -> QuditBasis[qo["Input"]["Reverse"], newBasis["Input"]["Reverse"]]["Reverse"],
        opts
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
Enclose @ With[{
    order = Range[Max[1, qs["OutputQudits"]]] + Max[0, Max[qo["FullInputOrder"]] - qs["OutputQudits"]]
},
    With[{
        op = ConfirmBy[qo[QuantumOperator[qs, order, Automatic]], QuantumOperatorQ]["Sort"]
    },
        Which[
            Max[1, op["OutputQudits"]] < Length[op["OutputOrder"]],
            QuantumTensorProduct[op["State"], QuantumState[{"UniformSuperposition", Length[op["OutputOrder"]] - op["OutputQudits"]}]],
            op["OutputQudits"] > Length[op["OutputOrder"]],
            With[{order = Complement[Range[op["OutputQudits"]], op["OutputOrder"]]},
                QuantumOperator["Discard", order, op["OutputDimensions"][[order]]] @ op["State"]
            ],
            True,
            op["State"]
        ]
    ]
]

(qo_QuantumOperator ? QuantumOperatorQ)[op_ ? QuantumOperatorQ] /; qo["Picture"] === op["Picture"] &&
    ! IntersectingQ[qo["InputOrder"], op["OutputOrder"]] &&
    ! IntersectingQ[qo["OutputOrder"], op["OutputOrder"]] && ! IntersectingQ[qo["InputOrder"], op["InputOrder"]] :=
    QuantumTensorProduct[qo, op]

(qo_QuantumOperator ? QuantumOperatorQ)[op_ ? QuantumOperatorQ] /; qo["Picture"] === op["Picture"] := Enclose @ Block[{
    top, bottom
},
    top = qo;
    bottom = op;
    ConfirmAssert[ContainsNone[top["OutputOrder"], Complement[bottom["OutputOrder"], top["InputOrder"]]], "Ambiguous output orders for operator composition"];
    ConfirmAssert[ContainsNone[bottom["InputOrder"], Complement[top["InputOrder"], bottom["OutputOrder"]]], "Ambiguous input orders for operator composition"];

    If[ bottom["OutputOrder"] != top["InputOrder"],
        Module[{
            (* order = Union[bottom["OutputOrder"], top["InputOrder"]], *)
            order = Join[bottom["FullOutputOrder"], DeleteCases[top["FullInputOrder"], Alternatives @@ bottom["FullOutputOrder"]]],
            basis
        },
            basis = If[Length[order] > 0,
                QuantumTensorProduct @@ ReplaceAll[order,
                    Join[Thread[bottom["FullOutputOrder"] -> bottom["Output"]["Decompose"]], Thread[top["FullInputOrder"] -> top["Input"]["Dual"]["Decompose"]]]
                ],
                QuditBasis[]
            ];
            If[ bottom["FullOutputOrder"] != order,
                bottom = bottom["OrderedOutput", order, basis]
            ];
            top = top["OrderedInput", order, basis];
        ]
    ];
    ConfirmAssert[top["InputDimension"] == bottom["OutputDimension"], "Applied operator input dimension should be equal to argument operator output dimension"];
    QuantumOperator[ConfirmBy[top["State"] @ bottom["State"], QuantumStateQ], {top["OutputOrder"], bottom["InputOrder"]}]
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

matrixFunction[f_, mat_] := Enclose[ConfirmBy[MatrixFunction[f, mat, Method -> "Jordan"], MatrixQ], MatrixFunction[f, mat] &]

QuantumOperator /: f_Symbol[left : Except[_QuantumOperator] ..., qo_QuantumOperator, right : Except[_QuantumOperator] ...] /; MemberQ[Attributes[f], NumericFunction] := With[{op = qo["Sort"]},
    Enclose @ QuantumOperator[
        ConfirmBy[
            If[MemberQ[{Minus, Times}, f], f[left, #, right] &, matrixFunction[f[left, #, right] &, #] &] @ op["Matrix"],
            MatrixQ
        ],
        op["Order"], op["Basis"], "Label" -> f[left, op["Label"], right]
    ]
]


QuantumOperator /: Plus[ops : _QuantumOperator...] := Fold[addQuantumOperators, {ops}]

addQuantumOperators[qo1_QuantumOperator ? QuantumOperatorQ, qo2_QuantumOperator ? QuantumOperatorQ] := Enclose @ Module[{
    orderInput, orderOutput,
    ordered1, ordered2
},
    orderInput = Sequence @@ With[{
            order = Union[qo1["InputOrder"], qo2["InputOrder"]],
            qbMap = Association[Thread[qo1["InputOrder"] -> qo1["Input"]["Decompose"]], Thread[qo2["InputOrder"] -> qo2["Input"]["Decompose"]]]
        },
            {"OrderedInput", order, QuantumTensorProduct[order /. qbMap]}
    ];
    orderOutput = Sequence @@ With[{
            order = Union[qo1["OutputOrder"], qo2["OutputOrder"]],
            qbMap = Association[Thread[qo1["OutputOrder"] -> qo1["Output"]["Decompose"]], Thread[qo2["OutputOrder"] -> qo2["Output"]["Decompose"]]]
        },
            {"OrderedOutput", order, QuantumTensorProduct[order /. qbMap]}
    ];
    ordered1 = qo1[orderInput]["Sort"][orderOutput]["Sort"];
    ordered2 = qo2[orderInput]["Sort"][orderOutput]["Sort"];
    ConfirmAssert[ordered1["Dimensions"] == ordered2["Dimensions"]];
    QuantumOperator[
        QuantumOperator[
            ordered1["MatrixRepresentation"] + ordered2["MatrixRepresentation"],
            QuantumBasis[ordered1["OutputDimensions"], ordered2["InputDimensions"]]
        ],
        ordered1["Order"],
        QuantumBasis[
            ordered1["Basis"],
            "Label" -> ordered1["Label"] + ordered2["Label"],
            "ParameterSpec" -> MergeParameterSpecs[ordered1, ordered2]
        ]
    ]
]


(* differentiation *)

QuantumOperator /: D[op : _QuantumOperator, args___] := QuantumOperator[D[op["State"], args], op["Order"]]


(* join *)

QuantumOperator[qo_ ? QuantumOperatorQ] := qo

QuantumOperator[qo__QuantumOperator ? QuantumOperatorQ] := QuantumOperator[{"Multiplexer", qo}]


(* equality *)

QuantumOperator /: Equal[qo : _QuantumOperator ... ] :=
    Equal @@ (#["Picture"] & /@ {qo}) && And @@ Thread[Equal @@ (Chop @ SetPrecisionNumeric @ SparseArrayFlatten @ #["Sort"]["MatrixRepresentation"] & /@ {qo})]


(* conversion *)

QuantumOperator[obj : _QuantumMeasurementOperator | _QuantumMeasurement | _QuantumChannel | _QuantumCircuitOperator, opts___] :=
    QuantumOperator[obj["QuantumOperator"], opts]


(* parameterization *)

(qo_QuantumOperator ? QuantumOperatorQ)[] := qo[QuantumState[{"Register", qo["InputDimensions"]}]]

(qo_QuantumOperator ? QuantumOperatorQ)[ps__] /; Length[{ps}] <= qo["ParameterArity"] :=
    QuantumOperator[qo["State"][ps], qo["Order"]]


StackQuantumOperators[ops : {_ ? QuantumOperatorQ ..}, name_ : "E"] := With[{
    basis = First[ops]["Basis"],
    order = MapAt[Prepend[#, Min[#] - 1] &, First[ops]["Order"], {1}]
},
    QuantumOperator[
        QuantumOperator[
            SparseArray[#["MatrixRepresentation"] & /@ ops],
            QuantumBasis[
                QuantumTensorProduct[QuditBasis[Subscript[name, #] & /@ Range @ Length @ ops], basis["Output"]],
                basis["Input"]
            ]
        ],
    order
    ]
]
