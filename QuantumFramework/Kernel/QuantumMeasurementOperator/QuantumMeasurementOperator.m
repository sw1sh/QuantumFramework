Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumMeasurementOperator"]

PackageScope["QuantumMeasurementOperatorQ"]



QuantumMeasurementOperatorQ[QuantumMeasurementOperator[qo_, _ ? targetQ]] := QuantumOperatorQ[qo]

QuantumMeasurementOperatorQ[___] := False


(* constructors *)

SetAttributes[QuantumMeasurementOperator, NHoldRest]

QuantumMeasurementOperator[arg_ -> eigenvalues_ ? VectorQ, args___] :=
    Enclose @ QuantumMeasurementOperator[ConfirmBy[QuantumBasis[arg, "Label" -> "M"], QuantumBasisQ] -> eigenvalues, args]

QuantumMeasurementOperator[target : _ ? targetQ, args___] :=
    QuantumMeasurementOperator[QuantumBasis[args, "Label" -> "M"], target]

QuantumMeasurementOperator[target_Integer, args___] := QuantumMeasurementOperator[{target}, args]

QuantumMeasurementOperator[] := QuantumMeasurementOperator[{1}]

QuantumMeasurementOperator[qb_ ? QuantumBasisQ -> eigenvalues_ ? VectorQ, target : (_ ? targetQ) : Automatic, args___] := Enclose @ Module[{
    basis, op, order, newTarget, qmo
},
    basis = If[ target === Automatic,
        qb,
        QuantumBasis[qb, Ceiling[Length[target] / Max[1, qb["Qudits"]]]]
    ];
    basis = QuantumBasis[basis, Ceiling[Length[eigenvalues] / basis["Dimension"]]];

    op = ConfirmBy[
        QuantumOperator[
            QuantumOperator[
                PadRight[SparseArray @ eigenvalues, basis["Dimension"]] . basis["Projectors"],
                # - Min[#, 1] + 1 &[Max[Replace[target, Automatic -> 0]] - Reverse @ Range[Max[1, basis["Qudits"]]] + 1],
                QuantumBasis[basis["OutputDimensions"], basis["InputDimensions"]]
            ],
            basis
        ],
        QuantumOperatorQ
    ];
    newTarget = Replace[target, Automatic -> op["FullInputOrder"]];
    order = PadRight[newTarget, Max[1, op["InputQudits"]], DeleteCases[op["FullInputOrder"], Alternatives @@ newTarget]];
    qmo = QuantumMeasurementOperator[
        QuantumOperator[op["State"], {Automatic, Sort @ order}],
        order[[;; Length @ newTarget]],
        args
    ];

    (* cache eigensystem *)
    (* With[{eigenvectors = ArrayReshape[basis["Elements"], {basis["Dimension"], basis["ElementDimension"]}]},
        CacheProperty[QuantumMeasurementOperator][qmo, "Eigenvalues", ___, eigenvalues];
        CacheProperty[QuantumMeasurementOperator][qmo, "Eigenvectors", ___, eigenvectors];
        CacheProperty[QuantumMeasurementOperator][qmo, "Eigensystem", ___, {eigenvalues, eigenvectors}];
        CacheProperty[QuantumMeasurementOperator][qmo, "Projectors", ___, basis["Projectors"]];
    ]; *)
    qmo
]

QuantumMeasurementOperator[qo_ ? QuantumOperatorQ, Automatic] := QuantumMeasurementOperator[qo, qo["InputOrder"]]

QuantumMeasurementOperator[qo_ ? QuantumOperatorQ] := QuantumMeasurementOperator[qo, Automatic]

QuantumMeasurementOperator[qo_ ? QuantumOperatorQ, args : PatternSequence[___, Except[_ ? targetQ]]] :=
    QuantumMeasurementOperator[QuantumOperator[qo, {qo["OutputOrder"], qo["FullInputOrder"]}, args], qo["InputOrder"]]

QuantumMeasurementOperator[qo_ ? QuantumOperatorQ, args__, target_ ? targetQ] :=
    QuantumMeasurementOperator[QuantumOperator[qo, {qo["OutputOrder"], qo["FullInputOrder"]}, args], target]


QuantumMeasurementOperator[qo_ ? QuantumOperatorQ, target : _ ? targetQ, args__] :=
    QuantumMeasurementOperator[
        With[{op = QuantumOperator[qo, args]},
            QuantumOperator[op, Sort @ Join[target, Complement[op["InputQuditOrder"] + Min[target] - 1, target]]]
        ],
        target
    ]

QuantumMeasurementOperator[tensor_ ? TensorQ /; TensorRank[tensor] == 3, target : (_ ? targetQ) : {1}, args___] :=
    QuantumMeasurementOperator[
        With[{op = QuantumOperator[MatrixFunction[Sqrt, #] & /@ tensor, args, "Label" -> "Eigen"]},
            QuantumOperator[op, {Prepend[# - Min[#] + 1 & @ Drop[op["OutputOrder"], 1], 0], op["InputOrder"]}]
        ],
        target
    ]

QuantumMeasurementOperator[tensor_ ? (TensorQ[#, Not @* StringQ] &) /; TensorRank[tensor] == 2, target : (_ ? targetQ) : {1}, args___] :=
    QuantumMeasurementOperator[
        QuantumOperator[tensor, args, "Label" -> "Eigen"],
        target
    ]

QuantumMeasurementOperator[tensor_ ? (TensorQ[#, Not @* StringQ] &) /; 2 <= TensorRank[tensor] <= 3, qb_ ? QuantumBasisQ, target : (_ ? targetQ) : {1}, args___] :=
    QuantumMeasurementOperator[tensor, target, qb, args]

QuantumMeasurementOperator[ops : {_ ? QuantumOperatorQ..}, target : (_ ? targetQ) : {1}, args___] /;
    And @@ (#["InputDimension"] == #["OutputDimension"] & /@ ops) :=
    QuantumMeasurementOperator[
        With[{op = StackQuantumOperators[Sqrt /@ ops]},
            QuantumOperator[op, {Prepend[If[Equal @@ Through[ops["OutputOrder"]], Identity, # - Min[#] + 1 &] @ Drop[op["OutputOrder"], 1], 0], op["InputOrder"]}, args]
        ],
        target
    ]

QuantumMeasurementOperator[
    qb_ ? QuantumBasisQ,
    defaultTarget : _ ? targetQ : Automatic,
    args___
    ] :=
Enclose @ Block[{
    basis = ConfirmBy[QuantumBasis[qb, args], QuantumBasisQ],
    target
},
    target = Replace[defaultTarget, Automatic -> Range[basis["Qudits"]]];
    If[ basis["Qudits"] < Length[target],
        basis = QuantumBasis[basis, Ceiling[Length[target] / Max[1, basis["Qudits"]]]]
    ];
    QuantumMeasurementOperator[basis -> If[basis["Dimension"] == 1, {1}, Range[0, basis["Dimension"] - 1]], target]
]

QuantumMeasurementOperator[qm_ ? QuantumMeasurementQ, opts___] := QuantumMeasurementOperator[qm["QuantumOperator"], opts]

QuantumMeasurementOperator[args : PatternSequence[Except[_ ? QuantumFrameworkOperatorQ | _ ? QuantumBasisQ], ___], target_ ? targetQ] :=
    Enclose @ With[{qb = ConfirmBy[QuantumBasis[args, "Label" -> "M"], QuantumBasisQ]}, QuantumMeasurementOperator[qb, target]]

QuantumMeasurementOperator[args : PatternSequence[Except[_ ? QuantumFrameworkOperatorQ], ___]] := QuantumMeasurementOperator[QuantumBasis[args, "Label" -> "M"]]

(* mutation *)

QuantumMeasurementOperator[qmo_ ? QuantumMeasurementOperatorQ, order : _ ? autoOrderQ, args___] := With[{
    op = QuantumOperator[qmo["QuantumOperator"], order]
},
    QuantumMeasurementOperator[op, Replace[qmo["Target"], Thread[qmo["InputOrder"] -> op["InputOrder"]], {1}], args]
]

QuantumMeasurementOperator[qmo_ ? QuantumMeasurementOperatorQ, args : PatternSequence[Except[_ ? targetQ], ___]] :=
    QuantumMeasurementOperator[QuantumOperator[qmo["Operator"], args], qmo["Target"]]

QuantumMeasurementOperator[qmo_ ? QuantumMeasurementOperatorQ, t : _ ? targetQ : Automatic, args___] := With[{
    target = Replace[t, Automatic -> qmo["Target"]],
    inputOrder = qmo["Operator"]["InputOrder"]
},
    QuantumMeasurementOperator[
        If[inputOrder === {} || ContainsAll[inputOrder, target], QuantumOperator[qmo["Operator"], args], QuantumOperator[qmo["Operator"], target, args]],
        target
    ]
]

(* auto reassign bad target *)
(* QuantumMeasurementOperator[qo_ ? QuantumOperatorQ, target_ ? targetQ] /; !ContainsAll[qo["FullInputOrder"], target] :=
    QuantumMeasurementOperator[
        qo,
        Cases[qo["FullInputOrder"], Alternatives @@ target] /. {} -> Automatic
    ] *)

(* QuantumMeasurementOperator[qo_ ? QuantumOperatorQ, target_ ? targetQ] /;
    qo["InputOrder"] != qo["FullInputOrder"] || qo["OutputOrder"] != qo["FullOutputOrder"] :=
    QuantumMeasurementOperator[
        QuantumOperator[qo, {qo["FullOutputOrder"], qo["FullInputOrder"]}],
        target
    ] *)


(* composition *)

(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[qs_ ? QuantumStateQ] :=
    QuantumMeasurement[qmo[QuantumOperator[qs]]["Sort"]]

(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[] := qmo[QuantumOperator[QuantumState[{"Register", qmo["InputDimensions"]}], {} -> qmo["InputOrder"]]]

(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[qo_ ? QuantumOperatorQ] := Enclose @ With[{
    top = qmo["SuperOperator"]["Sort"], bot = qo["Sort"]
},
    QuantumMeasurementOperator[
        ConfirmBy[QuantumOperator[top, {
            ReplacePart[
                top["FullOutputOrder"],
                Thread[
                    List /@ Range[qmo["Eigenqudits"]] ->
                    With[{extras = Select[bot["FullOutputOrder"], NonPositive]},
                        Take[Complement[1 - Range[Length[extras] + qmo["Eigenqudits"]], extras], - qmo["Eigenqudits"]]
                    ]
                ]
            ],
            top["InputOrder"]
        }] @ bot, QuantumOperatorQ],
        If[
            qo["OutputQudits"] < qo["InputQudits"] && qo["OutputDimension"] == qo["InputDimension"],
            bot["FullInputOrder"],
            qmo["Target"]
        ]
    ]
]


(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[qm : _ ? QuantumMeasurementOperatorQ | _ ? QuantumMeasurementQ] := Enclose @ Module[{
    top, bottom, result
},

    top = qmo["SuperOperator"]["SortOutput"];
    bottom = If[
        QuantumMeasurementOperatorQ[qm],
        qm["SuperOperator"]["SortOutput"],
        QuantumOperator[
            qm["State"],
            {Range[qm["OutputQudits"]] - qm["Eigenqudits"], Automatic}
        ]
    ];

    top = QuantumOperator[top["State"], {Join[
            With[{
                topEigens = Select[top["OutputOrder"], NonPositive],
                botEigens = Select[bottom["OutputOrder"], NonPositive]
            },
                Take[Complement[1 - Range[Length[topEigens] + Length[botEigens]], botEigens], Length[topEigens]]
            ],
            Select[top["OutputOrder"], Positive]
        ],
        top["InputOrder"]
    }];
    result = top[bottom]["SortOutput"];
    If[ QuantumMeasurementQ[qm], QuantumMeasurement, Identity] @
        QuantumMeasurementOperator[
            result,
            Join[qm["Target"], qmo["Target"]]
        ]
]

(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ] :=
    QuantumCircuitOperator[Append[qco["Operators"], qmo]]



QuantumMeasurementOperator /: (qmo1_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ) + (qmo2_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ) /;
    qmo1["Dimension"] == qmo2["Dimension"] && qmo1["Order"] == qmo2["Order"] && qmo1["Target"] == qmo2["Target"] :=
    QuantumMeasurementOperator[
        qmo1["Operator"] + qmo2["Operator"],
        qmo1["Target"]
    ]

QuantumMeasurementOperator /: f_Symbol[left : Except[_QuantumMeasurementOperator] ...,
    qmo_QuantumMeasurementOperator, right : Except[_QuantumMeasurementOperator] ...] /; MemberQ[Attributes[f], NumericFunction] :=
    QuantumMeasurementOperator[f[left, qmo["Operator"], right], qmo["Target"]]



(* equality *)

QuantumMeasurementOperator /: Equal[qmo : _QuantumMeasurementOperator ... ] :=
    Equal @@ (#["Picture"] & /@ {qmo}) && Equal @@ (#["Canonical"]["QuantumOperator"] & /@ {qmo})

