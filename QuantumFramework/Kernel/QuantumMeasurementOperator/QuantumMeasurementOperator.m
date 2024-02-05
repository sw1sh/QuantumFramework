Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumMeasurementOperator"]

PackageScope["QuantumMeasurementOperatorQ"]



quantumMeasurementOperatorQ[QuantumMeasurementOperator[qo_, _ ? targetsQ]] := QuantumOperatorQ[qo]

quantumMeasurementOperatorQ[___] := False


QuantumMeasurementOperatorQ[qmo_QuantumMeasurementOperator] := System`Private`HoldValidQ[qmo]

QuantumMeasurementOperatorQ[___] := False


qmo_QuantumMeasurementOperator /; quantumMeasurementOperatorQ[Unevaluated[qmo]] && ! System`Private`HoldValidQ[qmo] := System`Private`HoldSetValid[qmo]


(* constructors *)

SetAttributes[QuantumMeasurementOperator, NHoldRest]

QuantumMeasurementOperator[arg_ -> eigenvalues_ ? VectorQ, args___] :=
    Enclose @ QuantumMeasurementOperator[ConfirmBy[QuantumBasis[arg], QuantumBasisQ] -> eigenvalues, args]

QuantumMeasurementOperator[qo_ ? QuantumOperatorQ, target_ ? targetQ] := QuantumMeasurementOperator[qo, {target}]

QuantumMeasurementOperator[target : _Integer | {___Integer}, args___] := If[MatchQ[target, (_ ? targetsQ | _ ? targetQ)],
    QuantumMeasurementOperator[QuantumBasis[args], target],
    Failure["BadTarget", <|"MessageTemplate" -> "Measurement target should only contain positive integers."|>]
]

QuantumMeasurementOperator[target_ -> arg_, opts___] := QuantumMeasurementOperator[QuantumBasis[arg], target, opts]

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

QuantumMeasurementOperator[qo_ ? QuantumOperatorQ, args__, target : (_ ? targetsQ | _ ? targetQ)] :=
    QuantumMeasurementOperator[QuantumOperator[qo, {qo["OutputOrder"], qo["FullInputOrder"]}, args], target]


QuantumMeasurementOperator[qo_ ? QuantumOperatorQ, target : (_ ? targetsQ | _ ? targetQ), args__] :=
    QuantumMeasurementOperator[QuantumOperator[qo, args], target]

QuantumMeasurementOperator[qo_ ? QuantumOperatorQ, args : PatternSequence[___, Except[_ ? targetsQ | _ ? targetQ]]] :=
    QuantumMeasurementOperator[QuantumOperator[qo, {qo["OutputOrder"], qo["FullInputOrder"]}, args], qo["InputOrder"]]


QuantumMeasurementOperator[tensor_ ? TensorQ /; TensorRank[tensor] == 3, target : (_ ? targetQ) : {1}, args___] :=
    QuantumMeasurementOperator[
        With[{op = QuantumOperator[MatrixFunction[Sqrt, #] & /@ tensor], dims = Dimensions[tensor]},
            QuantumOperator[
                op["State"]["Split", 2],
                {Prepend[target, 0], target},
                QuantumBasis[
                    QuantumTensorProduct[QuditBasis[Table[Subscript["\[ScriptCapitalE]", i], {i, dims[[1]]}]], QuditBasis[dims[[2]]]],
                    QuditBasis[dims[[3]]]
                ]
            ]
        ],
        target,
        args
    ]

QuantumMeasurementOperator[tensor_ ? (TensorQ[#, Not @* StringQ] &) /; TensorRank[tensor] == 2, target : (_ ? targetsQ | _ ? targetQ) : {1}, args___] :=
    QuantumMeasurementOperator[
        QuantumOperator[tensor, args],
        target
    ]

QuantumMeasurementOperator[tensor_ ? (TensorQ[#, Not @* StringQ] &) /; 2 <= TensorRank[tensor] <= 3, qb_ ? QuantumBasisQ, target : (_ ? targetsQ | _ ? targetQ) : {1}, args___] :=
    QuantumMeasurementOperator[tensor, target, qb, args]

QuantumMeasurementOperator[ops : {_ ? QuantumOperatorQ..}, target : (_ ? targetQ) : {1}, args___] /;
    And @@ (#["InputDimension"] == #["OutputDimension"] & /@ ops) :=
    QuantumMeasurementOperator[
        With[{op = StackQuantumOperators[Sqrt /@ ops]},
            QuantumOperator[op["State"], {Prepend[#, 0], #} & @ Join[target, Drop[Drop[Union @@ op["Order"], Length[target]], 1]], args]
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

QuantumMeasurementOperator[args : PatternSequence[Except[_ ? QuantumFrameworkOperatorQ | _ ? QuantumBasisQ], ___], target : (_ ? targetsQ | _ ? targetQ), opts___] :=
    Enclose @ With[{qb = ConfirmBy[QuantumBasis[args], QuantumBasisQ]}, QuantumMeasurementOperator[qb, target, opts]]

QuantumMeasurementOperator[args : PatternSequence[Except[_ ? QuantumFrameworkOperatorQ], ___]] := QuantumMeasurementOperator[QuantumBasis[args]]

(* mutation *)

QuantumMeasurementOperator[qmo_ ? QuantumMeasurementOperatorQ, order : _ ? autoOrderQ, args___] := With[{
    op = QuantumOperator[qmo["QuantumOperator"], order]
},
    QuantumMeasurementOperator[op, Replace[qmo["Targets"], Thread[qmo["InputOrder"] -> op["InputOrder"]], {2}], args]
]

QuantumMeasurementOperator[qmo_ ? QuantumMeasurementOperatorQ, args : PatternSequence[Except[_ ? targetsQ | _ ? targetQ], ___]] :=
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

(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[args___] := QuantumCircuitOperator[qmo][args]

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
            qmo["Targets"]
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
            Join[qm["Targets"], qmo["Targets"]]
        ]
]

(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ] :=
    QuantumCircuitOperator[Append[qco["Operators"], qmo]]



QuantumMeasurementOperator /: (qmo1_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ) + (qmo2_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ) /;
    qmo1["Dimension"] == qmo2["Dimension"] && qmo1["Order"] == qmo2["Order"] && qmo1["Target"] == qmo2["Target"] :=
    QuantumMeasurementOperator[
        qmo1["Operator"] + qmo2["Operator"],
        qmo1["Targets"]
    ]

QuantumMeasurementOperator /: f_Symbol[left : Except[_QuantumMeasurementOperator] ...,
    qmo_QuantumMeasurementOperator, right : Except[_QuantumMeasurementOperator] ...] /; MemberQ[Attributes[f], NumericFunction] :=
    QuantumMeasurementOperator[f[left, qmo["Operator"], right], qmo["Target"]]



(* equality *)

QuantumMeasurementOperator /: Equal[qmo : _QuantumMeasurementOperator ... ] :=
    Equal @@ (#["Picture"] & /@ {qmo}) && Equal @@ (#["Canonical"]["QuantumOperator"] & /@ {qmo})


(* simplify *)

Scan[
    (Symbol[#][qmo_QuantumMeasurementOperator, args___] ^:= qmo[#, args]) &,
    {"Simplify", "FullSimplify", "Chop", "ComplexExpand"}
]


(* parameterization *)

(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[ps : PatternSequence[p : Except[_Association], ___]] /; ! MemberQ[QuantumMeasurementOperator["Properties"], p] && Length[{ps}] <= qmo["ParameterArity"] :=
    qmo[AssociationThread[Take[qmo["Parameters"], UpTo[Length[{ps}]]], {ps}]]

(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[rules_ ? AssociationQ] /; ContainsOnly[Keys[rules], qmo["Parameters"]] :=
    QuantumMeasurementOperator[qmo["Operator"][rules], qmo["Targets"]]

