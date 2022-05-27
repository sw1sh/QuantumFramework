Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumState"]

PackageScope["QuantumStateQ"]


QuantumState::inState = "is invalid";

QuantumState::inBasis = "has invalid basis";

QuantumState::incompatible = "is incompatible with its basis";


QuantumStateQ[QuantumState[state_, basis_]] :=
    (stateQ[state] || (Message[QuantumState::inState]; False)) &&
    (QuantumBasisQ[basis] || (Message[QuantumState::inBasis]; False)) &&
    (Length[state] === basis["Dimension"] || (Message[QuantumState::incompatible]; False))

QuantumStateQ[___] := False


(* basis argument input *)

QuantumState[state_ ? stateQ, basisArgs___] /; !QuantumBasisQ[basisArgs] := Enclose @ Module[{
    basis, multiplicity
},
    basis = ConfirmBy[QuantumBasis[basisArgs], QuantumBasisQ];
    multiplicity = basisMultiplicity[Length[state], basis["Dimension"]];
    basis = ConfirmBy[QuantumBasis[basis, multiplicity], QuantumBasisQ];
    QuantumState[
        PadRight[state, Table[basis["Dimension"], TensorRank[state]]],
        basis
    ]
]


(* association input *)

QuantumState[state_ ? AssociationQ, basisArgs___] /; VectorQ[Values[state]] := Enclose @ Module[{
    basis = ConfirmBy[QuantumBasis[basisArgs], QuantumBasisQ], multiplicity},
    multiplicity = basisMultiplicity[Length[state], basis["Dimension"]];
    basis = ConfirmBy[QuantumBasis[basis, multiplicity], QuantumBasisQ];
    ConfirmAssert[ContainsOnly[QuditName /@ Keys[state], basis["ElementNames"]], "Association keys and basis names don't match"];
    QuantumState[
        Values @ KeyMap[QuditName, state][[Key /@ basis["ElementNames"]]] /. _Missing -> 0,
        basis
    ]
]


(* eigenvalues input *)

QuantumState["Eigenvalues" -> eigenvalues_ ? VectorQ, basisArgs___] := With[{
    basis = QuantumBasis[basisArgs]
},
    QuantumState[
        Total @ MapThread[#1 #2 &, {eigenvalues, basis["Projectors"]}],
        basis
    ] /; Length[eigenvalues] == basis["Dimension"]
]


(* expand basis *)

QuantumState[state : Except[_ ? QuantumStateQ], args : Except[_ ? QuantumBasisQ]] :=
    Enclose @ QuantumState[state, ConfirmBy[QuantumBasis[args], QuantumBasisQ]]

QuantumState[state_ ? stateQ, basis_ ? QuantumBasisQ] := QuantumState[
    state,
    QuantumTensorProduct[basis, QuantumBasis[Max[2, Length[state] - basis["Dimension"]]]]
] /; Length[state] > basis["Dimension"]


(* pad state *)

QuantumState[state_ ? stateQ, basis_ ? QuantumBasisQ] := QuantumState[
    PadRight[state, Table[basis["Dimension"], TensorRank[state]]],
    basis
] /; Length[state] < basis["Dimension"]


(* Mutation *)

QuantumState[state_ ? stateQ, basis_ ? QuantumBasisQ] /; !SparseArrayQ[state] :=
    QuantumState[SparseArray[state], basis]


QuantumState[qs_ ? QuantumStateQ, args : Except[_ ? QuantumBasisQ, Except[Alternatives @@ $QuantumBasisPictures, _ ? nameQ | _Integer]]] :=
    Enclose @ QuantumState[qs, ConfirmBy[QuantumBasis[args], QuantumBasisQ]]

QuantumState[qs_ ? QuantumStateQ, args : PatternSequence[Except[_ ? QuantumBasisQ], ___]] :=
    Enclose @ QuantumState[qs, ConfirmBy[QuantumBasis[qs["Basis"], args], QuantumBasisQ]]


(* change of basis *)

QuantumState[qs_ ? QuantumStateQ, newBasis_ ? QuantumBasisQ] /; ! newBasis["SortedQ"] := QuantumState[qs, newBasis["Sort"]]

QuantumState[qs_ ? QuantumStateQ, newBasis_ ? QuantumBasisQ] /; qs["Basis"] == newBasis := QuantumState[qs["State"], newBasis]

QuantumState[qs_ ? QuantumStateQ, newBasis_ ? QuantumBasisQ] /; qs["ElementDimension"] == newBasis["ElementDimension"] := Switch[
    qs["StateType"],
    "Vector",
    QuantumState[
        SparseArrayFlatten[
            SparsePseudoInverse[newBasis["OutputMatrix"]] . (qs["OutputMatrix"] . qs["StateMatrix"] . SparsePseudoInverse[qs["InputMatrix"]]) . newBasis["InputMatrix"]
        ],
        newBasis
    ],
    "Matrix",
    QuantumState[
        SparsePseudoInverse[newBasis["Matrix"]] . (qs["Basis"]["Matrix"] . qs["DensityMatrix"] . SparsePseudoInverse[qs["Basis"]["Matrix"]]) . newBasis["Matrix"],
        newBasis
    ]
]


(* renew basis *)

QuantumState[qs_ ? QuantumStateQ] := qs["Computational"]

(*QuantumState[qs_ ? QuantumStateQ, args__] := With[{
    newBasis = QuantumBasis[qs["Basis"], args]},
    If[ qs["Basis"] === newBasis,
        qs,
        QuantumState[qs["State"], newBasis]
    ]
]
*)

(* equality *)

SetPrecisionNumeric[x_ /; NumericQ[x] || ArrayQ[x, _, NumericQ]] := SetPrecision[x, $MachinePrecision - 2]

SetPrecisionNumeric[x_] := x


QuantumState /: Equal[qs : _QuantumState ...] :=
    Equal @@ (#["Picture"] & /@ {qs}) &&
    Which[
        And @@ (#["PureStateQ"] & /@ {qs}),
        Thread[Equal @@ (Chop @ SetPrecisionNumeric[#["Computational"]["CanonicalStateVector"]] & /@ {qs})],
        And @@ (#["MixedStateQ"] & /@ {qs}),
        Thread[Equal @@ (Chop @ SetPrecisionNumeric[SparseArrayFlatten @ #["NormalizedMatrixRepresentation"]] & /@ {qs})],
        True,
        Thread[Equal @@ (Chop @ SetPrecisionNumeric[#["Split", #["Qudits"]]["Bend"]["Computational"]["CanonicalStateVector"]] & /@ {qs})]
    ]


(* numeric function *)

QuantumState /: f_Symbol[left : Except[_QuantumState] ..., qs_QuantumState, right : Except[_QuantumState] ...] /; MemberQ[Attributes[f], NumericFunction] :=
    Enclose @ QuantumState[
        If[ MemberQ[{Plus, Minus, Times}, f],
            ConfirmBy[f[left, qs["State"], right], stateQ],
            ConfirmBy[MatrixFunction[f[left, #, right] &, qs["DensityMatrix"], Method -> "Jordan"], MatrixQ]
        ],
        qs["Basis"]
    ]

(* Trace *)

QuantumState /: Tr[qs_QuantumState] := Tr @ qs["DensityMatrix"]


(* addition *)

addQuantumStates[qs1_QuantumState ? QuantumStateQ, qs2_QuantumState ? QuantumStateQ] /; qs1["Dimension"] == qs2["Dimension"] :=
    QuantumState[
        QuantumState[
            If[ qs1["StateType"] === qs2["StateType"] === "Vector",
                qs1["VectorRepresentation"] + qs2["VectorRepresentation"],
                qs1["MatrixRepresentation"] + qs2["MatrixRepresentation"]
            ],
            QuantumBasis[qs1["OutputDimensions"], qs1["InputDimensions"]]
        ],
        qs1["Basis"]
    ]

QuantumState /: HoldPattern[Plus[states : _QuantumState ...]] :=
    If[ Equal @@ (#["Dimension"] & /@ {states}), Fold[addQuantumStates, {states}],
        Failure["QuantumState", <|"MessageTemplate" -> "Incompatible dimensions"|>]
    ]


(* multiplication *)

multiplyQuantumStates[qs1_QuantumState, qs2_QuantumState] /; qs1["Dimension"] == qs2["Dimension"] :=
    QuantumState[
        QuantumState[
            If[ qs1["StateType"] === qs2["StateType"] === "Vector",
                qs1["VectorRepresentation"] * qs2["VectorRepresentation"],
                qs1["MatrixRepresentation"] * ArrayReshape[qs2["MatrixRepresentation"], Dimensions @ qs1["MatrixRepresentation"]]
            ],
            QuantumBasis[qs1["Dimensions"]]
        ],
        qs1["Basis"]
    ]

QuantumState /: HoldPattern[Times[states : _QuantumState ? QuantumStateQ ...]] :=
    If[ Equal @@ (#["Dimension"] & /@ {states}), Fold[multiplyQuantumStates, {states}],
        Failure["QuantumState", <|"MessageTemplate" -> "Incompatible dimensions"|>]
    ]



(* composition *)


(qs1_QuantumState ? QuantumStateQ)[(qs2_QuantumState ? QuantumStateQ)] /; qs1["Input"] == qs2["Output"] := Module[{
    state
},
    state = If[ qs1["VectorQ"] && qs2["VectorQ"],
        SparseArrayFlatten[qs1["StateMatrix"] . qs2["StateMatrix"]],
        With[{q1 = If[qs1["VectorQ"], qs1["Double"], qs1], q2 = If[qs2["VectorQ"], qs2["Double"], qs2]},
            ArrayReshape[
                q1["StateMatrix"] . q2["StateMatrix"],
                Table[qs1["OutputDimension"] qs2["InputDimension"], 2]
            ]
        ]
    ];
    QuantumState[
        state,
        "Output" -> qs1["Output"], "Input" -> qs2["Input"],
        "Label" -> qs1["Label"] @* qs2["Label"],
        "ParameterSpec" -> Join[qs2["ParameterSpec"], qs1["ParameterSpec"]]
    ]
]


(qs1_QuantumState ? QuantumStateQ)[(qs2_QuantumState ? QuantumStateQ)] /; qs1["InputDimension"] == qs2["OutputDimension"] := Module[{
    q1 = qs1["Computational"], q2 = qs2["Computational"], state
},
    state = If[
        TrueQ[qs1["VectorQ"] && qs2["VectorQ"]],
        SparseArrayFlatten[q1["StateMatrix"] . q2["StateMatrix"]],
        If[qs1["VectorQ"], q1 = q1["Double"]];
        If[qs2["VectorQ"], q2 = q2["Double"]];
        ArrayReshape[
            q1["StateMatrix"] . q2["StateMatrix"],
            Table[qs1["OutputDimension"] qs2["InputDimension"], 2]
        ]
    ];
    With[{
        s = QuantumState[state, "Output" -> QuditBasis @ qs1["OutputDimensions"], "Input" -> QuditBasis @ qs2["InputDimensions"]],
        b = QuantumBasis[
            "Output" -> qs1["Output"], "Input" -> qs2["Input"],
            "Label" -> qs1["Label"] @* qs2["Label"],
            "ParameterSpec" -> Join[qs2["ParameterSpec"], qs1["ParameterSpec"]]
    ]},
        QuantumState[s, b]
    ]
]


(* parameterization *)

(qs_QuantumState ? QuantumStateQ)[ps___] /; Length[{ps}] <= qs["ParameterArity"] :=
    QuantumState[
        Map[ReplaceAll[Thread[Take[qs["Parameters"], Length[{ps}]] -> {ps}]], qs["State"], {-1}],
        QuantumBasis[qs["Basis"], "ParameterSpec" -> Drop[qs["ParameterSpec"], Length[{ps}]]]
    ]
