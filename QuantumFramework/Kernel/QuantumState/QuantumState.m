Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumState"]

PackageScope["quantumStateQ"]
PackageScope["QuantumStateQ"]



quantumStateQ[QuantumState[state_, basis_]] :=
    stateQ[state] &&
    QuantumBasisQ[basis] &&
    Length[state] === basis["Dimension"]

quantumStateQ[___] := False


QuantumStateQ[qs_QuantumState] := System`Private`HoldValidQ[qs]

QuantumStateQ[___] := False


qs_QuantumState /; quantumStateQ[Unevaluated[qs]] && ! System`Private`HoldValidQ[qs] := System`Private`HoldSetValid[qs]


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

(* conversion *)

QuantumState[obj : _QuantumOperator | _QuantumMeasurementOperator | _QuantumMeasurement | _QuantumChannel | _QuantumCircuitOperator, opts___] :=
    QuantumState[obj["State"], opts]


(* number *)

QuantumState[x : Except[_ ? QuantumStateQ | _ ? stateQ], basisArgs___] := QuantumState[{x}, QuantumBasis[basisArgs]]


(* expand basis *)

QuantumState[state : Except[_ ? QuantumStateQ], args : Except[_ ? QuantumBasisQ]] :=
    Enclose @ QuantumState[state, ConfirmBy[QuantumBasis[args], QuantumBasisQ]]

QuantumState[state_ ? stateQ, basis_ ? QuantumBasisQ] := QuantumState[
    state,
    QuantumTensorProduct[basis, QuantumBasis[Max[2, Length[state] - basis["Dimension"]]]]
] /; Length[state] > basis["Dimension"] > 0


(* pad state *)

QuantumState[state_ ? stateQ, basis_ ? QuantumBasisQ] := QuantumState[
    PadRight[state, Table[basis["Dimension"], TensorRank[state]]],
    basis
] /; Length[state] < basis["Dimension"]


(* Mutation *)

QuantumState[state_ ? stateQ, basis_ ? QuantumBasisQ] /; !SparseArrayQ[state] && state =!= {} :=
    Enclose @ QuantumState[ConfirmBy[SparseArray[state, Length[state]], SparseArrayQ], basis]


QuantumState[qs_ ? QuantumStateQ, args : Except[_ ? QuantumBasisQ, Except[Alternatives @@ $QuantumBasisPictures, _ ? nameQ | _Integer]]] :=
    Enclose @ QuantumState[qs, ConfirmBy[QuantumBasis[args], QuantumBasisQ]]

QuantumState[qs_ ? QuantumStateQ, args : PatternSequence[Except[_ ? QuantumBasisQ | _ ? QuantumStateQ], ___]] :=
    Enclose @ QuantumState[qs, ConfirmBy[QuantumBasis[qs["Basis"], args], QuantumBasisQ]]


(* change of basis *)

QuantumState[qs_ ? QuantumStateQ, newBasis_ ? QuantumBasisQ] /; ! newBasis["SortedQ"] := QuantumState[qs, newBasis["Sort"]]

QuantumState[qs_ ? QuantumStateQ, newBasis_ ? QuantumBasisQ] /; qs["Basis"] == newBasis := QuantumState[qs["State"], newBasis]

QuantumState[qs_ ? QuantumStateQ, newBasis_ ? QuantumBasisQ] /; qs["Dimension"] == newBasis["Dimension"] :=
    Which[
        qs["Dimension"] == 0,
        QuantumState[qs["State"], newBasis],
        qs["VectorQ"],
        QuantumState[
            SparseArrayFlatten @ Dot[
                SparsePseudoInverse[newBasis["Output"]["ReducedMatrix"]],
                qs["Output"]["ReducedMatrix"] . qs["StateMatrix"] . SparsePseudoInverse[qs["Input"]["ReducedMatrix"]],
                newBasis["Input"]["ReducedMatrix"]
            ],
            newBasis
        ],
        qs["MatrixQ"],
        QuantumState[
            Dot[
                SparsePseudoInverse[newBasis["ReducedMatrix"]],
                qs["Basis"]["ReducedMatrix"] . qs["DensityMatrix"] . SparsePseudoInverse[qs["Basis"]["ReducedMatrix"]],
                newBasis["ReducedMatrix"]
            ],
            newBasis
        ],
        True,
        $Failed
    ]

QuantumState[qs_ ? QuantumStateQ, newBasis_ ? QuantumBasisQ] := Switch[
    qs["StateType"],
    "Vector",
    QuantumState[PadRight[qs["State"], newBasis["Dimension"]], newBasis],
    "Matrix",
    QuantumState[PadRight[qs["State"], {newBasis["Dimension"], newBasis["Dimension"]}], newBasis]
]

QuantumState[qs_ ? QuantumStateQ, newBasis_ ? QuantumBasisQ, opts__] :=
    QuantumState[qs, QuantumBasis[newBasis, opts]]


(* equality *)

QuantumState /: Equal[qs : _QuantumState ...] :=
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
        If[ MemberQ[{Minus, Times}, f],
            ConfirmBy[f[left, qs["State"], right], stateQ],
            ConfirmBy[Quiet @ MatrixFunction[f[left, #, right] &, qs["DensityMatrix"], Method -> "Jordan"], MatrixQ]
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
        qs1["Basis"],
        "ParameterSpec" -> MergeParameterSpecs[qs1, qs2]
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
        qs1["Basis"],
        "ParameterSpec" -> MergeParameterSpecs[qs1, qs2]
    ]

QuantumState /: HoldPattern[Times[states : _QuantumState ? QuantumStateQ ...]] :=
    If[ Equal @@ (#["Dimension"] & /@ {states}), Fold[multiplyQuantumStates, {states}],
        Failure["QuantumState", <|"MessageTemplate" -> "Incompatible dimensions"|>]
    ]


(* differentiation *)

QuantumState /: D[qs : _QuantumState, args___] := QuantumState[D[qs["State"], args], qs["Basis"]]


(* duals *)

SuperDagger[qs_QuantumState] ^:= qs["Dagger"]
SuperStar[qs_QuantumState] ^:= qs["Conjugate"]


(* simplify *)

Simplify[qs_QuantumState, args___] ^:= qs["Simplify", args]

FullSimplify[qs_QuantumState, args___] ^:= qs["FullSimplify", args]

Chop[qs_QuantumState, args___] ^:= qs["Chop", args]


(* join *)

QuantumState[qs_QuantumState ? QuantumStateQ] := qs

QuantumState[qs__QuantumState ? QuantumStateQ] := QuantumState[
    If[ And @@ (#["PureStateQ"] & /@ {qs}),
        SparseArrayFlatten[blockDiagonalMatrix[#["StateMatrix"] & /@ {qs}]],
        blockDiagonalMatrix[#["DensityMatrix"] & /@ {qs}]
    ],
    Plus @@ (#["Basis"] & /@ {qs}),
    "Label" -> CirclePlus @@ (#["Label"] & /@ {qs}),
    "ParameterSpec" -> MergeParameterSpecs[qs1, qs2]
]


(* composition *)


(top_QuantumState ? QuantumStateQ)[(bot_QuantumState ? QuantumStateQ)] := (QuantumOperator[top] @ QuantumOperator[bot])["Sort"]["State"]


(qs1_QuantumState ? QuantumStateQ)[(qs2_QuantumState ? QuantumStateQ)] /; qs1["InputDimension"] == qs2["OutputDimension"] := Module[{
    q1 = qs1["Computational"], q2 = qs2["Computational"], state
},
    state = Which[
        qs1["OutputDimension"] * qs2["InputDimension"] == 0,
        {},
        TrueQ[qs1["VectorQ"] && qs2["VectorQ"]],
        SparseArrayFlatten[q1["StateMatrix"] . q2["StateMatrix"]],
        True,
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
            "Picture" -> If[MemberQ[{qs1["Picture"], qs2["Picture"]}, "PhaseSpace"], "PhaseSpace", qs1["Picture"]],
            "ParameterSpec" -> MergeParameterSpecs[qs1, qs2]
    ]},
        QuantumState[s, b]
    ]
]


(* parameterization *)

(qs_QuantumState ? QuantumStateQ)[ps___] /; Length[{ps}] <= qs["ParameterArity"] := With[{rules = Thread[Take[qs["Parameters"], Length[{ps}]] -> {ps}]},
    QuantumState[
        Map[ReplaceAll[rules], qs["State"], {If[qs["VectorQ"], 1, 2]}],
        QuantumBasis[qs["Basis"], "Label" -> qs["Label"] /. rules, "ParameterSpec" -> Drop[qs["ParameterSpec"], Length[{ps}]]]
    ]
]
