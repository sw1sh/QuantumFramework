Package["Wolfram`QuantumFramework`"]

PackageScope["UnitaryEulerAngles"]
PackageScope["UnitaryEulerAnglesWithPhase"]



$QuantumOperatorProperties = {
    "Order",
    "InputOrder", "OutputOrder", "ControlOrder", "TargetOrder",
    "MatrixRepresentation", "Matrix",
    "TensorRepresentation", "Tensor",
    "Table",
    "Ordered", "OrderedInput", "OrderedOutput",
    "SortInput", "SortOutput", "Sort", "SortedQ",
    "ReverseOutput", "ReverseInput", "Reverse",
    "Shift",
    "OrderedMatrixRepresentation", "OrderedMatrix",
    "OrderedTensorRepresentation", "OrderedTensor",
    "Arity", "MaxArity", "FullArity", "TargetArity",
    "Range", "FullInputOrder", "FullOutputOrder", "FullOrder", "InputQuditOrder", "OutputQuditOrder", "QuditOrder",
    "OutputOrderQuditMapping", "InputOrderQuditMapping",
    "FirstOutputQudit", "LastOutputQudit", "FirstInputQudit", "LastInputQudit", "InputOrderQuditMapping",
    "HermitianQ", "UnitaryQ", "Eigenvalues", "Eigenvectors", "Eigensystem", "Projectors",
    "Transpose", "ConjugateTranspose",
    "UnstackOutput", "UnstackInput",
    "QuantumOperator", "Operator",
    "Computational", "Diagonalize",
    "Dagger", "Dual",
    "TraceNorm",
    "PauliDecompose",
    "CircuitDiagram",
    "OrderedFormula",
    "Simplify", "FullSimplify", "Chop", "ComplexExpand"
};

QuantumOperator["Properties"] := Union @ Join[$QuantumOperatorProperties, Complement[QuantumState["Properties"], {
    "BlochCartesianCoordinates", "BlochSphericalCoordinates", "BlochPlot"
}]];

QuantumOperatorProp[qo_, "Properties"] := Union @ Join[QuantumOperator["Properties"], Complement[qo["State"]["Properties"], {
    "BlochCartesianCoordinates", "BlochSphericalCoordinates", "BlochPlot"
}]]


qo_QuantumOperator["ValidQ"] := QuantumOperatorQ[qo]


QuantumOperator::undefprop = "property `` is undefined for this state";

QuantumOperator::failprop = "property `` have failed with ``";


(* basic getters *)

QuantumOperatorProp[QuantumOperator[state_, _], "State"] := state

QuantumOperatorProp[QuantumOperator[_, order : {_, _}], "Order"] := order

QuantumOperatorProp[QuantumOperator[_, {_, inputOrder_}], "InputOrder"] := inputOrder

QuantumOperatorProp[QuantumOperator[_, {outputOrder_, _}], "OutputOrder"] := outputOrder


(qo_QuantumOperator[prop_ ? propQ, args___]) /; QuantumOperatorQ[qo] := With[{
    result = QuantumOperatorProp[qo, prop, args]
    },
    If[ TrueQ[$QuantumFrameworkPropCache] &&
        ! MemberQ[{"Properties", "State", "Basis"}, prop] &&
        QuantumOperatorProp[qo, "Basis"]["ParameterArity"] == 0,
        QuantumOperatorProp[qo, prop, args] = result,
        result
    ] /;
        (!FailureQ[Unevaluated @ result] || Message[QuantumOperator::failprop, prop, result]) &&
        (!MatchQ[result, _QuantumOperatorProp] || Message[QuantumOperator::undefprop, prop])
]


(* computed properties *)

QuantumOperatorProp[qo_, "Arity"] := Length @ qo["InputOrder"]

QuantumOperatorProp[qo_, "Range"] := Max[qo["InputOrder"]] - Min[qo["InputOrder"]] + 1

QuantumOperatorProp[qo_, "MaxArity"] := Max[qo["InputQudits"], qo["Arity"]]

QuantumOperatorProp[qo_, "FullArity"] := Max[qo["InputQudits"], qo["Range"]]

QuantumOperatorProp[qo_, "FullInputOrder"] := Which[qo["InputDimension"] > 1,
    Take[
        If[MatchQ[qo["InputOrder"], {___, _ ? NonPositive, ___}], Identity, # - Min[#, 1] + 1 &] @
            If[ Length[qo["InputOrder"]] > 0,
                Join[Complement[Range[Max[qo["InputOrder"]] - qo["InputQudits"] + 1, Max[qo["InputOrder"]]], qo["InputOrder"]], qo["InputOrder"]],
                Range[qo["InputQudits"]]
            ],
        - qo["InputQudits"]
    ],
    qo["InputDimension"] == 0,
    {1},
    True,
    {}
]

QuantumOperatorProp[qo_, "FullOutputOrder"] := Which[qo["OutputDimension"] > 1,
    Take[
        If[ MatchQ[qo["OutputOrder"], {___, _ ? NonPositive, ___}], Identity, # - Min[#, 1] + 1 &] @
            If[ Length[qo["OutputOrder"]] > 0,
                Join[Complement[Range[Max[qo["OutputOrder"]] - qo["OutputQudits"] + 1, Max[qo["OutputOrder"]]], qo["OutputOrder"]], qo["OutputOrder"]],
                Range[qo["OutputQudits"]]
            ],
        - qo["OutputQudits"]
    ],
    qo["OutputDimension"] == 0,
    {1},
    True,
    {}
]

QuantumOperatorProp[qo_, "FullOrder"] := {qo["FullOutputOrder"], qo["FullInputOrder"]}

QuantumOperatorProp[qo_, "SetFullOutputOrder"] := QuantumOperator[qo, {qo["FullOutputOrder"], qo["InputOrder"]}]

QuantumOperatorProp[qo_, "SetFullInputOrder"] := QuantumOperator[qo, {qo["OutputOrder"], qo["FullInputOrder"]}]

QuantumOperatorProp[qo_, "SetFullOrder"] := QuantumOperator[qo, {qo["FullOutputOrder"], qo["FullInputOrder"]}]


QuantumOperatorProp[qo_, "ControlOrder1"] :=
    FirstCase[qo["Label"], Subscript["C", _][control1_, ___] | Interpretation[_, Subscript["C", _][control1_, ___]] :> control1, {}, {0}]

QuantumOperatorProp[qo_, "ControlOrder0"] :=
    FirstCase[qo["Label"], Subscript["C", _][_, control0_]  | Interpretation[_, Subscript["C", _][_, control0_]] :> control0, {}, {0}]

QuantumOperatorProp[qo_, "ControlOrder"] :=
    FirstCase[qo["Label"], Subscript["C", _][control1_, control0_ : {}] | Interpretation[_, Subscript["C", _][control1_, control0_ : {}]] :> Join[control1, control0], {}, {0}]

QuantumOperatorProp[qo_, "TargetOrder"] := Enclose[DeleteCases[qo["InputOrder"], Alternatives @@ Confirm @ qo["ControlOrder"]], qo["InputOrder"] &]

QuantumOperatorProp[qo_, "ControlArity"] := Length @ qo["ControlOrder"]

QuantumOperatorProp[qo_, "TargetArity"] := Length @ qo["TargetOrder"]

QuantumOperatorProp[qo_, "FirstInputQudit"] := Min @ qo["FullInputOrder"]

QuantumOperatorProp[qo_, "LastInputQudit"] := Max @ qo["FullInputOrder"]

QuantumOperatorProp[qo_, "FirstOutputQudit"] := Min @ qo["FullOutputOrder"]

QuantumOperatorProp[qo_, "LastOutputQudit"] := Max @ qo["FullOutputOrder"]

QuantumOperatorProp[qo_, "InputQuditOrder"] := qo["InputOrder"] - Min[qo["FullInputOrder"]] + 1

QuantumOperatorProp[qo_, "OutputQuditOrder"] := qo["OutputOrder"] - Min[qo["FullOutputOrder"]] + 1

QuantumOperatorProp[qo_, "QuditOrder"] := {qo["OutputQuditOrder"], qo["InputQuditOrder"]}

QuantumOperatorProp[qo_, "InputOrderQuditMapping"] := Thread[qo["FullInputOrder"] -> Range[qo["InputQudits"]]]

QuantumOperatorProp[qo_, "OutputOrderQuditMapping"] := Thread[qo["FullOutputOrder"] -> Range[qo["OutputQudits"]]]

QuantumOperatorProp[qo_, "OutputOrderDimensions"] := AssociationMap[
    qo["OutputDimensions"][[Replace[#, qo["OutputOrderQuditMapping"]]]] &,
    qo["FullOutputOrder"]
]

QuantumOperatorProp[qo_, "InputOrderDimensions"] := AssociationMap[
    qo["InputDimensions"][[Replace[#, qo["InputOrderQuditMapping"]]]] &,
    qo["FullInputOrder"]
]

QuantumOperatorProp[qo_, "TargetDimensions"] := qo["TargetOrder"] /. qo["InputOrderDimensions"]


QuantumOperatorProp[qo_, "SquareQ"] := qo["OutputDimension"] == qo["InputDimension"]

QuantumOperatorProp[qo_, "Tensor"] := qo["Sort"]["StateTensor"]

QuantumOperatorProp[qo_, "TensorRepresentation"] := qo["Sort"]["State"]["TensorRepresentation"]


QuantumOperatorProp[qo_, "Matrix"] := qo["Sort"]["StateMatrix"]

QuantumOperatorProp[qo_, "MatrixRepresentation"] := qo["Computational"]["Matrix"]

QuantumOperatorProp[qo_, "Operator"] := qo["Amplitudes"]

QuantumOperatorProp[qo_, "Table"] := TableForm[qo["Matrix"], TableHeadings -> {qo["Output"]["Names"], qo["Input"]["Names"]}, TableAlignments -> Center]

QuantumOperatorProp[qo_, "QuantumOperator"] := qo

QuantumOperatorProp[qo_, name : "Computational" | "SchmidtBasis" | "SpectralBasis"] := QuantumOperator[qo["State"][name], qo["Order"]]


QuantumOperatorProp[qo_, "OrderedMatrix"] := qo["Ordered"]["Matrix"]

QuantumOperatorProp[qo_, "OrderedMatrixRepresentation"] := qo["Ordered"]["MatrixRepresentation"]

QuantumOperatorProp[qo_, "OrderedTensor"] := qo["Ordered"]["Tensor"]

QuantumOperatorProp[qo_, "OrderedTensorRepresentation"] := qo["Ordered"]["TensorRepresentation"]


QuantumOperatorProp[qo_, "MatrixQuantumState"] /; qo["OutputDimension"] == qo["InputDimension"] := QuantumState[qo["Matrix"], qo["Output"]]

QuantumOperatorProp[qo_, prop : "MatrixState" | "VectorState" | "ToMatrix" | "ToVector" | "Vector"] := QuantumOperator[qo["State"][prop], qo["Order"]]

QuantumOperatorProp[qo_, "PermuteInput", perm_Cycles] := QuantumOperator[
    qo["State"]["PermuteInput", perm],
    qo["Order"]
]

QuantumOperatorProp[qo_, "PermuteOutput", perm_Cycles] := QuantumOperator[
    qo["State"]["PermuteOutput", perm],
    qo["Order"]
]

QuantumOperatorProp[qo_, "OrderInputExtra", inputSource_ ? orderQ, inputTarget_ ? orderQ] := Module[{
    extra = DeleteCases[inputTarget, Alternatives @@ inputSource],
    inputPerm, outputTarget
},
    (* assume extra qudits are on the right *)
    inputPerm = FindPermutation[Join[inputSource, extra], inputTarget];
    outputTarget = qo["FullOutputOrder"] /. Thread[Permute[qo["FullInputOrder"], inputPerm] -> inputTarget];
    QuantumOperator[
        qo["PermuteInput", inputPerm],
        {outputTarget, Sort @ inputTarget}
    ]
]

QuantumOperatorProp[qo_, "OrderOutputExtra", outputSource_ ? orderQ, outputTarget_ ? orderQ] := Module[{
    extra = DeleteCases[outputTarget, Alternatives @@ outputSource],
    outputPerm, inputTarget
},
    (* assume extra qudits are on the right *)
    outputPerm = FindPermutation[Join[outputSource, extra], outputTarget];
    inputTarget = qo["FullInputOrder"] /. Thread[Permute[qo["FullOutputOrder"], outputPerm] -> outputTarget];
    QuantumOperator[
        qo["PermuteOutput", outputPerm],
        {Sort @ outputTarget, inputTarget}
    ]
]

QuantumOperatorProp[qo_, "SortInput"] := If[
    OrderedQ[qo["InputOrder"]],
    qo,
    QuantumOperator[
        qo[
            "PermuteInput",
            InversePermutation @ FindPermutation[Ordering @ Ordering @ qo["FullInputOrder"]]
        ]["State"],
        {qo["OutputOrder"], Sort @ qo["InputOrder"]}
    ]
]

QuantumOperatorProp[qo_, "SortOutput"] := If[
    OrderedQ[qo["OutputOrder"]],
    qo,
    QuantumOperator[
        qo[
            "PermuteOutput",
            InversePermutation @ FindPermutation[Ordering @ Ordering @ qo["FullOutputOrder"]]
        ]["State"],
        {Sort @ qo["OutputOrder"], qo["InputOrder"]}
    ]
]


QuantumOperatorProp[qo_, "Sort"] := QuantumOperator[
    qo["SortOutput"]["SortInput"],
    "Label" -> collectLabel @ Replace[qo["Label"], {
        Subscript["C", subLabel_][controls__] :>
            Subscript["C", sortLabel[subLabel, qo["TargetOrder"]]][controls],
        Subscript["R", subLabel_][angle_] :>
            Subscript["R", sortLabel[subLabel, qo["InputOrder"]]][angle],
        "\[Pi]"[perm__] :> "\[Pi]" @@ {perm}[[Ordering[qo["InputOrder"]]]],
        subLabel_  :> sortLabel[subLabel, qo["InputOrder"]]
    }]
]

QuantumOperatorProp[qo_, "SortedQ"] := AllTrue[qo["Order"], OrderedQ]


QuantumOperatorProp[qo_, "ReverseOutput"] := QuantumOperator[qo["State"], {Reverse @ qo["OutputOrder"], qo["InputOrder"]}]

QuantumOperatorProp[qo_, "ReverseInput"] := QuantumOperator[qo["State"], {qo["OutputOrder"], Reverse @ qo["InputOrder"]}]

QuantumOperatorProp[qo_, "Reverse"] := QuantumOperator[qo["State"], Reverse /@ qo["Order"]]


QuantumOperatorProp[qo_, "Ordered" | "OrderedInput"] := qo["OrderedInput", Sort @ qo["FullInputOrder"]]

QuantumOperatorProp[qo_, "OrderedOutput"] := qo["OrderedOutput", Sort @ qo["FullOutputOrder"]]

QuantumOperatorProp[qo_, "OrderedOutput", from_Integer, to_Integer] /; qo["OutputQudits"] == 1 :=
    qo["OrderedOutput", Range[from, to], QuditBasis[qo["Output"], to - from + 1]]

QuantumOperatorProp[qo_, "Ordered" | "OrderedInput", from_Integer, to_Integer] /; qo["InputQudits"] == 1 :=
    qo["OrderedInput", Range[from, to], QuditBasis[qo["Input"], to - from + 1]]

QuantumOperatorProp[qo_, prop : "Ordered" | "OrderedInput" | "OrderedOutput", from_Integer, to_Integer] :=
    qo[prop, Range[from, to]]

QuantumOperatorProp[qo_, "Ordered" | "OrderedInput", order_ ? orderQ] :=
    qo["OrderedInput", order, QuantumTensorProduct[
        order /. Join[
            Thread[qo["FullInputOrder"] -> qo["Input"]["Decompose"]],
            Thread[qo["FullOutputOrder"] -> qo["Output"]["Decompose"]],
            # -> QuditBasis[2] & /@ Complement[order, Join @@ qo["FullOrder"]]
        ]
    ]]

QuantumOperatorProp[qo_, "OrderedOutput", order_ ? orderQ] :=
    qo["OrderedOutput", order, QuantumTensorProduct[
            order /. Join[
            Thread[qo["FullOutputOrder"] -> qo["Output"]["Decompose"]],
            Thread[qo["FullInputOrder"] -> qo["Input"]["Decompose"]],
            # -> QuditBasis[2] & /@ Complement[order, Join @@ qo["FullOrder"]]
        ]
    ]]

QuantumOperatorProp[qo_, "Ordered", from_Integer, to_Integer, qb_ ? QuditBasisQ] := qo["Ordered", Range[from, to], qb]

QuantumOperatorProp[qo_, "Ordered", order_ ? orderQ, qb_ ? QuditBasisQ] :=
    qo["OrderedInput", order, qb]

QuantumOperatorProp[qo_, "Ordered" | "OrderedInput" | "OrderedOutput", {}, ___] := qo

QuantumOperatorProp[qo_, "OrderedInput", qb_ ? QuditBasisQ] := qo["OrderedInput", qo["FullInputOrder"], qb]

QuantumOperatorProp[qo_, "OrderedInput", order_ ? orderQ, qb_ ? QuditBasisQ] := Enclose @ Block[{
    arity, pos
},
    ConfirmAssert[ContainsAll[order, qo["FullInputOrder"]], "Given order should contain all operator order qudits"];
    arity = Length[order];
    pos = Catenate @ Lookup[PositionIndex[order], qo["FullInputOrder"], Nothing];
    ConfirmAssert[arity <= qb["Qudits"], "Order size should be less than or equal to number of qudits"];
    QuantumOperator[
        If[ arity > qo["InputQudits"],
            QuantumTensorProduct[
                QuantumOperator[qo, {qo["OutputOrder"], qo["FullInputOrder"]}, "Input" -> qb["Extract", pos]],
                With[{iqb = qb["Delete", pos]["Dual"]},
                    QuantumOperator[{"Identity", iqb}, Max[qo["LastOutputQudit"], qo["LastInputQudit"]] + Range @ iqb["Qudits"]]
                ]
            ],
            QuantumOperator[qo, "Input" -> qb["Extract", pos]]
        ]["OrderInputExtra", qo["FullInputOrder"], order],
        "Label" -> qo["Label"]
    ]
]

QuantumOperatorProp[qo_, "OrderedOutput", qb_ ? QuditBasisQ] := qo["OrderedOutput", qo["FullOutputOrder"], qb]

QuantumOperatorProp[qo_, "OrderedOutput", order_ ? orderQ, qb_ ? QuditBasisQ] := Enclose @ Block[{
    arity, pos
},
    ConfirmAssert[ContainsAll[order, qo["FullOutputOrder"]], "Given order should contain all operator order qudits"];
    arity = Length[order];
    pos = Catenate @ Lookup[PositionIndex[order], qo["FullOutputOrder"], Nothing];
    ConfirmAssert[arity <= qb["Qudits"], "Order size should be less than or equal to number of qudits"];
    QuantumOperator[
        If[ arity > qo["OutputQudits"],
            QuantumTensorProduct[
                QuantumOperator[qo, {qo["FullOutputOrder"], qo["InputOrder"]}, "Output" -> qb["Extract", pos]],
                With[{iqb = qb["Delete", pos]},
                    QuantumOperator[{"Identity", iqb}, Max[qo["LastOutputQudit"], qo["LastInputQudit"]] + Range @ iqb["Qudits"]]
                ]
            ],
            QuantumOperator[qo, "Output" -> qb["Extract", pos]]
        ]["OrderOutputExtra", qo["FullOutputOrder"], order],
        "Label" -> qo["Label"]
    ]
]

QuantumOperatorProp[qo_, "OrderedFormula", OptionsPattern[]] /; qo["State"]["DegenerateStateQ"] := 0

QuantumOperatorProp[qo_, "OrderedFormula", OptionsPattern["Normalize" -> False]] := With[{s = qo["State"]["Pure"]},
    With[{
        v = SparseArray @ s[If[TrueQ[OptionValue["Normalize"]], "NormalizedStateVector", "StateVector"]],
        d = s["InputDimension"],
        order = Join @@ qo["Order"]
    },
        Dot[
            Map[
                Replace[qn_QuditName :> With[{names = qn["Name"]},
                    With[{qudits = #["Qudits"] & /@ names},
                        QuditName @@ MapThread[
                            QuditName[Thread[Subscript[Flatten[{#1["Name"]}], OverHat /@ #2]], "Dual" -> #1["DualQ"]] &,
                            {names, TakeList[order, qudits]}
                        ] /; Total[qudits] == Length[order]
                    ] /; MatchQ[names, {__QuditName}]
                ]],
                With[{pos = Catenate @ v["ExplicitPositions"]}, s["Names", Thread[{Quotient[pos - 1, d] + 1, Mod[pos - 1, d] + 1}]]]
            ],
            v["ExplicitValues"]
        ]
    ]
]

QuantumOperatorProp[qo_, "Reorder", order : {_ ? orderQ | Automatic, _ ? orderQ | Automatic}, controlQ_ : False] := Block[{
    output = Replace[order[[1]], Automatic :> qo["FullOutputOrder"]], input = Replace[order[[2]], Automatic :> qo["FullInputOrder"]],
    inputRepl, outputRepl
},
    inputRepl =
        Thread[
            Take[
                If[ TrueQ[controlQ],
                    Join[#, Complement[qo["FullInputOrder"], #]] & @ Join[Sort @ qo["ControlOrder"], qo["TargetOrder"]],
                    Take[qo["FullInputOrder"], UpTo[Length[qo["FullInputOrder"]]]]
                ],
                UpTo[Length[input]]
            ] ->
            Take[input, UpTo[Length[qo["FullInputOrder"]]]]
        ];
    outputRepl =
        Thread[
            Take[qo["FullOutputOrder"], UpTo[Length[output]]] ->
            Take[output, UpTo[Length[qo["FullOutputOrder"]]]]];
    QuantumOperator[
        qo["State"],
        {qo["FullOutputOrder"] /. outputRepl, qo["FullInputOrder"] /. inputRepl},
        "Label" -> ReplaceAll[qo["Label"],
            Subscript["C", name_][c1_, c0_] :> RuleCondition[Subscript["C", name][c1 /. inputRepl, c0 /. inputRepl]]
        ]
    ]
]

QuantumOperatorProp[qo_, "Reorder", order_ ? orderQ, args___] := qo["Reorder", {order, Automatic}, args]


QuantumOperatorProp[qo_, "Shift", n : _Integer : 1] := qo["Reorder", qo["Order"] /. k_Integer :> k + n]



QuantumOperatorProp[qo_, "UnstackOutput", n_Integer : 1] /; 1 <= n <= qo["OutputQudits"] :=
    QuantumOperator[#, {Drop[qo["OutputOrder"], {n}], qo["InputOrder"]}] & /@ qo["State"]["UnstackOutput", n]

QuantumOperatorProp[qo_, "UnstackInput", n_Integer : 1] /; 1 <= n <= qo["InputQudits"] :=
    QuantumOperator[#, {qo["OutputOrder"], Drop[qo["InputOrder"], {n}]}] & /@ qo["State"]["UnstackInput", n]


QuantumOperatorProp[qo_, "HermitianQ"] := HermitianMatrixQ[qo["Matrix"]]

QuantumOperatorProp[qo_, "UnitaryQ"] := UnitaryMatrixQ[N[qo["Matrix"]]]

QuantumOperatorProp[qo_, "Eigenvalues", opts___] /; qo["SquareQ"] := eigenvalues[qo["MatrixRepresentation"], opts, "Sort" -> False, "Normalize" -> True]

QuantumOperatorProp[qo_, "Eigenvectors", opts___] /; qo["SquareQ"] := eigenvectors[qo["MatrixRepresentation"], opts, "Sort" -> False, "Normalize" -> True]

QuantumOperatorProp[qo_, "Eigensystem", opts___] /; qo["SquareQ"] := eigensystem[qo["MatrixRepresentation"], opts, "Sort" -> False, "Normalize" -> True]

QuantumOperatorProp[qo_, "Eigenbasis", opts___] /; qo["SquareQ"] := With[{vecs = qo["Eigenvectors", opts, "Sort" -> False]},
    QuantumBasis[AssociationThread[Symbol["\[FormalV]" <> ToString[#]] & /@ Range[Length[vectors]], vectors]]
]

QuantumOperatorProp[qo_, "SplitBasis"] := With[{
    output = Thread[{qo["FullOutputOrder"], qo["Output"]["Decompose"]}],
    input = Thread[{qo["FullInputOrder"], qo["Input"]["Decompose"]}]
},
    {
        QuantumBasis[
            "Output" -> QuantumTensorProduct @ Select[output, NonPositive[#[[1]]] &][[All, 2]],
            "Input" -> QuantumTensorProduct @ Select[input, NonPositive[#[[1]]] &][[All, 2]]
        ],
        QuantumBasis[
            "Output" -> QuantumTensorProduct @ Select[output, Positive[#[[1]]] &][[All, 2]],
            "Input" -> QuantumTensorProduct @ Select[input, Positive[#[[1]]] &][[All, 2]]
        ]
    }
]

QuantumOperatorProp[qo_, "Projectors", opts___] /; qo["SquareQ"] := projector /@ SparseArray @ Chop @ qo["Eigenvectors", opts]

QuantumOperatorProp[qo_, "Diagonalize", opts___] /; qo["SquareQ"] := Block[{vectors, values},
    {values, vectors} = qo["Eigensystem", opts, "Sort" -> True];
    QuantumOperator[
        DiagonalMatrix[values],
        Take[#, UpTo[1]] & /@ qo["Order"],
        QuantumBasis[AssociationThread[Subscript["s", #] & /@ Range[Length[values]], vectors], qo["Basis"]["Options"]]
    ]
]

QuantumOperatorProp[qo_, "Transpose"] := With[{qudits = Min[qo["OutputQudits"], qo["InputQudits"]]},
    qo["Transpose", Thread[{Take[qo["OutputOrder"], qudits], Take[qo["InputOrder"], qudits]}]]
]

QuantumOperatorProp[qo_, "Transpose", order : {(List | Rule)[_Integer, _Integer]...}] := Block[{
    outputMap = MapAt[1, Rule @@@ order, {All, 2}],
    inputMap = MapAt[0, Rule @@@ Reverse /@ order, {All, 2}],
    map, out, in, qudits
},
    map = Join[Replace[qo["FullOutputOrder"], Append[outputMap, o_ :> 0[o]], {1}], Replace[qo["FullInputOrder"], Append[inputMap, i_ :> 1[i]], {1}]];
    {out, in} = {Cases[map, 0[_]], Cases[map, 1[_]]};
    qudits = Join[
        Replace[Keys[outputMap], Append[qo["OutputOrderQuditMapping"], _ -> Nothing], {1}],
        Replace[Keys[inputMap], Append[qo["InputOrderQuditMapping"], _ -> Nothing], {1}] + qo["OutputQudits"]
    ];
    QuantumOperator[
        QuantumState[qo["State"], qo["Basis"]]["Permute", FindPermutation[map, Join[out, in]]],
        Map[First, {out, in}, {2}]
    ]
]


QuantumOperatorProp[qo_, prop : "Dagger" | "ConjugateTranspose" | "Inverse"] := simplifyLabel @ QuantumOperator[
    qo["State"][prop], {qo["InputOrder"], qo["OutputOrder"]}]

QuantumOperatorProp[qo_, prop : "Conjugate" | "Dual"] := QuantumOperator[qo["State"][prop], qo["Order"]]

QuantumOperatorProp[qo_, "Bend", shift : _Integer ? Positive : Automatic] :=
    simplifyLabel @ If[ qo["MatrixQ"],
        QuantumOperator[qo["State"]["Bend"], Join[#, # + Replace[shift, Automatic :> Max[qo["Order"]] - Min[#] + 1]] & /@ qo["Order"]],
        QuantumTensorProduct[qo, qo["Conjugate"]["Shift", Replace[shift, Automatic :> Max[qo["Order"]]]]]
    ]

QuantumOperatorProp[qo_, "Unbend"] :=
    If[ qo["MatrixQ"],
        qo,
        QuantumOperator[qo["Sort"]["State"]["Unbend"], Take[Sort[#], Length[#] / 2] & /@ qo["Order"]]
    ]

QuantumOperatorProp[qo_, "Double"] := With[{state = qo["State"]["Double"]},
    QuantumOperator[
        QuantumState[
            state,
            QuantumBasis[
                "Output" -> QuantumTensorProduct[Times @@@ Partition[state["Output"]["Decompose"], 2]],
                "Input" -> QuantumTensorProduct[Times @@@ Partition[state["Input"]["Decompose"], 2]],
                "Label" -> With[{label = Replace[qo["Label"], Interpretation[_, label_] :> label]}, Interpretation[Style[label, Bold], label]],
                state["Basis"]["Options"]
            ]
        ],
        qo["Order"]
    ]
]

QuantumOperatorProp[qo_, "Undouble"] := Enclose @ QuantumOperator[
    QuantumState[qo["State"], QuantumBasis[
        Splice[{#, #}] & /@ ConfirmBy[Sqrt[qo["OutputDimensions"]], AllTrue[IntegerQ]],
        Splice[{#, #}] & /@ ConfirmBy[Sqrt[qo["InputDimensions"]], AllTrue[IntegerQ]],
        "Label" -> Replace[qo["Label"], Interpretation[_, label_] :> label]
    ]]["Undouble"],
    qo["Order"]
]

QuantumOperatorProp[qo_, "TensorReverseInput", order_ ? orderQ] :=
    QuantumOperator[qo["State"]["TensorReverseInput", order /. qo["InputOrderQuditMapping"]], qo["Order"]]

QuantumOperatorProp[qo_, "TensorReverseOutput", order_ ? orderQ] :=
    QuantumOperator[qo["State"]["TensorReverseOutput", order /. qo["OutputOrderQuditMapping"]], qo["Order"]]

QuantumOperatorProp[qo_, "Tr" | "Trace" | "TraceNorm" | "Norm"] := Tr[qo["Matrix"]]

QuantumOperatorProp[qo_, "Normalize"] := qo / Abs[qo["Norm"]]

QuantumOperatorProp[qo_, "PrimeBasis", outputQudit : _Integer | Automatic : Automatic, inputQudit : _Integer | Automatic : Automatic, opts___] :=
With[{state = qo["State"]["PrimeBasis"]},
    QuantumOperator[state, {
        If[ outputQudit === Automatic,
            # - Min[#, 1] + 1 &[Max[qo["OutputOrder"]] - Reverse @ Range[state["OutputQudits"]] + 1],
            outputQudit + Range[state["OutputQudits"]] - 1
        ],
        Which[
            inputQudit === Automatic && outputQudit === Automatic,
            # - Min[#, 1] + 1 &[Max[qo["InputOrder"]] - Reverse @ Range[state["InputQudits"]] + 1],
            inputQudit === Automatic && outputQudit =!= Automatic,
            outputQudit + Range[state["InputQudits"]] - 1,
            True,
            inputQudit + Range[state["InputQudits"]] - 1
        ]
    },
    opts,
    "Label" -> qo["Label"]
    ]
]

QuantumOperatorProp[qo_, prop : "Simplify" | "FullSimplify" | "Chop" | "ComplexExpand", args___] := QuantumOperator[qo["State"][prop, args], qo["Order"]]


(* evolution *)

QuantumOperatorProp[qo_, "EvolutionOperator", args___] := QuantumEvolve[qo, None, args]

QuantumOperatorProp[qo_, "NEvolutionOperator", args___] /; qo["ParameterArity"] == 1 := Module[{
    parameter = First @ qo["Parameters"],
    parameterSpec = First @ qo["ParameterSpec"],
    initialParameter = First @ qo["InitialParameters"],
    leftEquations, rightEquations, initialEquations, equations
},
        leftEquations = Flatten[
            (1 / I) qo["MatrixRepresentation"] . Partition[
                    Subscript["u", #][parameter] & /@ Range[qo["OutputDimension"] ^ 2],
                    qo["OutputDimension"]
                ]
        ];
        rightEquations = Subscript["u", #]'[parameter] & /@ Range[qo["OutputDimension"] ^ 2];
        equations = Equal @@@ Transpose[{rightEquations, leftEquations}];
        initialEquations = Equal @@@
            Transpose @ {
                Subscript["u", #][initialParameter] & /@ Range[qo["OutputDimension"] ^ 2],
                Flatten[IdentityMatrix[qo["OutputDimension"]]]
            };
        QuantumOperator[
            Partition[Flatten @
                NDSolveValue[
                    Join[equations, initialEquations],
                    Subscript["u", #][parameter] & /@ Range[qo["OutputDimension"] ^ 2],
                    Evaluate @ parameterSpec,
                    args
                ],
                qo["OutputDimension"]
            ],
            qo["Order"],
            "ParameterSpec" -> First @ qo["ParameterSpec"]
        ]
    ]

QuantumOperatorProp[qo_, "EigenvaluePlot", args___] /; qo["ParameterArity"] == 1 :=
    ReImPlot[Evaluate @ qo["Eigenvalues"], Evaluate @ First @ qo["ParameterSpec"],
        args
    ]


QuantumOperatorProp[qo_, "PauliDecompose"] /; qo["InputQudits"] == qo["OutputQudits"] && MatchQ[qo["Dimensions"], {2 ..}] := With[{
    n = qo["InputQudits"],
    mat = {{0, 0, 1 / 2, 1 / 2}, {1 / 2, I / 2, 0, 0}, {1 / 2, - I / 2, 0, 0}, {0, 0, - 1 / 2, 1 / 2}}
},
    KeyMap[StringJoin @ Replace[#, {1 -> "X", 2 -> "Y", 3 -> "Z", 4 -> "I"}, {1}] &] @ Association @ Most @ ArrayRules[
        Nest[
            Transpose[# . mat, RotateRight[Range[n]]] &,
            Flatten[ArrayReshape[qo["Matrix"], ConstantArray[2, 2 n]], Array[{#, # + n} &, n]],
            n
        ]
    ]
]

QuantumOperatorProp[qo_, "PauliDecompose"] /; qo["InputDimensions"] == qo["OutputDimensions"] := Enclose @ Block[{
    dims = qo["InputDimensions"],
    targetValues = Flatten[qo["MatrixRepresentation"]],
    paulis, ops, params, values
},
	paulis = Tuples[{"I", "X", "Y", "Z"}, Length[dims]];
	ops = Map[MapIndexed[QuantumOperator, Thread[{#, dims}]] &, paulis];
	params = \[FormalT] @@@ paulis;
	values = Flatten @ Total @ MapThread[#1 kroneckerProduct @@ (#["Matrix"] & /@ #2) &, {params, ops}];
	DeleteCases[AssociationThread[ops, First @ ConfirmMatch[SolveValues[Thread[values == targetValues], params], {__}]], 0]
]


UnitaryEulerAngles[a_, b_, c_, d_] := With[{theta = Simplify[2 ArcSin[Abs[b]]]},
    If[NumericQ[theta] && theta == 0, {0, 0, Simplify[Mod[Arg[d], 2 Pi]]}, {theta, Simplify[Mod[Arg[c], 2 Pi]], Simplify[Mod[Arg[b] - Pi, 2 Pi]]}]
]
UnitaryEulerAngles[u_ ? SquareMatrixQ] /; Dimensions[u] === {2, 2} := UnitaryEulerAngles[u[[1, 1]], u[[1, 2]] , u[[2, 1]], u[[2, 2]]]

UnitaryEulerAngles[qo_QuantumOperator] := UnitaryEulerAngles[qo["MatrixRepresentation"]]

UnitaryEulerAnglesWithPhase[u_ ? SquareMatrixQ] /; Dimensions[u] === {2, 2} := With[{a = u[[1, 1]], b = u[[1, 2]], c = u[[2, 1]], d = u[[2, 2]]},
    With[{phase = Simplify[Arg[a]], theta = Simplify[2 ArcSin[Abs[b]]]},
        {If[NumericQ[theta] && theta == 0, {0, 0, Simplify[Mod[Arg[d] - phase, 2 Pi]]}, {theta, Simplify[Mod[Arg[c] - phase, 2 Pi]], Simplify[Mod[Arg[b] - Pi - phase, 2 Pi]]}], phase}
    ]
]

UnitaryEulerAnglesWithPhase[mat_ ? MatrixQ] /; Times @@ Dimensions[mat] == 4 := UnitaryEulerAnglesWithPhase[ArrayReshape[mat, {2, 2}]]

UnitaryEulerAnglesWithPhase[qo_QuantumOperator] := UnitaryEulerAnglesWithPhase[qo["MatrixRepresentation"]]


QuantumOperatorProp[qo_, "ZYZ"] /; qo["VectorQ"] && qo["Dimension"] == 4 := Enclose @ Module[{angles, phase},
    {angles, phase} = UnitaryEulerAnglesWithPhase[qo["MatrixRepresentation"]];
    If[ NumericQ[phase] && phase == 0,
        QuantumOperator[{"U", Splice @ angles}, qo["Order"]],
        QuantumCircuitOperator[{QuantumOperator[{"GlobalPhase", phase}], QuantumOperator[{"U", Splice @ angles}, qo["Order"]]}]
    ]
]

QuantumOperatorProp[qo_, "SimpleQASM"] /; qo["Dimensions"] === {2, 2} := Enclose @ With[{
    angles = ToLowerCase @ ToString[NumberForm[N[#]]] & /@
        ConfirmBy[UnitaryEulerAngles[qo["MatrixRepresentation"]], AllTrue[NumericQ]]
},
    StringTemplate["U(``, ``, ``)"][Sequence @@ angles]
]

QuantumOperatorProp[qo_, "SimpleQASM"] :=
    Replace[qo["Label"], {
        "SWAP" | "\[Pi]"[_, _] :> "swap",
        Superscript[label_, CircleTimes[_]] :> QuantumOperator[label]["SimpleQASM"],
        label_ :> StringTemplate["// Unimplimented QASM for operator with label: `` ----"][label]
    }]

QuantumOperatorProp[qo_, "TargetOperator"] := Module[{control1, control0, n, m},
    control1 = qo["ControlOrder1"];
    control0 = qo["ControlOrder0"];
    n = Length[control1];
    m = Length[control0];
    If[n + m == 0, Return[qo]];
    QuantumOperator[
        QuantumTensorProduct[
            QuantumOperator[QuantumState[{"Register", n, 2 ^ n - 1}]["Dagger"], {{}, control1}],
            QuantumOperator[QuantumState[{"Register", m, 0}]["Dagger"], {{}, control0}]
        ] @ qo @
        QuantumTensorProduct[
            QuantumOperator[QuantumState[{"Register", n, 2 ^ n - 1}], {control1, {}}],
            QuantumOperator[QuantumState[{"Register", m, 0}], {control0, {}}]
        ],
        "Label" -> Replace[qo["Label"], Subscript["C", label_][__] :> label]
    ]
]

QuantumOperatorProp[qo_, "QASM"] /; qo["ControlOrder"] =!= {} && MatchQ[qo["TargetOrder"], {_}] :=
    Replace[qo["Label"], {
        Subscript["C", _][control1_, control0_] :>
            With[{n = Length[control1], m = Length[control0]},
                StringTemplate["ctrl(``) @ negctrl(``) @ "][n, m] <>
                (
                QuantumTensorProduct[{
                    If[ n > 0,
                        QuantumOperator[QuantumState[{"Register", n, 2 ^ n - 1}]["Dagger"], {{}, control1}],
                        Nothing
                    ],
                    If[ m > 0,
                        QuantumOperator[QuantumState[{"Register", m, 0}]["Dagger"], {{}, control0}],
                        Nothing
                    ]
                }] @ qo @
                QuantumTensorProduct[{
                    If[ n > 0,
                        QuantumOperator[QuantumState[{"Register", n, 2 ^ n - 1}], {control1, {}}],
                        Nothing
                    ],
                    If[ m > 0,
                        QuantumOperator[QuantumState[{"Register", m, 0}], {control0, {}}],
                        Nothing
                    ]
                }]
                )["SimpleQASM"] <> " " <> StringRiffle[Map[StringTemplate["q[``]"], Join[control1, control0, qo["TargetOrder"]] - 1], " "] <> ";"
            ],
        _ :> $Failed
        }
    ]

QuantumOperatorProp[qo_, "QASM"] /; MatchQ[qo["Dimensions"], {2 ..}] :=
    qo["SimpleQASM"] <> " " <> StringRiffle[Map[StringTemplate["q[``]"], qo["InputOrder"] - 1], " "] <> ";"


QuantumOperatorProp[qo_, "CircuitDiagram", opts___] := QuantumCircuitOperator[qo]["Diagram", opts]


(* state properties *)

QuantumOperatorProp[qo_, args : PatternSequence[prop_String, ___] | PatternSequence[{prop_String, ___}, ___]] /;
    MemberQ[Intersection[qo["State"]["Properties"], qo["Properties"]], prop] := qo["State"][args]

