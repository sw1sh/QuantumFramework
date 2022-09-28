Package["Wolfram`QuantumFramework`"]

PackageScope["UnitaryEulerAngles"]
PackageScope["UnitaryEulerAnglesWithPhase"]



$QuantumOperatorProperties = {
    "Order",
    "InputOrder", "OutputOrder", "ControlOrder", "TargetOrder",
    "MatrixRepresentation", "Matrix",
    "TensorRepresentation", "Tensor",
    "Ordered", "OrderedInput", "OrderedOutput",
    "SortInput", "SortOutput", "Sort",
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
    "Computational",
    "Dagger", "Dual",
    "TraceNorm"
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
    If[TrueQ[$QuantumFrameworkPropCache], QuantumOperatorProp[qo, prop, args] = result, result] /;
        (!FailureQ[Unevaluated @ result] || Message[QuantumOperator::failprop, prop, result]) &&
        (!MatchQ[result, _QuantumOperatorProp] || Message[QuantumOperator::undefprop, prop])
]


(* computed properties *)

QuantumOperatorProp[qo_, "Arity"] := Length @ qo["InputOrder"]

QuantumOperatorProp[qo_, "Range"] := Max[qo["InputOrder"]] - Min[qo["InputOrder"]] + 1

QuantumOperatorProp[qo_, "MaxArity"] := Max[qo["InputQudits"], qo["Arity"]]

QuantumOperatorProp[qo_, "FullArity"] := Max[qo["InputQudits"], qo["Range"]]

QuantumOperatorProp[qo_, "FullInputOrder"] := If[qo["InputDimension"] > 1,
    Take[
        If[MatchQ[qo["InputOrder"], {___, _ ? NonPositive, ___}], Identity, # - Min[#, 1] + 1 &] @
            Join[Complement[Range[Max[qo["InputOrder"]] - qo["InputQudits"] + 1, Max[qo["InputOrder"]]], qo["InputOrder"]], qo["InputOrder"]],
        - qo["InputQudits"]
    ],
    {}
]

QuantumOperatorProp[qo_, "FullOutputOrder"] := If[qo["OutputDimension"] > 1,
    Take[
        If[MatchQ[qo["OutputOrder"], {___, _ ? NonPositive, ___}], Identity, # - Min[#, 1] + 1 &] @
            Join[Complement[Range[Max[qo["OutputOrder"]] - qo["OutputQudits"] + 1, Max[qo["OutputOrder"]]], qo["OutputOrder"]], qo["OutputOrder"]],
        - qo["OutputQudits"]
    ],
    {}
]

QuantumOperatorProp[qo_, "FullOrder"] := {qo["FullOutputOrder"], qo["FullInputOrder"]}

QuantumOperatorProp[qo_, "SetFullOutputOrder"] := QuantumOperator[qo, {qo["FullOutputOrder"], qo["InputOrder"]}]

QuantumOperatorProp[qo_, "SetFullInputOrder"] := QuantumOperator[qo, {qo["OutputOrder"], qo["FullInputOrder"]}]

QuantumOperatorProp[qo_, "SetFullOrder"] := QuantumOperator[qo, {qo["FullOutputOrder"], qo["FullInputOrder"]}]


QuantumOperatorProp[qo_, "ControlOrder1"] :=
    FirstCase[qo["Label"], "Controlled"[_, control1_, ___] :> control1, {}, {0}]

QuantumOperatorProp[qo_, "ControlOrder0"] :=
    FirstCase[qo["Label"], "Controlled"[_, _, control0] :> control0, {}, {0}]

QuantumOperatorProp[qo_, "ControlOrder"] :=
    FirstCase[qo["Label"], "Controlled"[_, control1_, control0_ : {}] :> Join[control1, control0], {}, {0}]

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


QuantumOperatorProp[qo_, "SquareQ"] := qo["OutputDimension"] == qo["InputDimension"]

QuantumOperatorProp[qo_, "Tensor"] := qo["Sort"]["StateTensor"]

QuantumOperatorProp[qo_, "TensorRepresentation"] := qo["Sort"]["State"]["TensorRepresentation"]


QuantumOperatorProp[qo_, "Matrix"] := qo["Sort"]["StateMatrix"]

QuantumOperatorProp[qo_, "MatrixRepresentation"] := qo["Computational"]["Matrix"]

QuantumOperatorProp[qo_, "Operator"] := qo["Amplitudes"]

QuantumOperatorProp[qo_, "QuantumOperator"] := qo

QuantumOperatorProp[qo_, name : "Computational" | "SchmidtBasis" | "SpectralBasis"] := QuantumOperator[qo["State"][name], qo["Order"]]


QuantumOperatorProp[qo_, "OrderedMatrix"] := qo["Ordered"]["Matrix"]

QuantumOperatorProp[qo_, "OrderedMatrixRepresentation"] := qo["Ordered"]["MatrixRepresentation"]

QuantumOperatorProp[qo_, "OrderedTensor"] := qo["Ordered"]["Tensor"]

QuantumOperatorProp[qo_, "OrderedTensorRepresentation"] := qo["Ordered"]["TensorRepresentation"]


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
    QuantumOperator[qo[
        "PermuteInput",
        InversePermutation @ FindPermutation[Ordering @ Ordering @ qo["FullInputOrder"]]
    ]["State"],
        {qo["OutputOrder"], Sort @ qo["InputOrder"]}
    ]
]

QuantumOperatorProp[qo_, "SortOutput"] := If[
    OrderedQ[qo["OutputOrder"]],
    qo,
    QuantumOperator[qo[
        "PermuteOutput",
        InversePermutation @ FindPermutation[Ordering @ Ordering @ qo["FullOutputOrder"]]
    ]["State"],
        {Sort @ qo["OutputOrder"], qo["InputOrder"]}
    ]
]

QuantumOperatorProp[qo_, "Sort"] := qo["SortOutput"]["SortInput"]


QuantumOperatorProp[qo_, "ReverseOutput"] := QuantumOperator[qo["State"], {Reverse @ qo["OutputOrder"], qo["InputOrder"]}]

QuantumOperatorProp[qo_, "ReverseInput"] := QuantumOperator[qo["State"], {qo["OutputOrder"], Reverse @ qo["InputOrder"]}]

QuantumOperatorProp[qo_, "Reverse"] := QuantumOperator[qo["State"], Reverse /@ qo["Order"]]


QuantumOperatorProp[qo_, "Ordered" | "OrderedInput"] := qo["OrderedInput", Sort @ qo["FullInputOrder"]]

QuantumOperatorProp[qo_, "OrderedOutput"] := qo["OrderedOutput", Sort @ qo["FullOutputOrder"]]

QuantumOperatorProp[qo_, "OrderedOutput", from_Integer, to_Integer] /; qo["OutputQudits"] == 1 :=
    qo["OrderedOutput", Range[from, to], QuditBasis[qo["Output"], to - from + 1]]

QuantumOperatorProp[qo_, "Ordered" | "OrderedInput", from_Integer, to_Integer] /; qo["InputQudits"] == 1 :=
    qo["OrderedInput", Range[from, to], QuditBasis[qo["Input"], to - from + 1]]

QuantumOperatorProp[qo_, "Ordered", from_Integer, to_Integer, qb_ ? QuditBasisQ] := qo["Ordered", Range[from, to], qb]

QuantumOperatorProp[qo_, "Ordered", order_ ? orderQ, qb_ ? QuditBasisQ] :=
    qo["OrderedInput", order, qb]

QuantumOperatorProp[qo_, "Ordered" | "OrderedInput" | "OrderedOutput", {}, ___] := qo

QuantumOperatorProp[qo_, "OrderedInput", order_ ? orderQ] := Enclose @ With[{
    dimensions = qo["InputDimensions"]
},
    ConfirmAssert[Length[order] <= Length[dimensions], "Not enough input dimensions to order"];
    qo["OrderedInput", order,
        QuditBasis @ Extract[
            dimensions,
            List /@ (order /. qo["InputOrderQuditMapping"])
        ]
    ]
]

QuantumOperatorProp[qo_, "OrderedOutput", order_ ? orderQ] := Enclose @ With[{
    dimensions = qo["OutputDimensions"]
},
    ConfirmAssert[Length[order] <= Length[dimensions], "Not enough output dimensions to order"];
    qo["OrderedOutput", order,
        QuditBasis @ Extract[
            dimensions,
            List /@ (order /. qo["OutputOrderQuditMapping"])
        ]
    ]
]

QuantumOperatorProp[qo_, "OrderedInput", qb_ ? QuditBasisQ] := qo["OrderedInput", qo["FullInputOrder"], qb]

QuantumOperatorProp[qo_, "OrderedInput", order_ ? orderQ, qb_ ? QuditBasisQ] := Enclose @ With[{
    arity = Length[order], pos = Catenate @ Position[order, Alternatives @@ qo["FullInputOrder"]]
},
    ConfirmAssert[ContainsAll[order, qo["FullInputOrder"]], "Given order should contain all operator order qudits"];
    ConfirmAssert[arity <= qb["Qudits"], "Order size should be less than or equal to number of qudits"];
    If[ arity > qo["InputQudits"],
        QuantumTensorProduct[
            QuantumOperator[qo, {qo["OutputOrder"], qo["FullInputOrder"]}, "Input" -> qb["Extract", pos]],
            With[{iqb = qb["Delete", pos]},
                QuantumOperator[{"Identity", iqb}, Max[qo["LastOutputQudit"], qo["LastInputQudit"]] + Range @ iqb["Qudits"]]
            ]
        ],
        QuantumOperator[qo, "Input" -> qb["Extract", pos]]
    ]["OrderInputExtra", qo["FullInputOrder"], order]
]

QuantumOperatorProp[qo_, "OrderedOutput", qb_ ? QuditBasisQ] := qo["OrderedOutput", qo["FullOutputOrder"], qb]

QuantumOperatorProp[qo_, "OrderedOutput", order_ ? orderQ, qb_ ? QuditBasisQ] := Enclose @ With[{
    arity = Length[order], pos = Catenate @ Position[order, Alternatives @@ qo["FullOutputOrder"]]
},
    ConfirmAssert[ContainsAll[order, qo["FullOutputOrder"]], "Given order should contain all operator order qudits"];
    ConfirmAssert[arity <= qb["Qudits"], "Order size should be less than or equal to number of qudits"];
    If[ arity > qo["OutputQudits"],
        QuantumTensorProduct[
            QuantumOperator[qo, {qo["FullOutputOrder"], qo["InputOrder"]}, "Output" -> qb["Extract", pos]],
            With[{iqb = qb["Delete", pos]},
                QuantumOperator[{"Identity", iqb}, Max[qo["LastOutputQudit"], qo["LastInputQudit"]] + Range @ iqb["Qudits"]]
            ]
        ],
        QuantumOperator[qo, "Output" -> qb["Extract", pos]]
    ]["OrderOutputExtra", qo["FullOutputOrder"], order]
]


QuantumOperatorProp[qo_, "Shift", n : _Integer ? NonNegative : 1] := QuantumOperator[qo, qo["Order"] /. k_Integer ? Positive :> k + n]


QuantumOperatorProp[qo_, "UnstackOutput", n_Integer : 1] /; 1 <= n <= qo["OutputQudits"] :=
    QuantumOperator[#, {Drop[qo["OutputOrder"], {n}], qo["InputOrder"]}] & /@ qo["State"]["UnstackOutput", n]

QuantumOperatorProp[qo_, "UnstackInput", n_Integer : 1] /; 1 <= n <= qo["InputQudits"] :=
    QuantumOperator[#, {qo["OutputOrder"], Drop[qo["InputOrder"], {n}]}] & /@ qo["State"]["UnstackInput", n]


QuantumOperatorProp[qo_, "HermitianQ"] := HermitianMatrixQ[qo["Matrix"]]

QuantumOperatorProp[qo_, "UnitaryQ"] := UnitaryMatrixQ[qo["Matrix"]]

QuantumOperatorProp[qo_, "Eigenvalues"] /; qo["SquareQ"] := Eigenvalues[qo["MatrixRepresentation"]]

QuantumOperatorProp[qo_, "Eigenvectors", opts___] /; qo["SquareQ"] := eigenvectors[qo["MatrixRepresentation"], opts, "Sort" -> False, "Normalize" -> True]

QuantumOperatorProp[qo_, "Eigensystem", opts___] /; qo["SquareQ"] := eigensystem[qo["MatrixRepresentation"], opts, "Sort" -> False, "Normalize" -> True]

QuantumOperatorProp[qo_, "Projectors", opts___] := projector /@ SparseArray @ Chop @ qo["Eigenvectors", opts]

QuantumOperatorProp[qo_, "Transpose"] := QuantumOperator[
    qo["State"]["Transpose"], {qo["InputOrder"], qo["OutputOrder"]}]


simplifyLabel[op_QuantumOperator] := QuantumOperator[op, "Label" -> simplifyLabel[op["Label"]]]

simplifyLabel[l_] := Replace[l, {
    SuperDagger[label : "X" | "Y" | "Z" | "NOT" | "H" | "SWAP"] :> label,
    SuperDagger["Controlled"[x_, rest__]] :> "Controlled"[simplifyLabel[x], rest],
    SuperDagger[(r : Subscript["R", _] | "P")[angle_]] :> r[- angle],
    SuperDagger["PhaseShift"[n_] | n_Integer] :> "PhaseShift"[-n],
    SuperDagger["U2"[a_, b_]] :> "U2"[Pi - a, Pi - b],
    SuperDagger[SuperDagger[label_]] :> label
}]

QuantumOperatorProp[qo_, "Dagger" | "ConjugateTranspose"] := simplifyLabel @ QuantumOperator[
    qo["State"]["Dagger"], {qo["InputOrder"], qo["OutputOrder"]}]

QuantumOperatorProp[qo_, prop : "Conjugate" | "Dual"] := QuantumOperator[qo["State"][prop], qo["Order"]]

QuantumOperatorProp[qo_, "TensorReverseInput", order_ ? orderQ] :=
    QuantumOperator[qo["State"]["TensorReverseInput", order /. qo["InputOrderQuditMapping"]], qo["Order"]]

QuantumOperatorProp[qo_, "TensorReverseOutput", order_ ? orderQ] :=
    QuantumOperator[qo["State"]["TensorReverseOutput", order /. qo["OutputOrderQuditMapping"]], qo["Order"]]

QuantumOperatorProp[qo_, "TraceNorm"] := qo["Unbend"]["TraceNorm"]

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

QuantumOperatorProp[qo_, "Simplify"] := QuantumOperator[qo["State"]["Simplify"], qo["Order"]]


(* evolution *)

QuantumOperatorProp[qo_, "EvolutionOperator", args___] /; qo["ParameterArity"] == 1 && qo["InputDimension"] == qo["OutputDimension"] :=
With[{
    parameter = First @ qo["Parameters"]
},
    QuantumOperator[
        If[ FreeQ[Normal[qo["Matrix"]], parameter],
            MatrixExp[-I parameter qo["Matrix"]],
            DSolveValue[
                {
                    \[FormalU]'[parameter] == 1 / I TrigToExp[qo["Matrix"]] . \[FormalU][parameter],
                    \[FormalU][0] == IdentityMatrix[qo["InputDimension"]]
                },
                \[FormalU][parameter] \[Element] Matrices[qo["MatrixNameDimensions"]],
                parameter,
                args
            ]
        ],
        qo["Order"],
        qo["Basis"],
        "ParameterSpec" -> First @ qo["ParameterSpec"]
    ]
]

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


UnitaryEulerAngles[b_, c_] := FullSimplify /@ {2 ArcSin[Abs[b]], Mod[Arg[c], 2 Pi], Mod[Arg[b] - Pi, 2 Pi]}
UnitaryEulerAngles[u_ ? SquareMatrixQ] /; Dimensions[u] === {2, 2} := UnitaryEulerAngles[u[[1, 2]] , u[[2, 1]]]

UnitaryEulerAngles[qo_QuantumOperator] := UnitaryEulerAngles[qo["MatrixRepresentation"]]

UnitaryEulerAnglesWithPhase[u_ ? SquareMatrixQ] /; Dimensions[u] === {2, 2} := With[{b = u[[1, 2]], c = u[[2, 1]]},
    With[{phase = Arg[u[[1, 1]]]}, {{2 ArcSin[Abs[b]], Mod[Arg[c] - phase, 2 Pi], Mod[Arg[b] - Pi - phase, 2 Pi]}, phase}]
]

UnitaryEulerAnglesWithPhase[qo_QuantumOperator] := UnitaryEulerAnglesWithPhase[qo["MatrixRepresentation"]]


QuantumOperatorProp[qo_, "ZYZ"] /; qo["Dimensions"] === {2, 2} := Enclose @ Module[{angles, phase},
    {angles, phase} = UnitaryEulerAnglesWithPhase[qo["MatrixRepresentation"]];
    ConfirmAssert[NumericQ[phase] && AllTrue[angles, NumericQ]];
    If[ phase == 0,
        QuantumOperator[{"U", Splice @ angles}],
        QuantumCircuitOperator[{QuantumOperator[{"GlobalPhase", phase}], QuantumOperator[{"U", Splice @ angles}]}]
    ]
]

QuantumOperatorProp[qo_, "SimpleQASM"] /; qo["Dimensions"] === {2, 2} := Enclose @ With[{
    angles = StringReplace[ToLowerCase @ ToString[#, InputForm], {"Pi" -> "pi", "I" -> "im"}] & /@
        ConfirmBy[UnitaryEulerAngles[qo["MatrixRepresentation"]], AllTrue[NumericQ]]
},
    StringTemplate["U(``, ``, ``)"][Sequence @@ angles]
]

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
        "Label" -> Replace[qo["Label"], "Controlled"[label_, ___] :> label]
    ]
]

QuantumOperatorProp[qo_, "QASM"] /; qo["Dimensions"] === {2, 2} :=
    qo["SimpleQASM"] <> " " <> StringRiffle[Map[StringTemplate["q[``]"], qo["InputOrder"] - 1], " "] <> ";"

QuantumOperatorProp[qo_, "QASM"] /; qo["ControlOrder"] =!= {} && MatchQ[qo["TargetOrder"], {_}] :=
    Replace[qo["Label"], {
        "Controlled"[_, control1_, control0_] :>
            With[{n = Length[control1], m = Length[control0]},
                StringTemplate["ctrl(``) @ negctrl(``) @ "][n, m] <>
                (
                QuantumTensorProduct[
                    QuantumOperator[QuantumState[{"Register", n, 2 ^ n - 1}]["Dagger"], {{}, control1}],
                    QuantumOperator[QuantumState[{"Register", m, 0}]["Dagger"], {{}, control0}]
                ] @ qo @
                QuantumTensorProduct[
                    QuantumOperator[QuantumState[{"Register", n, 2 ^ n - 1}], {control1, {}}],
                    QuantumOperator[QuantumState[{"Register", m, 0}], {control0, {}}]
                ]
                )["SimpleQASM"] <> " " <> StringRiffle[Map[StringTemplate["q[``]"], Join[control1, control0, qo["TargetOrder"]] - 1], " "] <> ";"
            ],
        _ :> $Failed
        }
    ]


(* state properties *)

QuantumOperatorProp[qo_, args : PatternSequence[prop_String, ___] | PatternSequence[{prop_String, ___}, ___]] /;
    MemberQ[Intersection[qo["State"]["Properties"], qo["Properties"]], prop] := qo["State"][args]

