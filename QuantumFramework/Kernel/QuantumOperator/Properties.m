Package["Wolfram`QuantumFramework`"]

PackageScope["QuantumOperatorProp"]



$QuantumOperatorProperties = {
    "Order",
    "InputOrder", "OutputOrder",
    "MatrixRepresentation", "Matrix",
    "TensorRepresentation", "Tensor",
    "Ordered", "OrderedInput", "OrderedOutput", "SortInput", "SortOutput", "Sort",
    "OrderedMatrixRepresentation", "OrderedMatrix",
    "OrderedTensorRepresentation", "OrderedTensor",
    "Arity", "MaxArity", "FullArity", "Range", "FullInputOrder", "InputQuditOrder", "OutputQuditOrder",
    "FirstOutputQudit", "LastOutputQudit", "FirstInputQudit", "LastInputQudit", "InputOrderQuditMapping",
    "HermitianQ", "UnitaryQ", "Eigenvalues", "Eigenvectors", "Projectors",
    "ConjugateTranspose",
    "QuantumOperator", "Operator",
    "Computational",
    "Dagger", "Dual"
};

QuantumOperator["Properties"] := DeleteDuplicates @ Join[$QuantumOperatorProperties, Complement[QuantumState["Properties"], {
    "BlochCartesianCoordinates", "BlochSphericalCoordinates", "BlochPlot"
}]];

QuantumOperatorProp[qo_, "Properties"] := DeleteDuplicates @ Join[QuantumOperator["Properties"], Complement[qo["State"]["Properties"], {
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
        Join[Complement[Range[Max[qo["InputOrder"]] - qo["InputQudits"] + 1, Max[qo["InputOrder"]]], qo["InputOrder"]], qo["InputOrder"]],
        - qo["InputQudits"]
    ],
    {}
]

QuantumOperatorProp[qo_, "FullOutputOrder"] := If[qo["OutputDimension"] > 1,
    Take[
        Join[Complement[Range[Max[qo["OutputOrder"]] - qo["OutputQudits"] + 1, Max[qo["OutputOrder"]]], qo["OutputOrder"]], qo["OutputOrder"]],
        - qo["OutputQudits"]
    ],
    {}
]

QuantumOperatorProp[qo_, "FirstInputQudit"] := Min @ qo["FullInputOrder"]

QuantumOperatorProp[qo_, "LastInputQudit"] := Max @ qo["FullInputOrder"]

QuantumOperatorProp[qo_, "FirstOutputQudit"] := Min @ qo["FullOutputOrder"]

QuantumOperatorProp[qo_, "LastOutputQudit"] := Max @ qo["FullOutputOrder"]

QuantumOperatorProp[qo_, "InputQuditOrder"] := qo["InputOrder"] - Min[qo["FullInputOrder"]] + 1

QuantumOperatorProp[qo_, "OutputQuditOrder"] := qo["OutputOrder"] - Min[qo["FullOutputOrder"]] + 1

QuantumOperatorProp[qo_, "InputOrderQuditMapping"] := Thread[qo["FullInputOrder"] -> Range[qo["InputQudits"]]]

QuantumOperatorProp[qo_, "OutputOrderQuditMapping"] := Thread[qo["FullOutputOrder"] -> Range[qo["OutputQudits"]]]


QuantumOperatorProp[qo_, "Tensor"] := qo["StateTensor"]

QuantumOperatorProp[qo_, "TensorRepresentation"] := qo["State"]["TensorRepresentation"]


QuantumOperatorProp[qo_, "Matrix"] := qo["StateMatrix"]

QuantumOperatorProp[qo_, "MatrixRepresentation"] := qo["Computational"]["Matrix"]

QuantumOperatorProp[qo_, "Operator"] := qo["Amplitudes"]

QuantumOperatorProp[qo_, "QuantumOperator"] := qo

QuantumOperatorProp[qo_, name : "Computational" | "SchmidtBasis" | "SpectralBasis"] := QuantumOperator[qo["State"][name], qo["Order"]]


QuantumOperatorProp[qo_, "OrderedMatrix"] := qo["Ordered"]["Matrix"]

QuantumOperatorProp[qo_, "OrderedMatrixRepresentation"] := qo["Ordered"]["MatrixRepresentation"]

QuantumOperatorProp[qo_, "OrderedTensor"] := qo["Ordered"]["Tensor"]

QuantumOperatorProp[qo_, "OrderedTensorRepresentation"] := qo["Ordered"]["TensorRepresentation"]


QuantumOperatorProp[qo_, {"PermuteInput", perm_Cycles}] := QuantumOperator[
    qo["State"][{"PermuteInput", perm}],
    qo["Order"]
]

QuantumOperatorProp[qo_, {"PermuteOutput", perm_Cycles}] := QuantumOperator[
    qo["State"][{"PermuteOutput", perm}],
    qo["Order"]
]

QuantumOperatorProp[qo_, {"OrderInputExtra", inputSource_ ? orderQ, inputTarget_ ? orderQ}] := Module[{
    extra = DeleteCases[inputTarget, Alternatives @@ inputSource],
    inputPerm, outputTarget
},
    (* assume extra qudits are on the right *)
    inputPerm = FindPermutation[Join[inputSource, extra], inputTarget];
    outputTarget = qo["FullOutputOrder"] /. Thread[Permute[qo["FullInputOrder"], inputPerm] -> inputTarget];
    QuantumOperator[
        qo[{"PermuteInput", inputPerm}],
        outputTarget,
        Sort @ inputTarget
    ]
]

QuantumOperatorProp[qo_, {"OrderOutputExtra", outputSource_ ? orderQ, outputTarget_ ? orderQ}] := Module[{
    extra = DeleteCases[outputTarget, Alternatives @@ outputSource],
    outputPerm, inputTarget
},
    (* assume extra qudits are on the right *)
    outputPerm = FindPermutation[Join[outputSource, extra], outputTarget];
    inputTarget = qo["FullInputOrder"] /. Thread[Permute[qo["FullOutputOrder"], outputPerm] -> outputTarget];
    QuantumOperator[
        qo[{"PermuteOutput", outputPerm}],
        Sort @ outputTarget,
        inputTarget
    ]
]

QuantumOperatorProp[qo_, "SortInput"] := QuantumOperator[qo[{
    "PermuteInput",
    InversePermutation @ FindPermutation[qo["FullInputOrder"]]
}],
    qo["OutputOrder"],
    Sort @ qo["InputOrder"]
]

QuantumOperatorProp[qo_, "SortOutput"] := QuantumOperator[qo[{
    "PermuteOutput",
    InversePermutation @ FindPermutation[qo["FullOutputOrder"]]
}],
    Sort @ qo["OutputOrder"],
    qo["InputOrder"]
]

QuantumOperatorProp[qo_, "Sort"] := qo["SortOutput"]["SortInput"]


QuantumOperatorProp[qo_, "Ordered" | "OrderedInput"] := qo[{"OrderedInput", Sort @ qo["FullInputOrder"]}]

QuantumOperatorProp[qo_, "OrderedOutput"] := qo[{"OrderedOutput", Sort @ qo["FullOutputOrder"]}]

QuantumOperatorProp[qo_, {"Ordered", from_Integer, to_Integer}] := qo[{"Ordered", Range[from, to]}]

QuantumOperatorProp[qo_, {"Ordered", from_Integer, to_Integer, qb_ ? QuditBasisQ}] := qo[{"Ordered", Range[from, to], qb}]

QuantumOperatorProp[qo_, {"Ordered", order_ ? orderQ, qb_ ? QuditBasisQ}] :=
    qo[{"OrderedInput", order, qb}]

QuantumOperatorProp[qo_, {"Ordered" | "OrderedInput" | "OrderedOutput", {}, ___}] := qo

QuantumOperatorProp[qo_, {"OrderedInput", order_ ? orderQ}] := Enclose @ With[{
    dimensions = qo["InputDimensions"]
},
    ConfirmAssert[Length[order] <= Length[dimensions], "Not enough input dimensions to order"];
    qo[{"OrderedInput", order,
        QuditBasis @ Extract[
            dimensions,
            List /@ (order /. qo["InputOrderQuditMapping"])
        ]}
    ]
]

QuantumOperatorProp[qo_, {"OrderedOutput", order_ ? orderQ}] := Enclose @ With[{
    dimensions = qo["OutputDimensions"]
},
    ConfirmAssert[Length[order] <= Length[dimensions], "Not enough output dimensions to order"];
    qo[{"OrderedOutput", order,
        QuditBasis @ Extract[
            dimensions,
            List /@ (order /. qo["OutputOrderQuditMapping"])
        ]}
    ]
]

QuantumOperatorProp[qo_, {"OrderedInput", qb_ ? QuditBasisQ}] := qo[{"OrderedInput", qo["FullInputOrder"], qb}]

QuantumOperatorProp[qo_, {"OrderedInput", order_ ? orderQ, qb_ ? QuditBasisQ}] := Enclose @ With[{
    arity = Length[order]
},
    ConfirmAssert[ContainsAll[order, qo["FullInputOrder"]], "Given order should contain all operator order qudits"];
    ConfirmAssert[arity <= qb["Qudits"], "Order size should be less than or equal to number of qudits"];
    If[ arity > qo["InputQudits"],
        QuantumTensorProduct[
            qo,
            With[{iqb = qb[{"Delete", Catenate @ Position[order, Alternatives @@ qo["FullInputOrder"]]}]},
                QuantumOperator[
                    QuantumOperator[{"Identity", iqb}],
                    Automatic,
                    Max[qo["LastOutputQudit"], qo["LastInputQudit"]] + Range @ iqb["Qudits"]
                ]
            ]
        ],
        qo
    ][{"OrderInputExtra", qo["FullInputOrder"], order}]
]

QuantumOperatorProp[qo_, {"OrderedOutput", qb_ ? QuditBasisQ}] := qo[{"OrderedOutput", qo["FullOutputOrder"], qb}]

QuantumOperatorProp[qo_, {"OrderedOutput", order_ ? orderQ, qb_ ? QuditBasisQ}] := Enclose @ With[{
    arity = Length[order]
},
    ConfirmAssert[ContainsAll[order, qo["FullOutputOrder"]], "Given order should contain all operator order qudits"];
    ConfirmAssert[arity <= qb["Qudits"], "Order size should be less than or equal to number of qudits"];
    If[ arity > qo["OutputQudits"],
        QuantumTensorProduct[
            qo,
            With[{iqb = qb[{"Delete", Catenate @ Position[order, Alternatives @@ qo["FullOutputOrder"]]}]},
                QuantumOperator[
                    QuantumOperator[{"Identity", iqb}],
                    Max[qo["LastOutputQudit"], qo["LastInputQudit"]] + Range @ iqb["Qudits"],
                    Automatic
                ]
            ]
        ],
        qo
    ][{"OrderOutputExtra", qo["FullOutputOrder"], order}]
]

QuantumOperatorProp[qo_, "Numeric"] := QuantumOperator[qo["State"]["Numeric"], qo["Order"]]


QuantumOperatorProp[qo_, "HermitianQ"] := HermitianMatrixQ[qo["Matrix"]]

QuantumOperatorProp[qo_, "UnitaryQ"] := UnitaryMatrixQ[qo["Matrix"]]

QuantumOperatorProp[qo_, "Eigenvalues"] := Eigenvalues[qo["MatrixRepresentation"]]

QuantumOperatorProp[qo_, "Eigenvectors"] := eigenvectors[qo["MatrixRepresentation"], "Sort" -> False]

QuantumOperatorProp[qo_, "Projectors"] := projector /@ SparseArray @ Chop @ qo["Eigenvectors"]

QuantumOperatorProp[qo_, "Transpose"] := QuantumOperator[
    Transpose[qo["Matrix"]], qo["Basis"]["Transpose"], {qo["InputOrder"], qo["OutputOrder"]}]

QuantumOperatorProp[qo_, "Dagger" | "ConjugateTranspose"] := QuantumOperator[
    ConjugateTranspose[qo["Matrix"]], qo["Basis"]["Dagger"], {qo["InputOrder"], qo["OutputOrder"]}]

QuantumOperatorProp[qo_, "Dual"] := QuantumOperator[QuantumOperator[Conjugate[qo["Matrix"]], qo["Basis"]["Dual"]], qo["Order"]]


(* state properties *)

QuantumOperatorProp[qo_, args : PatternSequence[prop_String, ___] | PatternSequence[{prop_String, ___}, ___]] /;
    MemberQ[Intersection[qo["State"]["Properties"], qo["Properties"]], prop] := qo["State"][args]

