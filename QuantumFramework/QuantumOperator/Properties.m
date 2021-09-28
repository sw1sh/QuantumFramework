Package["QuantumFramework`"]

PackageScope["QuantumOperatorProp"]



$QuantumOperatorProperties = {
    "MatrixRepresentation", "Matrix",
    "TensorRepresentation", "Tensor",
    "Ordered",
    "OrderedMatrixRepresentation", "OrderedMatrix",
    "OrderedTensorRepresentation", "OrderedTensor",
    "Arity", "MaxArity", "Order", "TotalOrder",
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


QuantumOperator::undefprop = "QuantumOperator property `` is undefined for this state";


(* basic getters *)

QuantumOperatorProp[QuantumOperator[state_, _], "State"] := state

QuantumOperatorProp[QuantumOperator[_, order_], "Order"] := order


(qo_QuantumOperator[prop_ ? propQ, args___]) /; QuantumOperatorQ[qo] := With[{
    result = QuantumOperatorProp[qo, prop, args]
    },
    (QuantumOperatorProp[qo, prop, args] = result) /; !FailureQ[Unevaluated @ result] &&
        (!MatchQ[result, _QuantumOperatorProp] || Message[QuantumOperator::undefprop, prop])
]


(* computed properties *)

QuantumOperatorProp[qo_, "Arity"] := Length @ qo["Order"]

QuantumOperatorProp[qo_, "MaxArity"] := Max[qo["InputQudits"], Max[qo["Order"]]]

QuantumOperatorProp[qo_, "TotalOrder"] := Join[qo["Order"], Complement[Range[qo["MaxArity"]], qo["Order"]]]


QuantumOperatorProp[qo_, "Tensor"] := qo["StateTensor"]

QuantumOperatorProp[qo_, "TensorRepresentation"] := qo["State"]["TensorRepresentation"]


QuantumOperatorProp[qo_, "Matrix"] := qo["StateMatrix"]

QuantumOperatorProp[qo_, "MatrixRepresentation"] := qo["Computational"]["Matrix"]

QuantumOperatorProp[qo_, "Operator"] := qo["Amplitudes"]

QuantumOperatorProp[qo_, "QuantumOperator"] := qo

QuantumOperatorProp[qo_, "Computational"] := QuantumOperator[qo["State"]["Computational"], qo["Order"]]


QuantumOperatorProp[qo_, prop : "OrderedMatrixRepresentation" | "OrderedMatrix"] := qo[{prop, qo["MaxArity"]}]

QuantumOperatorProp[qo_, prop : "OrderedTensorRepresentation" | "OrderedTensor"] := qo[{prop, qo["MaxArity"]}]

QuantumOperatorProp[qo_, {"OrderedMatrix", arity_Integer}] :=
    qo[{"Ordered", arity}]["Matrix"]

QuantumOperatorProp[qo_, {"OrderedMatrixRepresentation", arity_Integer}] :=
    qo[{"Ordered", arity}]["MatrixRepresentation"]

QuantumOperatorProp[qo_, {"OrderedTensor", arity_Integer}] :=
    qo[{"Ordered", arity}]["Tensor"]

QuantumOperatorProp[qo_, {"OrderedTensorRepresentation", arity_Integer}] :=
    qo[{"Ordered", arity}]["TensorRepresentation"]


QuantumOperatorProp[qo_, {"PermuteInput", perm_Cycles}] := QuantumOperator[
    QuantumState[QuantumState[
        Flatten @ Transpose[
            qo["Tensor"],
            FindPermutation[Join[Range @ qo["OutputQudits"], qo["OutputQudits"] + PermutationList[perm, qo["InputQudits"]]]]
        ],
        qo["Basis"]
        ],
        QuantumBasis[qo["Basis"], "Input" -> qo["Input"][{"Permute", perm}]]
    ],
    qo["Order"]
]

QuantumOperatorProp[qo_, {"PermuteOutput", perm_Cycles}] := QuantumOperator[
    QuantumState[QuantumState[
        Flatten @ Transpose[
            qo["Tensor"],
            FindPermutation[Join[PermutationList[perm, qo["OutputQudits"]], qo["OutputQudits"] + Range @ qo["InputQudits"]]]
        ],
        qo["Basis"]
        ],
        QuantumBasis[qo["Basis"], "Output" -> qo["Output"][{"Permute", perm}]]
    ],
    qo["Order"]
]

QuantumOperatorProp[qo_, {"Permute", perm_Cycles}] := qo[{"PermuteOutput", perm}][{"PermuteInput", perm}]


QuantumOperatorProp[qo_, "Ordered"] := qo[{"Ordered", qo["MaxArity"]}]

QuantumOperatorProp[qo_, {"Ordered", qudits_Integer}] := If[qo["InputDimension"] <= 1, qo,
    With[{arity = Max[qo["MaxArity"], qudits]},
        If[ arity > qo["InputQudits"],
            QuantumOperator[
                QuantumTensorProduct[
                    qo,
                    Sequence @@ Table[QuantumOperator[{"Identity", Last[qo["InputDimensions"]]}], arity - qo["InputQudits"]]
                ],
                Range[arity]
            ],
            qo
        ][{
            "Permute", FindPermutation[Join[qo["Order"], Complement[Range[arity], qo["Order"]]]]
        }]
    ]
]


QuantumOperatorProp[qo_, "HermitianQ"] := HermitianMatrixQ[qo["Matrix"]]

QuantumOperatorProp[qo_, "UnitaryQ"] := UnitaryMatrixQ[qo["Matrix"]]


QuantumOperatorProp[qo_, "Eigenvalues"] := Eigenvalues[qo["Matrix"]]

QuantumOperatorProp[qo_, "Eigenvectors"] := Normalize /@ Eigenvectors[qo["Matrix"]]

QuantumOperatorProp[qo_, "Projectors"] := projector @* Normalize /@ qo["Eigenvectors"]

QuantumOperatorProp[qo_, "Dagger" | "ConjugateTranspose"] := QuantumOperator[ConjugateTranspose[qo["Matrix"]], qo["Basis"]["Dagger"], qo["Order"]]

QuantumOperatorProp[qo_, "Dual"] := QuantumOperator[Conjugate[qo["Matrix"]], qo["Basis"]["Dual"], qo["Order"]]


(* state properties *)

QuantumOperatorProp[qo_, args : PatternSequence[prop_String, ___] | PatternSequence[{prop_String, ___}, ___]] /;
    MemberQ[Intersection[qo["State"]["Properties"], qo["Properties"]], prop] := qo["State"][args]

