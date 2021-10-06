Package["Wolfram`QuantumFramework`"]

PackageScope["QuantumOperatorProp"]



$QuantumOperatorProperties = {
    "InputOrder", "QuditOrder",
    "MatrixRepresentation", "Matrix",
    "TensorRepresentation", "Tensor",
    "Ordered",
    "OrderedMatrixRepresentation", "OrderedMatrix",
    "OrderedTensorRepresentation", "OrderedTensor",
    "Arity", "MaxArity", "Range", "Order", "QuditOrder", "InputOrder", "CircuitOrder", "InputQuditOrder", "OutputOrder",
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

QuantumOperatorProp[QuantumOperator[_, order_], "Order"] := order


(qo_QuantumOperator[prop_ ? propQ, args___]) /; QuantumOperatorQ[qo] := With[{
    result = QuantumOperatorProp[qo, prop, args]
    },
    (QuantumOperatorProp[qo, prop, args] = result) /; (!FailureQ[Unevaluated @ result] || Message[QuantumOperator::failprop, prop, result]) &&
        (!MatchQ[result, _QuantumOperatorProp] || Message[QuantumOperator::undefprop, prop])
]


(* computed properties *)

QuantumOperatorProp[qo_, "Arity"] := Length @ qo["Order"]

QuantumOperatorProp[qo_, "Range"] := Max[qo["Order"]] - Min[qo["Order"]] + 1

QuantumOperatorProp[qo_, "MaxArity"] := Max[qo["InputQudits"], qo["Range"]]

QuantumOperatorProp[qo_, "QuditOrder"] := Range[qo["MaxArity"]]

QuantumOperatorProp[qo_, "InputOrder"] := qo["QuditOrder"] + Min[qo["Order"]] - 1

QuantumOperatorProp[qo_, "CircuitOrder"] :=  qo["QuditOrder"] + Max[Max[qo["Order"]] - qo["InputQudits"], 0]

QuantumOperatorProp[qo_, "InputQuditOrder"] := qo["Order"] - Min[qo["InputOrder"]] + 1

QuantumOperatorProp[qo_, "OutputOrder"] := Range[Max[qo["InputOrder"]] - qo["OutputQudits"] + 1, Max[qo["InputOrder"]]]

QuantumOperatorProp[qo_, "Tensor"] := qo["StateTensor"]

QuantumOperatorProp[qo_, "TensorRepresentation"] := qo["State"]["TensorRepresentation"]


QuantumOperatorProp[qo_, "Matrix"] := qo["StateMatrix"]

QuantumOperatorProp[qo_, "MatrixRepresentation"] := qo["Computational"]["Matrix"]

QuantumOperatorProp[qo_, "Operator"] := qo["Amplitudes"]

QuantumOperatorProp[qo_, "QuantumOperator"] := qo

QuantumOperatorProp[qo_, name : "Computational" | "SchmidtBasis" | "SpectralBasis"] := QuantumOperator[qo["State"][name], qo["Order"]]


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
    qo["State"][{"PermuteInput", perm}],
    Permute[qo["InputOrder"], perm]
]

QuantumOperatorProp[qo_, {"PermuteOutput", perm_Cycles}] := QuantumOperator[
    qo["State"][{"PermuteOutput", perm}],
    qo["InputOrder"]
]

QuantumOperatorProp[qo_, {"Permute", perm_Cycles}] := With[{shift = Max[qo["OutputQudits"] - qo["InputQudits"], 0]},
qo[
    {"PermuteOutput", InversePermutation @ FindPermutation[Join[Range[shift], shift + PermutationList[perm, qo["InputQudits"]]]]}
][
    {"PermuteInput", perm}
]
]


QuantumOperatorProp[qo_, "Ordered"] := qo[{"Ordered", Splice @ MinMax[qo["InputOrder"]]}]

QuantumOperatorProp[qo_, {"Ordered", arity_Integer}] := qo[{"Ordered", Min[qo["Order"]], Min[qo["Order"]] + arity - 1}]

QuantumOperatorProp[qo_, {"Ordered", from_Integer, to_Integer}] := Enclose @ (

ConfirmAssert[AllTrue[qo["Order"], Between[{from, to}]], "Range should include operator's order"];

If[qo["InputDimension"] <= 1, qo,
    With[{arity = to - from + 1},
        QuantumOperator[If[ arity > qo["InputQudits"],
            QuantumOperator[
                QuantumTensorProduct[
                    qo,
                    QuantumOperator[{"Identity", Table[Last[qo["InputDimensions"]], arity - qo["InputQudits"]]}]
                    (*Sequence @@ Table[QuantumOperator[{"Identity", Last[qo["InputDimensions"]]}, {i + Max[qo["InputOrder"]]}], {i, arity - qo["InputQudits"]}]
                *)],
                Range[from, to]
            ],
            qo
        ][{
            "Permute",
            InversePermutation @ FindPermutation[Join[qo["Order"], Complement[Range[from, to], qo["Order"]]]]
        }],
        Range[from, to]
        ]
    ]
]
)


QuantumOperatorProp[qo_, "HermitianQ"] := HermitianMatrixQ[qo["Matrix"]]

QuantumOperatorProp[qo_, "UnitaryQ"] := UnitaryMatrixQ[qo["Matrix"]]

QuantumOperatorProp[qo_, "Eigenvalues"] := Eigenvalues[qo["MatrixRepresentation"]]

QuantumOperatorProp[qo_, "Eigenvectors"] := eigenvectors[qo["MatrixRepresentation"], "Sort" -> False]

QuantumOperatorProp[qo_, "Projectors"] := projector /@ qo["Eigenvectors"]

QuantumOperatorProp[qo_, "Dagger" | "ConjugateTranspose"] := QuantumOperator[
    ConjugateTranspose[qo["Matrix"]], qo["Basis"]["Dagger"], Take[qo["Order"], UpTo[qo["OutputQudits"]]]]

QuantumOperatorProp[qo_, "Dual"] := QuantumOperator[Conjugate[qo["Matrix"]], qo["Basis"]["Dual"], qo["Order"]]


(* state properties *)

QuantumOperatorProp[qo_, args : PatternSequence[prop_String, ___] | PatternSequence[{prop_String, ___}, ___]] /;
    MemberQ[Intersection[qo["State"]["Properties"], qo["Properties"]], prop] := qo["State"][args]

