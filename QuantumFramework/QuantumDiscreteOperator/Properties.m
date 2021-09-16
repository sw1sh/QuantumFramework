Package["QuantumFramework`"]

PackageScope["QuantumDiscreteOperatorProp"]




$QuantumDiscreteOperatorProperties = {
    "MatrixRepresentation", "Matrix",
    "TensorRepresentation", "Tensor",
    "OrderedMatrixRepresentation", "OrderedMatrix",
    "OrderedTensorRepresentation", "OrderedTensor",
    "Arity", "MaxArity", "Order", "TotalOrder",
    "HermitianQ", "UnitaryQ", "Eigenvalues", "Eigenvectors", "Projectors"
};

$QuantumDiscreteOperatorProperties = DeleteDuplicates @ Join[$QuantumDiscreteOperatorProperties, $QuantumDiscreteStateProperties];


QuantumDiscreteOperator["Properties"] := $QuantumDiscreteOperatorProperties

QuantumDiscreteOperatorProp[qdo_, "Properties"] := DeleteDuplicates @ Join[QuantumDiscreteOperator["Properties"], qdo["State"]["Properties"]]


qdo_QuantumDiscreteOperator["ValidQ"] := QuantumDiscreteOperatorQ[qdo]


QuantumDiscreteOperator::undefprop = "QuantumDiscreteOperator property `` is undefined for this state";


(* basic getters *)

QuantumDiscreteOperatorProp[QuantumDiscreteOperator[state_, _], "State"] := state

QuantumDiscreteOperatorProp[QuantumDiscreteOperator[_, order_], "Order"] := order


(qdo_QuantumDiscreteOperator[prop_ ? propQ, args___]) /; QuantumDiscreteOperatorQ[qdo] := With[{
    result = QuantumDiscreteOperatorProp[qdo, prop, args]
    },
    (QuantumDiscreteOperatorProp[qdo, prop, args] = result) /; !MatchQ[result, _QuantumDiscreteOperatorProp] || Message[QuantumDiscreteOperator::undefprop, prop]
]


(* computed properties *)

QuantumDiscreteOperatorProp[qdo_, "Arity"] := Length @ qdo["Order"]

QuantumDiscreteOperatorProp[qdo_, "MaxArity"] := Max[qdo["InputQudits"], Max[qdo["Order"]]]

QuantumDiscreteOperatorProp[qdo_, "TotalOrder"] := Join[qdo["Order"], Complement[Range[qdo["MaxArity"]], qdo["Order"]]]


QuantumDiscreteOperatorProp[qdo_, "TensorRepresentation" | "Tensor"] := Switch[
    qdo["StateType"],
    "Vector",
    ArrayReshape[qdo["StateVector"], qdo["Dimensions"]],
    "Matrix",
    $Failed
]

QuantumDiscreteOperatorProp[qdo_, "MatrixRepresentation" | "Matrix"] := Switch[
    qdo["StateType"],
    "Vector",
    ArrayReshape[qdo["StateVector"], qdo["MatrixNameDimensions"]],
    "Matrix",
    $Failed
]

QuantumDiscreteOperatorProp[qdo_, "Operator"] := Association @ Thread[qdo["BasisElementNames"] -> Flatten[qdo["MatrixRepresentation"]]]


QuantumDiscreteOperatorProp[qdo_, "OrderedMatrixRepresentation" | "OrderedMatrix"] := qdo[{"OrderedMatrix", qdo["MaxArity"]}]

QuantumDiscreteOperatorProp[qdo_, "OrderedTensorRepresentation" | "OrderedTensor"] := qdo[{"OrderedTensor", qdo["MaxArity"]}]

QuantumDiscreteOperatorProp[qdo_, {"OrderedMatrixRepresentation" | "OrderedMatrix", arity_Integer}] :=
    OrderedMatrixRepresentation[qdo["Matrix"], arity, qdo["Order"]]

QuantumDiscreteOperatorProp[qdo_, {"OrderedTensorRepresentation" | "OrderedTensor", arity_Integer}] :=
    Map[OrderedMatrixRepresentation[#, arity, qdo["Order"]] &, qdo["Tensor"], {TensorRank[qdo["Tensor"]] - 2}]


QuantumDiscreteOperatorProp[qdo_, "HermitianQ"] := HermitianMatrixQ[qdo["MatrixRepresentation"]]

QuantumDiscreteOperatorProp[qdo_, "UnitaryQ"] := UnitaryMatrixQ[qdo["MatrixRepresentation"]]


QuantumDiscreteOperatorProp[qdo_, "Eigenvalues"] := Eigenvalues[qdo["Matrix"]]

QuantumDiscreteOperatorProp[qdo_, "Eigenvectors"] := Eigenvectors[qdo["Matrix"]]

QuantumDiscreteOperatorProp[qdo_, "Projectors"] := projector /@ qdo["Eigenvectors"]


(* state properties *)

QuantumDiscreteOperatorProp[qdo_, args : PatternSequence[prop_String, ___] | PatternSequence[{prop_String, ___}, ___]] /;
    MemberQ[Intersection[qdo["State"]["Properties"], qdo["Properties"]], prop] := qdo["State"][args]

