Package["QuantumFramework`"]

PackageScope["QuantumDiscreteOperatorProp"]




$QuantumDiscreteOperatorProperties = {
    "MatrixRepresentation",
    "OrderedMatrixRepresentation", "Arity", "Order",
    "HermitianQ", "UnitaryQ", "Eigenvalues", "Eigenvectors"
};

$QuantumDiscreteOperatorProperties = DeleteDuplicates @ Join[$QuantumDiscreteOperatorProperties, $QuantumDiscreteStateProperties];


QuantumDiscreteOperator["Properties"] := $QuantumDiscreteOperatorProperties

QuantumDiscreteOperatorProp[qdo_, "Properties"] := Join[QuantumDiscreteOperator["Properties"], qdo["State"]["Properties"]]


qdo_QuantumDiscreteOperator["ValidQ"] := QuantumDiscreteOperatorQ[qdo]


QuantumDiscreteOperator::undefprop = "QuantumDiscreteOperator property `` is undefined for this state";


(* basic getters *)

QuantumDiscreteOperatorProp[QuantumDiscreteOperator[state_, _], "State"] := state

QuantumDiscreteOperatorProp[QuantumDiscreteOperator[_, order_], "Order"] := order


(qdo_QuantumDiscreteOperator[prop_ ? propQ, args___]) /; QuantumDiscreteOperatorQ[qdo] := With[{
    result = QuantumDiscreteOperatorProp[qdo, prop, args]
    },
    result /; !MatchQ[result, _QuantumDiscreteOperatorProp] || Message[QuantumDiscreteOperator::undefprop, prop]
]


(* computed properties *)

QuantumDiscreteOperatorProp[qdo_, "Arity"] := Length @ qdo["Order"]

QuantumDiscreteOperatorProp[qdo_, "Qudits"] := qdo["InputQudits"]

QuantumDiscreteOperatorProp[qdo_, "Dimensions"] := qdo["State"]["Dimensions"][[- qdo["InputQudits"] ;; ]]

QuantumDiscreteOperatorProp[qdo_, "Dimension"] := Sqrt @ qdo["State"]["Dimension"]


QuantumDiscreteOperatorProp[qdo_, "MatrixRepresentation" | "Matrix"] := With[{
    state = qdo["State"]
},
    Switch[
        qdo["StateType"],
        "Vector",
        ArrayReshape[state["StateVector"], {qdo["Dimension"], qdo["Dimension"]}],
        "Matrix",
        $Failed
    ]
]

QuantumDiscreteOperatorProp[qdo_, "Operator"] := Association @ Thread[qdo["BasisElementNames"] -> Flatten[qdo["MatrixRepresentation"]]]


QuantumDiscreteOperatorProp[qdo_, "OrderedMatrixRepresentation"] := qdo[{"OrderedMatrixRepresentation", Max[qdo["Qudits"], Max[qdo["Order"]]]}]

QuantumDiscreteOperatorProp[qdo_, {"OrderedMatrixRepresentation", length_Integer ? Positive}] :=
    OrderedMatrixRepresentation[qdo["MatrixRepresentation"], length, qdo["Order"]]


QuantumDiscreteOperatorProp[qdo_, "HermitianQ"] := HermitianMatrixQ[qdo["MatrixRepresentation"]]

QuantumDiscreteOperatorProp[qdo_, "UnitaryQ"] := UnitaryMatrixQ[qdo["MatrixRepresentation"]]


QuantumDiscreteOperatorProp[qdo_, "Eigenvalues"] := Eigenvalues[qdo["MatrixRepresentation"]]

QuantumDiscreteOperatorProp[qdo_, "Eigenvectors"] := Eigenvectors[qdo["MatrixRepresentation"]]


(* state properties *)

QuantumDiscreteOperatorProp[qdo_, args : PatternSequence[prop_String, ___] | PatternSequence[{prop_String, ___}, ___]] /;
    MemberQ[Intersection[qdo["State"]["Properties"], qdo["Properties"]], prop] := qdo["State"][args]

