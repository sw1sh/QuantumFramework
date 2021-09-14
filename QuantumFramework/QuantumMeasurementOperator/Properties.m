Package["QuantumFramework`"]

PackageScope["QuantumMeasurementOperatorProp"]



$QuantumMeasurementOperatorProperties = {
    "DiscreteOperator",
    "Operator", "Basis", "MatrixRepresentation", "POVMElements",
    "OrderedMatrixRepresentation", "OrderedPOVMElements",
    "Arity", "Dimensions", "Order", "HermitianQ", "UnitaryQ", "Eigenvalues", "Eigenvectors",
    "ProjectionQ", "POVMQ"
};

$QuantumMeasurementOperatorProperties = DeleteDuplicates @ Join[
    $QuantumMeasurementOperatorProperties,
    QuantumDiscreteOperator["Properties"]
]

QuantumMeasurementOperator["Properties"] := $QuantumMeasurementOperatorProperties

QuantumMeasurementOperatorProp[qmo_, "Properties"] :=
    DeleteDuplicates @ Join[QuantumMeasurementOperator["Properties"], qmo["DiscreteOperator"]["Properties"]]


QuantumMeasurementOperator::undefprop = "QuantumMeasurementOperator property `` is undefined for this operator";


(qmo_QuantumMeasurementOperator[prop_ ? propQ, args___]) /; QuantumMeasurementOperatorQ[qmo] := With[{
    result = QuantumMeasurementOperatorProp[qmo, prop, args]
    },
    (QuantumMeasurementOperatorProp[qmo, prop, args] = result)
        /; !MatchQ[result, _QuantumMeasurementOperatorProp] || Message[QuantumMeasurementOperator::undefprop, prop]
]


(* getters *)

QuantumMeasurementOperatorProp[_[op_], "DiscreteOperator"] := op


QuantumMeasurementOperatorProp[qmo_, "Type"] := Which[
    qmo["InputDimension"] == qmo["OutputDimension"],
    "Projection",
    True,
    "POVM"
]

QuantumMeasurementOperatorProp[qmo_, "ProjectionQ"] := qmo["Type"] === "Projection"

QuantumMeasurementOperatorProp[qmo_, "POVMQ"] := qmo["Type"] === "POVM"


(* operator properties *)

QuantumMeasurementOperatorProp[qmo_, args : PatternSequence[prop_String, ___] | PatternSequence[{prop_String, ___}, ___]] /;
    MemberQ[Intersection[qmo["DiscreteOperator"]["Properties"], qmo["Properties"]], prop] := qmo["DiscreteOperator"][args]

