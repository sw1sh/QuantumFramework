Package["QuantumFramework`"]

PackageScope["QuantumMeasurementOperatorProp"]



$QuantumMeasurementOperatorProperties = {
    "QuantumOperator",
    "Operator", "Basis", "MatrixRepresentation", "POVMElements",
    "OrderedMatrixRepresentation", "OrderedPOVMElements",
    "Arity", "Dimensions", "Order", "HermitianQ", "UnitaryQ", "Eigenvalues", "Eigenvectors",
    "ProjectionQ", "POVMQ"
};

$QuantumMeasurementOperatorProperties = DeleteDuplicates @ Join[
    $QuantumMeasurementOperatorProperties,
    $QuantumOperatorProperties
]

QuantumMeasurementOperator["Properties"] := $QuantumMeasurementOperatorProperties

qmo_QuantumMeasurementOperator["ValidQ"] := QuantumMeasurementOperatorQ[qmo]


QuantumMeasurementOperator::undefprop = "QuantumMeasurementOperator property `` is undefined for this operator";


(qmo_QuantumMeasurementOperator[prop_ ? propQ, args___]) /; QuantumMeasurementOperatorQ[qmo] := With[{
    result = QuantumMeasurementOperatorProp[qmo, prop, args]
    },
    (QuantumMeasurementOperatorProp[qmo, prop, args] = result)
        /; !FailureQ[Unevaluated @ result] && (!MatchQ[result, _QuantumMeasurementOperatorProp] || Message[QuantumMeasurementOperator::undefprop, prop])
]

QuantumMeasurementOperatorProp[qmo_, "Properties"] :=
    DeleteDuplicates @ Join[QuantumMeasurementOperator["Properties"], qmo["QuantumOperator"]["Properties"]]


(* getters *)

QuantumMeasurementOperatorProp[_[op_], "QuantumOperator"] := op


QuantumMeasurementOperatorProp[qmo_, "Type"] := Which[
    qmo["OutputQudits"] == qmo["InputQudits"],
    "Projection",
    qmo["OutputQudits"] == qmo["InputQudits"] + 1,
    "POVM",
    True,
    "Unknown"
]

QuantumMeasurementOperatorProp[qmo_, "ProjectionQ"] := qmo["Type"] === "Projection"

QuantumMeasurementOperatorProp[qmo_, "POVMQ"] := qmo["Type"] === "POVM"

QuantumMeasurementOperatorProp[qmo_, "POVMElements"] := If[qmo["POVMQ"], qmo["Tensor"], projector /@ qmo["Matrix"]]

QuantumMeasurementOperatorProp[qmo_, "OrderedPOVMElements"] := qmo[{"OrderedPOVMElements", qmo["MaxArity"]}]

QuantumMeasurementOperatorProp[qmo_, {"OrderedPOVMElements", arity_Integer}] := If[qmo["POVMQ"],
    qmo[{"OrderedTensor", arity}],
    projector /@ qmo[{"OrderedMatrix", arity}]
]

QuantumMeasurementOperatorProp[qmo_, "Operators"] := If[qmo["POVMQ"],
    AssociationThread[Range[0, Length[qmo["Tensor"]] - 1], QuantumOperator[#, QuantumBasis["Output" -> qmo["Basis"]["Input"]], qmo["Order"]] & /@ qmo["Tensor"]],
    AssociationThread[Eigenvalues[qmo["Matrix"]], QuantumOperator[projector @ #, qmo["Basis"], qmo["Order"]] & /@ Eigenvectors[qmo["OrderedMatrix"]]]
]


(* operator properties *)

QuantumMeasurementOperatorProp[qmo_, args : PatternSequence[prop_String, ___] | PatternSequence[{prop_String, ___}, ___]] /;
    MemberQ[Intersection[qmo["QuantumOperator"]["Properties"], qmo["Properties"]], prop] := qmo["QuantumOperator"][args]

