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

QuantumMeasurementOperatorProp[qmo_, "OrderedPOVMElements"] /; qmo["POVMQ"] := qmo["OrderedTensor"]

QuantumMeasurementOperatorProp[qmo_, {"OrderedPOVMElements", arity_Integer}] /; qmo["POVMQ"] := qmo[{"OrderedTensor", arity}]

QuantumMeasurementOperatorProp[qmo_, "Operators"] := If[qmo["POVMQ"],
    AssociationThread[Ket /@ Range[0, Length[qmo["Tensor"]] - 1], QuantumDiscreteOperator[#, QuantumBasis["Output" -> qmo["Basis"]["Input"]], qmo["Order"]] & /@ qmo["Tensor"]],
    AssociationThread[Ket /@ Eigenvalues[qmo["Matrix"]], QuantumDiscreteOperator[projector @ #, qmo["Basis"], qmo["Order"]] & /@ Eigenvectors[qmo["OrderedMatrix"]]]
]


(* operator properties *)

QuantumMeasurementOperatorProp[qmo_, args : PatternSequence[prop_String, ___] | PatternSequence[{prop_String, ___}, ___]] /;
    MemberQ[Intersection[qmo["DiscreteOperator"]["Properties"], qmo["Properties"]], prop] := qmo["DiscreteOperator"][args]

