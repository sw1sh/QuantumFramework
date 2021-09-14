Package["QuantumFramework`"]

PackageExport["QuantumMeasurementOperator"]

PackageScope["QuantumMeasurementOperatorQ"]



QuantumMeasurementOperatorQ[QuantumMeasurementOperator[op_]] := QuantumDiscreteOperatorQ[op]

QuantumMeasurementOperatorQ[___] := False


(* constructors *)

QuantumMeasurementOperator[args : PatternSequence[Except[_ ? QuantumDiscreteOperatorQ], ___]] :=
    Enclose @ QuantumMeasurementOperator[ConfirmBy[QuantumDiscreteOperator[args], QuantumDiscreteOperatorQ]]


(* composition *)

(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[qds_ ? QuantumDiscreteStateQ] := Module[{
    state
},
    state = qmo["DiscreteOperator"][qds];
    QuantumMeasurement[AssociationThread[state["BasisElementNames"], state["Eigenvalues"]], state["PureStates"]]
]

(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[qdo_ ? QuantumDiscreteOperatorQ] :=
    QuantumMeasurementOperator[qmo["DiscreteOperator"][qdo]]

