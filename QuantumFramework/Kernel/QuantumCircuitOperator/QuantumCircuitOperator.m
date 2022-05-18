Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumCircuitOperator"]

PackageScope["QuantumCircuitOperatorQ"]



QuantumCircuitOperatorQ[QuantumCircuitOperator[operators_]] := VectorQ[operators, QuantumFrameworkOperatorQ]

QuantumCircuitOperatorQ[___] := False


QuantumCircuitOperator[operators_, args__] := QuantumCircuitOperator[MapAt[Head[#][#, args] &, operators, 1]]

QuantumCircuitOperator[op_ ? QuantumFrameworkOperatorQ] := QuantumCircuitOperator[{op}]

(qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ)[op_ ? QuantumCircuitOperatorQ] :=
    QuantumCircuitOperator[Join[op["Operators"], qco["Operators"]]]

(qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ)[op_ ? QuantumFrameworkOperatorQ] :=
    QuantumCircuitOperator[Prepend[qco["Operators"], op]]

(qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ)[qs_ ? QuantumStateQ] := Fold[ReverseApplied[Construct], qs, qco["Operators"]]

op_QuantumMeasurementOperator[qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ] :=
    QuantumCircuitOperator[Append[qco["Operators"], op]]

