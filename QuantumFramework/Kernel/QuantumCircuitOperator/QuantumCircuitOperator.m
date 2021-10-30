Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumCircuitOperator"]

PackageScope["QuantumCircuitOperatorQ"]



QuantumCircuitOperatorQ[QuantumCircuitOperator[operators_]] := VectorQ[operators, QuantumFrameworkOperatorQ]

QuantumCircuitOperatorQ[___] := False


QuantumCircuitOperator[operators_, args__] := QuantumCircuitOperator[MapAt[Head[#][#, args] &, operators, 1]]

QuantumCircuitOperator[op_ ? QuantumFrameworkOperatorQ] := QuantumCircuitOperator[{op}]


(qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ)[op_ ? QuantumFrameworkOperatorQ] :=
    QuantumCircuitOperator[Prepend[qco["Operators"], op]]

(qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ)[qs_ ? QuantumStateQ] := qco["CircuitOperator"][qs]

op_QuantumMeasurementOperator[qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ] :=
    QuantumCircuitOperator[Append[qco["Operators"], op]]
