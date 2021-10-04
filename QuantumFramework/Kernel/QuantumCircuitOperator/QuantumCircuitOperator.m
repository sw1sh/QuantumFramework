Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumCircuitOperator"]

PackageScope["QuantumCircuitOperatorQ"]



QuantumCircuitOperatorQ[QuantumCircuitOperator[operators_]] := VectorQ[operators, QuantumFrameworkOperatorQ]

QuantumCircuitOperatorQ[___] := False


QuantumCircuitOperator[operators_, args__] := QuantumCircuitOperator[MapAt[Head[#][#, args] &, operators, 1]]

QuantumCircuitOperator[op_ ? QuantumFrameworkOperatorQ] := QuantumCircuitOperator[{op}]


(qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ)[arg_ ? QuantumOperatorQ] := Head[arg][qco["CircuitOperator"][arg]]

(qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ)[qs_ ? QuantumStateQ] := qco["CircuitOperator"][qs]

