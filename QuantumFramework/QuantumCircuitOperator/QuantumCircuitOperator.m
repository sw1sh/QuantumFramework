Package["QuantumFramework`"]

PackageExport["QuantumCircuitOperator"]

PackageScope["QuantumCircuitOperatorQ"]



QuantumCircuitOperatorQ[QuantumCircuitOperator[operators_]] := VectorQ[operators, QuantumFrameworkOperatorQ]

QuantumCircuitOperatorQ[___] := False


QuantumCircuitOperator[operators_, args__] := QuantumCircuitOperator[MapAt[Head[#][#, args] &, operators, 1]]


(qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ)[arg_ ? QuantumOperatorQ] := Head[arg][qco["CircuitOperator"][arg]]

