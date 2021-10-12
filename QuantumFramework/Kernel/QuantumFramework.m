Package["Wolfram`QuantumFramework`"]

PackageScope["QuantumFrameworkOperatorQ"]
PackageScope["$QuantumFrameworkPropCache"]


QuantumFrameworkOperatorQ[op_] := QuantumOperatorQ[op] || QuantumMeasurementOperatorQ[op] || QuantumCircuitOperatorQ[op]

$QuantumFrameworkPropCache = True

