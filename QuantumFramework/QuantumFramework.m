Package["QuantumFramework`"]

PackageScope["QuantumFrameworkOperatorQ"]



QuantumFrameworkOperatorQ[op_] := QuantumOperatorQ[op] || QuantumMeasurementOperatorQ[op] || QuantumCircuitOperatorQ[op]

