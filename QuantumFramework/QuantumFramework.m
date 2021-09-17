Package["QuantumFramework`"]

PackageScope["QuantumOperatorQ"]



QuantumOperatorQ[op_] := QuantumDiscreteOperatorQ[op] || QuantumMeasurementOperatorQ[op] || QuantumCircuitOperatorQ[op]

