Package["Wolfram`QuantumFramework`"]

PackageScope["QuantumFrameworkOperatorQ"]
PackageScope["$QuantumFrameworkPropCache"]


QuantumFrameworkOperatorQ[op_] := QuantumOperatorQ[op] ||
    QuantumMeasurementOperatorQ[op] ||
    QuantumChannelQ[op] ||
    QuantumCircuitOperatorQ[op]

$QuantumFrameworkPropCache = True

FrontEndExecute[FE`systemQ[FE`s_] := StringMatchQ[Quiet[Check[Context[FE`s], ""]], "System`" | "Wolfram`QuantumFramework`"]]

