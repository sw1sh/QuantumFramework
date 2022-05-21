Package["Wolfram`QuantumFramework`"]

PackageScope["QuantumFrameworkOperatorQ"]
PackageScope["$QuantumFrameworkPropCache"]
PackageScope["CacheProperty"]



QuantumFrameworkOperatorQ[op_] := QuantumOperatorQ[Unevaluated @ op] ||
    QuantumMeasurementOperatorQ[Unevaluated @ op] ||
    QuantumChannelQ[Unevaluated @ op] ||
    QuantumCircuitOperatorQ[Unevaluated @ op]

$QuantumFrameworkPropCache = True

FrontEndExecute[FE`systemQ[FE`s_] := StringMatchQ[Quiet[Check[Context[FE`s], ""]], "System`" | "Wolfram`QuantumFramework`"]]

