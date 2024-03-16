Package["Wolfram`QuantumFramework`"]

PackageScope["QuantumFrameworkOperatorQ"]
PackageScope["$QuantumFrameworkPropCache"]

PackageImport["DocumentationSearch`"]



QuantumFrameworkOperatorQ[op_] := QuantumOperatorQ[Unevaluated @ op] ||
    QuantumMeasurementOperatorQ[Unevaluated @ op] ||
    QuantumChannelQ[Unevaluated @ op] ||
    QuantumCircuitOperatorQ[Unevaluated @ op]

$QuantumFrameworkPropCache = True

If[ $FrontEnd =!= Null,
    FrontEndExecute[FE`systemQ[FE`s_] := StringMatchQ[Quiet[Check[Context[FE`s], ""]], "System`" | "Wolfram`QuantumFramework`"]];

    Scan[
        FE`Evaluate[FEPrivate`AddSpecialArgCompletion[#]] &,
        Import[FileNameJoin[{PacletObject["Wolfram/QuantumFramework"]["Location"], "AutoCompletionData", "specialArgFunctions.tr"}], "WL"]
    ]
]

