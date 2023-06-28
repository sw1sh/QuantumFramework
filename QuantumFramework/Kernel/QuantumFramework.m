Package["Wolfram`QuantumFramework`"]

PackageScope["QuantumFrameworkOperatorQ"]
PackageScope["$QuantumFrameworkPropCache"]
PackageScope["CacheProperty"]

PackageImport["DocumentationSearch`"]



QuantumFrameworkOperatorQ[op_] := QuantumOperatorQ[Unevaluated @ op] ||
    QuantumMeasurementOperatorQ[Unevaluated @ op] ||
    QuantumChannelQ[Unevaluated @ op] ||
    QuantumCircuitOperatorQ[Unevaluated @ op]

$QuantumFrameworkPropCache = True

FrontEndExecute[FE`systemQ[FE`s_] := StringMatchQ[Quiet[Check[Context[FE`s], ""]], "System`" | "Wolfram`QuantumFramework`"]]

Scan[
    FE`Evaluate[FEPrivate`AddSpecialArgCompletion[#]] &,
    Import[FileNameJoin[{PacletObject["Wolfram/QuantumFramework"]["Location"], "AutoCompletionData", "specialArgFunctions.tr"}], "WL"]
]

(* Quiet[CreateDocumentationIndex[FileNameJoin[{PacletObject["Wolfram/QuantumFramework"]["Location"], "Documentation", "English"}]]] *)

If[ PacletFind["ServiceConnection_IBMQ"] === {},
    PacletInstall[PacletObject["Wolfram/QuantumFramework"]["AssetLocation", "IBMQ.paclet"]]
]

