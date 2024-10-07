
pacletInstalledQ[paclet_, version_] := AnyTrue[Through[PacletFind[paclet]["Version"]], ResourceFunction["VersionOrder"][#, version] <= 0 &]

If[ ! pacletInstalledQ["ServiceConnection_IBMQ", "0.0.4"],
    PacletInstall[PacletObject["Wolfram/QuantumFramework"]["AssetLocation", "IBMQ.paclet"]]
]

If[ ! pacletInstalledQ["Cotengra", "0.1"],
    PacletInstall[PacletObject["Wolfram/QuantumFramework"]["AssetLocation", "Cotengra.paclet"]]
]

$ContextAliases["H`"] = "WolframInstitute`Hypergraph`"

PacletManager`Package`loadWolframLanguageCode[
    "Wolfram/QuantumFramework",
    "Wolfram`QuantumFramework`",
    ParentDirectory[DirectoryName[$InputFileName]],
    "Kernel/QuantumFramework.m",
    "AutoUpdate" -> False,
    "AutoloadSymbols" -> {
        "Wolfram`QuantumFramework`QuditName",
        "Wolfram`QuantumFramework`QuditBasis",
        "Wolfram`QuantumFramework`QuantumBasis",
        "Wolfram`QuantumFramework`QuantumState",
        "Wolfram`QuantumFramework`QuantumOperator",
        "Wolfram`QuantumFramework`QuantumMeasurementOperator",
        "Wolfram`QuantumFramework`QuantumCircuitOperator",
        "Wolfram`QuantumFramework`QuantumMeasurement",
        "Wolfram`QuantumFramework`QuantumTensorProduct",
        "Wolfram`QuantumFramework`QuantumPartialTrace",
        "Wolfram`QuantumFramework`QuantumDistance",
        "Wolfram`QuantumFramework`QuantumSimilarity",
        "Wolfram`QuantumFramework`QuantumEntanglementMonotone",
        "Wolfram`QuantumFramework`QuantumEntangledQ",
        "Wolfram`QuantumFramework`QuantumWignerTransform",
        "Wolfram`QuantumFramework`QuantumChannel"
    },
    "HiddenImports" -> {},
    "SymbolsToProtect" -> {}
];

Wolfram`QuantumFramework`PackageScope`Memoize[Wolfram`QuantumFramework`PackageScope`FromOperatorShorthand];


