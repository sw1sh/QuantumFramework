
pacletInstalledQ[paclet_, version_] := AnyTrue[Through[PacletFind[paclet]["Version"]], ResourceFunction["VersionOrder"][#, version] <= 0 &]

If[ ! pacletInstalledQ["ServiceConnection_IBMQ", "0.0.4"],
    PacletInstall[PacletObject["Wolfram/QuantumFramework"]["AssetLocation", "IBMQ.paclet"]]
]

If[ ! pacletInstalledQ["Cotengra", "0.1"],
    PacletInstall[PacletObject["Wolfram/QuantumFramework"]["AssetLocation", "Cotengra.paclet"]]
]

$ContextAliases["H`"] = "WolframInstitute`Hypergraph`"

ClearAll["Wolfram`QuantumFramework`*", "Wolfram`QuantumFramework`**`*"]

PacletManager`Package`loadWolframLanguageCode[
    "Wolfram/QuantumFramework",
    "Wolfram`QuantumFramework`",
    ParentDirectory[DirectoryName[$InputFileName]],
    "Kernel/QuantumFramework.m",
    "AutoUpdate" -> False,
    "AutoloadSymbols" -> {},
    "HiddenImports" -> {},
    "SymbolsToProtect" -> {}
];

Wolfram`QuantumFramework`PackageScope`Memoize[Wolfram`QuantumFramework`PackageScope`FromOperatorShorthand];


