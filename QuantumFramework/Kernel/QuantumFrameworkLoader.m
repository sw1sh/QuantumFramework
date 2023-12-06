If[ PacletFind["ServiceConnection_IBMQ"] === {},
    PacletInstall[PacletObject["Wolfram/QuantumFramework"]["AssetLocation", "IBMQ.paclet"]]
]

If[ PacletFind["Cotengra"] === {},
    PacletInstall[PacletObject["Wolfram/QuantumFramework"]["AssetLocation", "Cotengra.paclet"]]
]

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
        "Wolfram`QuantumFramework`QuantumEntanglementMonotone",
        "Wolfram`QuantumFramework`QuantumEntangledQ",
        "Wolfram`QuantumFramework`QuantumWignerTransform",
        "Wolfram`QuantumFramework`QuantumChannel"
    },
    "HiddenImports" -> {}
];