Get["Wolfram`QuantumFramework`"]

$AutoCompletionData = Block[{$ContextPath},
    AppendTo[$ContextPath, "Wolfram`QuantumFramework`PackageScope`"];
    With[{
        basisNames = $QuditBasisNames,
        stateNames = $QuantumStateNames,
        operatorNames = $QuantumOperatorNames,
        measurementOperatorNames = $QuantumMeasurementOperatorNames,
        channelNames = $QuantumChannelNames,
        circuitNames = $QuantumCircuitOperatorNames,

        entanglementMonotones = $QuantumEntanglementMonotones,
        distances = $QuantumDistances
    },
        {
            "QuditBasis" -> {basisNames},
            "QuantumBasis" -> {basisNames},
            "QuantumState" -> {stateNames, basisNames},
            "QuantumOperator" -> {operatorNames, basisNames},
            "QuantumMeasurementOperator" -> {Join[basisNames, measurementOperatorNames], basisNames},
            "QuantumChannel" -> {channelNames, basisNames},
            "QuantumCircuitOperator" -> {circuitNames},
            "QuantumEntanglementMonotone" -> {0, entanglementMonotones},
            "QuantumDistance" -> {0, 0, distances}
        }
    ]
]

$path = FileNameJoin @ {DirectoryName[$InputFileName], "AutoCompletionData"}

If[ !FileExistsQ[$path],
    CreateDirectory[$path, CreateIntermediateDirectories -> True]
]

Put[ResourceFunction["ReadableForm"] @ $AutoCompletionData, FileNameJoin[{$path, "specialArgFunctions.tr"}]]

