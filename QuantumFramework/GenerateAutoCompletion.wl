Needs["Wolfram`QuantumFramework`"]

$AutoCompletionData = With[{
    basisNames = Wolfram`QuantumFramework`PackageScope`$QuditBasisNames,
    stateNames = Wolfram`QuantumFramework`PackageScope`$QuantumStateNames,
    operatorNames = Wolfram`QuantumFramework`PackageScope`$QuantumOperatorNames,
    measurementOperatorNames = Wolfram`QuantumFramework`PackageScope`$QuantumMeasurementOperatorNames,
    channelNames =  Wolfram`QuantumFramework`PackageScope`$QuantumChannelNames,
    circuitNames = Wolfram`QuantumFramework`PackageScope`$QuantumCircuitOperatorNames
},
    {
        "QuditBasis" -> {basisNames},
        "QuantumBasis" -> {basisNames},
        "QuantumState" -> {stateNames, basisNames},
        "QuantumOperator" -> {operatorNames, basisNames},
        "QuantumMeasurementOperator" -> {Join[basisNames, measurementOperatorNames], basisNames},
        "QuantumChannel" -> {channelNames, basisNames},
        "QuantumCircuitOperator" -> {circuitNames}
    }
]

$path = FileNameJoin @ {DirectoryName[$InputFileName], "AutoCompletionData"}

If[ !FileExistsQ[$path],
    CreateDirectory[$path, CreateIntermediateDirectories -> True]
]

Put[ResourceFunction["ReadableForm"] @ $AutoCompletionData, FileNameJoin[{$path, "specialArgFunctions.tr"}]]

