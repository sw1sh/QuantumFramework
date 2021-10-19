Get["Wolfram`QuantumFramework`"]

$ContextPath = Prepend[$ContextPath, "Wolfram`QuantumFramework`PackageScope`"]

$AutoCompletionData = With[{
    basisNames = $QuditBasisNames,
    stateNames = $QuantumStateNames,
    operatorNames = $QuantumOperatorNames,
    measurementOperatorNames = Join[
        $QuditBasisNames,
        $QuantumMeasurementOperatorNames
    ]
},
    {
        "QuditBasis" -> {basisNames},
        "QuantumBasis" -> {basisNames},
        "QuantumState" -> {stateNames, basisNames},
        "QuantumOperator" -> {operatorNames, basisNames},
        "QuantumMeasurementOperator" -> {measurementOperatorNames, basisNames}
    }
]

$path = FileNameJoin @ {DirectoryName[$InputFileName], "AutoCompletionData"}

If[ !FileExistsQ[$path],
    CreateDirectory[$path, CreateIntermediateDirectories -> True]
]

Put[ResourceFunction["ReadableForm"] @ $AutoCompletionData, FileNameJoin[{$path, "specialArgFunctions.tr"}]]

