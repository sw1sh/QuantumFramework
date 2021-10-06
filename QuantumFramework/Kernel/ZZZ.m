Package["Wolfram`QuantumFramework`"]



If[ $Notebooks,
    With[{
        basisNames = $QuditBasisNames,
        stateNames = $QuantumStateNames,
        operatorNames = $QuantumOperatorNames,
        measurementOperatorNames = $QuantumMeasurementOperatorNames
    },
        FE`Evaluate[FEPrivate`AddSpecialArgCompletion[
            "QuditBasis" -> {basisNames},
            "QuantumBasis" -> {basisNames},
            "QuantumState" -> {stateNames, basisNames},
            "QuantumOperator" -> {operatorNames, basisNames},
            "QuantumMeasurementOperator" -> {measurementOperatorNames, basisNames}]]
    ]
]

