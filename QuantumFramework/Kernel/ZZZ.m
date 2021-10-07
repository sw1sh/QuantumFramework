Package["Wolfram`QuantumFramework`"]



If[ $Notebooks,
    With[{
        basisNames = $QuditBasisNames,
        stateNames = $QuantumStateNames,
        operatorNames = $QuantumOperatorNames,
        measurementOperatorNames = $QuantumMeasurementOperatorNames
    },
        Scan[
            ResourceFunction["AddCodeCompletion"][First[#]] @@ Last[#] &, {
                "QuditBasis" -> {basisNames},
                "QuantumBasis" -> {basisNames},
                "QuantumState" -> {stateNames, basisNames},
                "QuantumOperator" -> {operatorNames, basisNames},
                "QuantumMeasurementOperator" -> {measurementOperatorNames, basisNames}
            }
        ]
    ]
]

