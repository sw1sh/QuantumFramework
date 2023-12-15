Package["Wolfram`QuantumFramework`"]

PackageScope["$QuantumMeasurementOperatorNames"]



$QuantumMeasurementOperatorNames = {"RandomHermitian", "WignerMICPOVM"}


QuantumMeasurementOperator[{"RandomHermitian", args___}, opts___] := With[{
    basis = QuantumBasis[args]
},
    QuantumMeasurementOperator[
        QuantumOperator[
            With[{m = RandomComplex[1 + I, {basis["Dimension"], basis["Dimension"]}]}, (m + ConjugateTranspose[m]) / 2],
            basis
        ],
        opts
]
]

QuantumMeasurementOperator[{"WignerMICPOVM", args___}, opts___] := Enclose @ FullSimplify @ QuantumMeasurementOperator[
    ConfirmBy[WignerMICPOVM[args], ArrayQ[#, 3] &],
    opts
]


QuantumMeasurementOperator[name_String | name_String[args___], opts___] /; MemberQ[$QuantumMeasurementOperatorNames, name] := QuantumMeasurementOperator[{name, args}, opts]

