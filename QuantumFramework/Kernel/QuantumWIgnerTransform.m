Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumWignerTransform"]



Options[QuantumWignerTransform] = {"Exact" -> True}

QuantumWignerTransform[qb_ ? QuditBasisQ, opts : OptionsPattern[]] := QuditBasis[{"Wigner", qb, opts}]


QuantumWignerTransform[qb_ ? QuantumBasisQ, opts : OptionsPattern[]] /; qb["Picture"] =!= "PhaseSpace" :=
    QuantumBasis[QuantumWignerTransform[qb["Output"], opts], QuantumWignerTransform[qb["Input"], opts], "PhaseSpace"]


QuantumWignerTransform[qs_ ? QuantumStateQ, opts : OptionsPattern[]] /; qs["Picture"] =!= "PhaseSpace" :=
    QuantumState[qs["Bend"], QuantumWignerTransform[qs["Basis"], opts]]


QuantumWignerTransform[qo_ ? QuantumOperatorQ, opts : OptionsPattern[]] /; qo["Picture"] =!= "PhaseSpace" :=
    QuantumOperator[QuantumWignerTransform[qo["State"], opts], qo["Order"]]


QuantumWignerTransform[qmo_ ? QuantumMeasurementOperatorQ, opts : OptionsPattern[]] /; qmo["Picture"] =!= "PhaseSpace" :=
    QuantumMeasurementOperator[QuantumWignerTransform[qmo["Operator"], opts], qmo["Target"]]

