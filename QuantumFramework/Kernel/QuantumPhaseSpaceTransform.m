Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumPhaseSpaceTransform"]



QuantumPhaseSpaceTransform[qb_ ? QuditBasisQ, phaseSpaceBasis_ ? QuditBasisQ] :=
    Enclose @ If[phaseSpaceBasis["Dimension"] == qb["Dimension"] ^ 2, phaseSpaceBasis, QuditBasis[qb["Dimension"] ^ 2]]

QuantumPhaseSpaceTransform[qb_ ? QuditBasisQ] :=
    QuantumPhaseSpaceTransform[qb, QuditBasis["Wigner"[qb["Dimensions"], "Exact" -> ! qb["NumberQ"]]]]

QuantumPhaseSpaceTransform[qb_ ? QuantumBasisQ] :=
    QuantumPhaseSpaceTransform[qb, QuantumBasis["Wigner"[qb["OutputDimensions"], "Exact" -> ! qb["NumberQ"]], "Wigner"[qb["InputDimensions"], "Exact" -> ! qb["NumberQ"]]]]

QuantumPhaseSpaceTransform[qb_ ? QuditBasisQ, args__] := QuantumPhaseSpaceTransform[qb, QuditBasis[args]]

QuantumPhaseSpaceTransform[qb_ ? QuantumBasisQ, args__] := Enclose @ Block[{
    basis = QuantumBasis[args], newBasis
},

    newBasis = If[
        basis["InputDimension"] == 1 && qb["InputDimension"] > 1,
        QuantumBasis[basis, "Input" -> basis["Output"]["Dual"]],
        basis
    ];

    newBasis = QuantumBasis[
        QuditBasis[newBasis["Output"], ConfirmBy[If[qb["OutputDimension"] == 1, 1, Log[newBasis["OutputDimension"], qb["OutputDimension"] ^ 2]], IntegerQ]],
        QuditBasis[newBasis["Input"], ConfirmBy[If[qb["InputDimension"] == 1, 1, Log[newBasis["InputDimension"], qb["InputDimension"] ^ 2]], IntegerQ]]
    ];

    Enclose @ QuantumBasis[
        QuantumPhaseSpaceTransform[qb["Output"], newBasis["Output"]],
        QuantumPhaseSpaceTransform[qb["Input"], newBasis["Input"]],
        "Picture" -> "PhaseSpace",
        qb["Meta"]
    ]
]

QuantumPhaseSpaceTransform[qs_ ? QuantumStateQ, args___] := Enclose @ Chop @ Simplify @ QuantumState[
    ConfirmBy[qs["Double"], QuantumStateQ],
    ConfirmBy[QuantumPhaseSpaceTransform[If[qs["NumberQ"], N, Identity] @ qs["Basis"], args], QuantumBasisQ]
]

QuantumPhaseSpaceTransform[qo_ ? QuantumOperatorQ, args___] := Enclose @ QuantumOperator[
    ConfirmBy[QuantumPhaseSpaceTransform[qo["State"], args], QuantumStateQ],
    qo["Order"]
]

QuantumPhaseSpaceTransform[qc_ ? QuantumChannelQ, args___] :=
    Enclose @ QuantumChannel[ConfirmBy[QuantumPhaseSpaceTransform[qc["QuantumOperator"], args], QuantumOperatorQ]]["DiscardExtraQudits"]

QuantumPhaseSpaceTransform[qmo_ ? QuantumMeasurementOperatorQ, args___] :=
    Enclose @ QuantumMeasurementOperator[qmo["Double"], ConfirmBy[QuantumPhaseSpaceTransform[qmo["POVM"]["Basis"], args], QuantumBasisQ]]

QuantumPhaseSpaceTransform[qco_ ? QuantumCircuitOperatorQ, args___] :=
    Enclose @ QuantumCircuitOperator[If[BarrierQ[#], #, Confirm @ QuantumPhaseSpaceTransform[#, args]] & /@ qco["Elements"], qco["Label"]]

