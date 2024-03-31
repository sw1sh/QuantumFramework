Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumPhaseSpaceTransform"]



QuantumPhaseSpaceTransform[qb_ ? QuditBasisQ, args__] := With[{exactQ = TrueQ[OptionValue[Select[{args}, OptionQ], "Exact"]]},
    Enclose @ With[{
        phaseSpaceBasis = ConfirmBy[QuditBasis @@ DeleteCases[{args}, "Exact" -> _], QuditBasisQ]
    },
        If[exactQ, Identity, N] @ If[phaseSpaceBasis["Dimension"] == qb["Dimension"] ^ 2, phaseSpaceBasis, QuditBasis[qb["Dimension"] ^ 2]]
    ]
]

QuantumPhaseSpaceTransform[qb_ ? QuditBasisQ] := QuantumPhaseSpaceTransform[qb, "Wigner"[qb["Dimension"]]]

QuantumPhaseSpaceTransform[qb_ ? QuantumBasisQ, args___] := 
    Enclose @ QuantumBasis[
        QuantumTensorProduct[ConfirmBy[QuantumPhaseSpaceTransform[#, args], QuditBasisQ] & /@ qb["Output"]["Decompose"]],
        QuantumTensorProduct[ConfirmBy[QuantumPhaseSpaceTransform[#, args], QuditBasisQ] & /@ qb["Input"]["Decompose"]],
        "Picture" -> "PhaseSpace",
        qb["Meta"]
    ]

QuantumPhaseSpaceTransform[qs_ ? QuantumStateQ, args___] := Enclose @ Chop @ Simplify @ QuantumState[
    ConfirmBy[qs["Double"], QuantumStateQ],
    ConfirmBy[QuantumPhaseSpaceTransform[qs["Basis"], args, "Exact" -> ! qs["NumberQ"]], QuantumBasisQ]
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

