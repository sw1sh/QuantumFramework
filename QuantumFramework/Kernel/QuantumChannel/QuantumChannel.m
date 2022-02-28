Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumChannel"]

PackageScope["QuantumChannelQ"]



QuantumChannelQ[QuantumChannel[qo_]] := QuantumOperatorQ[qo] && qo["OutputQudits"] - qo["InputQudits"] === 1

QuantumChannelQ[___] := False



(qc_QuantumChannel ? QuantumChannelQ)[qs_ ? QuantumStateQ] := QuantumPartialTrace[qc["Operator"] @ qs, {1}]

(qc_QuantumChannel ? QuantumChannelQ)[qo_ ? QuantumOperatorQ] := QuantumChannel[qc["Operator"] @ qo]

