Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumPartialTranspose"]


QuantumPartialTranspose[qobj_, args___] /; QuantumStateQ[qobj] || QuantumOperatorQ[qobj] := qobj["Transpose", args]

