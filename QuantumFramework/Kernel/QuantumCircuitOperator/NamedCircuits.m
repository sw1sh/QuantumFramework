Package["Wolfram`QuantumFramework`"]



QuantumCircuitOperator[{"Graph", g_Graph}] := QuantumCircuitOperator[QuantumOperator["CZ", {#1, #2}] & @@@ EdgeList[IndexGraph @ g]]

