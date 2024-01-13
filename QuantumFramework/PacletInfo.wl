(* ::Package:: *)

PacletObject[
  <|
    "Name" -> "Wolfram/QuantumFramework",
    "Description" -> "Perform analytic and numeric quantum computations",
    "Creator" -> "Wolfram Research, Quantum Computation Framework team",
    "License" -> "MIT",
    "PublisherID" -> "Wolfram",
    "Version" -> "1.2.15",
    "WolframVersion" -> "13.1+",
    "PrimaryContext" -> "Wolfram`QuantumFramework`",
    "Extensions" -> {
      {
        "Kernel",
        "Root" -> "Kernel",
        "Context" -> {
          "Wolfram`QuantumFrameworkLoader`",
          {
            "Wolfram`QuantumFramework`",
            "QuantumFrameworkMain.m"
          }
        },
        "Symbols" -> {
          "Wolfram`QuantumFramework`QuditName",
          "Wolfram`QuantumFramework`QuditBasis",
          "Wolfram`QuantumFramework`QuantumBasis",
          "Wolfram`QuantumFramework`QuantumState",
          "Wolfram`QuantumFramework`QuantumOperator",
          "Wolfram`QuantumFramework`QuantumMeasurementOperator",
          "Wolfram`QuantumFramework`QuantumCircuitOperator",
          "Wolfram`QuantumFramework`QuantumMeasurement",
          "Wolfram`QuantumFramework`QuantumTensorProduct",
          "Wolfram`QuantumFramework`QuantumPartialTrace",
          "Wolfram`QuantumFramework`QuantumDistance",
          "Wolfram`QuantumFramework`QuantumEntanglementMonotone",
          "Wolfram`QuantumFramework`QuantumEntangledQ",
          "Wolfram`QuantumFramework`QuantumWignerTransform",
          "Wolfram`QuantumFramework`QuantumChannel",
          "Wolfram`QuantumFramework`QuantumStateEstimate",
          "Wolfram`QuantumFramework`QuantumMeasurementSimulation",
          "Wolfram`QuantumFramework`QuantumEvolve"
        }
      },
      {"Documentation", "Language" -> "English"},
      {"AutoCompletionData", "Root" -> "AutoCompletionData"},
      {"FrontEnd"},
      {"Asset", "Root" -> "Assets", "Assets" -> {
        {"IBMQ.paclet", "ServiceConnection_IBMQ-0.0.2.paclet"},
        {"Cotengra.paclet", "Cotengra-0.1.paclet"}
      }}
    }
  |>
]
