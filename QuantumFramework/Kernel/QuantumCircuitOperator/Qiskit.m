Package["Wolfram`QuantumFramework`"]

PackageExport["QiskitCircuit"]
PackageExport["QuantumCircuitOperatorToQiskit"]



existingGateNames = {
    "barrier", "measure", "reset", "u3", "u2", "u1", 
    "cx", "id", "u0", "u", "p", "x", "y", "z", "h", "s", "sdg", "t", 
    "tdg", "rx", "ry", "rz", "sx", "sxdg", "cz", "cy", "swap", "ch", 
    "ccx", "cswap", "crx", "cry", "crz", "cu1", "cp", "cu3", "csx", 
    "cu", "rxx", "rzz", "rccx", "rc3x", "c3x", "c3sx", "c4x"
};

labelToGate = Replace[{
    "Controlled"[x_String, ___] :> "c" <> ToLowerCase[x],
    "Computational" | "Computational"[___] -> "measure",
    x_String :> ToLowerCase[x]
}]

QuantumCircuitOperatorToQiskit[qco_QuantumCircuitOperator] := Enclose @ Block[{
    operators = Map[
        With[{
            label = labelToGate @ #["Label"]
        },
            If[ label == "measure",
                Splice[{label, None, {#, #}} & /@ (#["InputOrder"] - 1)],
                {
                    label,
                    NumericArray @ N @ #["MatrixRepresentation"],
                    #["InputOrder"] - 1
                }
            ]
        ] &,
        qco["Operators"]
    ],
    arity = {qco["Arity"], Replace[Quiet[qco["Eigenqudits"]], Except[_Integer] -> Nothing]}
},
    ExternalEvaluate[Confirm @ $PythonSession, "
from wolframclient.language import wl

from qiskit import QuantumCircuit
from qiskit.extensions import UnitaryGate

import pickle
import os

if 'texbin' not in os.environ['PATH']:
    os.environ['PATH'] += os.pathsep + os.pathsep.join(['/Library/TeX/texbin'])

circuit = QuantumCircuit(
    * <* Wolfram`QuantumFramework`Qiskit`PackagePrivate`arity *>
)

for name, data, order in <* Wolfram`QuantumFramework`Qiskit`PackagePrivate`operators *>:
    if name.lower() in <* Wolfram`QuantumFramework`Qiskit`PackagePrivate`existingGateNames *>:
        getattr(circuit, name.lower())(*tuple(order))
    else:
        circuit.append(UnitaryGate(data, name), tuple(order))

wl.Wolfram.QuantumFramework.QiskitCircuit(pickle.dumps(circuit))
    "]
]

QiskitCircuit[bytes_ByteArray]["Bytes"] := bytes

qc_QiskitCircuit["Eval", attr_String, args___, kwargs : OptionsPattern[]] :=
    PythonEvalAttr[{qc["Bytes"], attr, args, kwargs}]

qc_QiskitCircuit["EvalBytes", attr_String, args___, kwargs : OptionsPattern[]] :=
    PythonEvalAttr[{qc["Bytes"], attr, args, kwargs}, "ReturnBytes" -> True]


qiskitDiagram[qc_QiskitCircuit, OptionsPattern[{"Scale" -> 5}]] := Enclose @ With[{
    latex = StringSplit[qc["Eval", "draw", "output" -> "latex_source", "scale" -> OptionValue["Scale"]], EndOfLine]
},
    Confirm @ Check[Needs["MaTeX`"], ResourceFunction["MaTeXInstall"][]; Needs["MaTeX`"]];
    MaTeX`MaTeX[
        StringJoin @ latex[[7 ;; -2]],
        "BasePreamble" -> {},
        "Preamble" -> Prepend[latex[[3 ;; 4]], "\\standaloneconfig{border=-16 -8 -16 -16}"]
    ]
]
    

qc_QiskitCircuit["Diagram", opts : OptionsPattern[]] := qiskitDiagram[qc, opts]

qc_QiskitCircuit["Qubits"] := qc["Eval", "num_qubits"]

qc_QiskitCircuit["Depth"] := qc["Eval", "depth"]

qc_QiskitCircuit["Ops"] := qc["Eval", "count_ops"]


qiskitMatrix[qc_QiskitCircuit] := Block[{Wolfram`QuantumFramework`pythonBytes = qc["Bytes"]},
    ExternalEvaluate[$PythonSession, "
from qiskit import BasicAer, transpile

import pickle

qc = pickle.loads(<* Wolfram`QuantumFramework`pythonBytes *>)

backend = BasicAer.get_backend('unitary_simulator')

job = backend.run(transpile(qc, backend))
job.result().get_unitary(qc, decimals=3)
    "]
]

qc_QiskitCircuit["Matrix"] := qiskitMatrix[qc]


(* Formatting *)

QiskitCircuit /: MakeBoxes[qc_QiskitCircuit, format_] :=
    BoxForm`ArrangeSummaryBox[
        "QiskitCircuit",
        qc,
        qc["Diagram", "Scale" -> 1],
        {
            {BoxForm`SummaryItem[{"Qubits: ", qc["Qubits"]}]},
            {BoxForm`SummaryItem[{"Depth: ", qc["Depth"]}]}
        },
        {
            {BoxForm`SummaryItem[{"Ops: ", qc["Ops"]}]}
        },
        format,
        "Interpretable" -> Automatic
    ]

