Package["Wolfram`QuantumFramework`"]

PackageScope["PythonEvalAttr"]
PackageScope["$PythonSession"]



$PythonPackages = {"qiskit", "qiskit-ibm-provider", "matplotlib"}

confirmPython[session_] := confirmPython[session] = AllTrue[$PythonPackages, ResourceFunction["PythonPackageInstalledQ"][session, #] &] &&
    ExternalEvaluate[session, "import qiskit\nfrom qiskit import QuantumCircuit"] === Null

$PythonSession := Block[{session},
Enclose[
    SelectFirst[ExternalSessions[], confirmPython, ConfirmBy[
        session = StartExternalSession["Python"],
        confirmPython,
        "No qiskit package found. Installing it first instead."
    ]],
    Enclose[
        PrintTemporary["Installing Python Packages ..."];
        Confirm[ResourceFunction["PythonPackageInstall"][session, $PythonPackages]];
        ConfirmBy[session, confirmPython, "Failed to install qiskit."]
    ] &
]
]


PythonEvalAttr[{bytes_ByteArray, attr_String, args___, kwargs : OptionsPattern[]}, OptionsPattern[{"ReturnBytes" -> False}]] := Block[{
    Wolfram`QuantumFramework`pythonBytes = bytes,
    Wolfram`QuantumFramework`pythonAttr = attr,
    Wolfram`QuantumFramework`pythonArgs = {args},
    Wolfram`QuantumFramework`pythonKWargs = Association @ {kwargs},
    Wolfram`QuantumFramework`returnBytes = OptionValue["ReturnBytes"]
},
    ExternalEvaluate[$PythonSession, "
import pickle

attr = getattr(pickle.loads(<* Wolfram`QuantumFramework`pythonBytes *>), <* Wolfram`QuantumFramework`pythonAttr *>)
if hasattr(attr, '__call__'):
    result = attr(* <* Wolfram`QuantumFramework`pythonArgs *>, ** <* Wolfram`QuantumFramework`pythonKWargs *>)
else:
    result = attr

if <* TrueQ @ Wolfram`QuantumFramework`returnBytes  *>:
    result = pickle.dumps(result)

result
    "]
]

