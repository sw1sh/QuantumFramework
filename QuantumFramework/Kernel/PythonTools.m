Package["Wolfram`QuantumFramework`"]

PackageScope["PythonEvalAttr"]
PackageScope["$PythonSession"]



confirmPython[session_] := ExternalEvaluate[session, "import qiskit"] === Null

$PythonSession := Enclose @
    ConfirmBy[
        SelectFirst[ExternalSessions[], confirmPython, StartExternalSession["Python"]],
        confirmPython,
        "No suitable python environment found."
    ]


PythonEvalAttr[{bytes_ByteArray, attr_String, args___, kwargs : OptionsPattern[]}, OptionsPattern[{"ReturnBytes" -> False}]] := Block[{
    Wolfram`QuantumFramework`pythonBytes = bytes,
    Wolfram`QuantumFramework`pythonAttr = attr,
    Wolfram`QuantumFramework`pythonArgs = {args},
    Wolfram`QuantumFramework`pythonKWargs = Association @ {kwargs}
},
    ExternalEvaluate[$PythonSession, "
import pickle

attr = getattr(pickle.loads(<* Wolfram`QuantumFramework`pythonBytes *>), <* Wolfram`QuantumFramework`pythonAttr *>)
if hasattr(attr, '__call__'):
    result = attr(* <* Wolfram`QuantumFramework`pythonArgs *>, ** <* Wolfram`QuantumFramework`pythonKWargs *>)
else:
    result = attr

if <* TrueQ @ OptionValue[\"ReturnBytes\"] *>:
    result = pickle.dumps(result)

result
    "]
]

