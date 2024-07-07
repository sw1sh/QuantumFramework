Package["Wolfram`QuantumFramework`"]

PackageScope["PythonEvalAttr"]
PackageScope["$PythonSession"]
PackageScope["PythonEvaluate"]



$BasePythonPackages = {
    "wolframclient", "matplotlib", "pylatexenc", "qiskit>=1.0"
}

$PythonEnvironmentPackages = <|
    "default" -> {"qiskit-aer", "qiskit-ibm-provider", "qiskit-braket-provider"},
    "qctrl" -> {"qiskit-aer", "qiskit-ibm-provider", "qiskit-braket-provider", "fire-opal"},
    "classiq" -> {"classiq>=0.40.0"},
    "pyzx" -> {"git+https://github.com/Quantomatic/pyzx.git"}
|>

$PythonSession[env_String] := With[{id = StringTemplate["QuantumFramework_``"] @ env},
    SelectFirst[
        ExternalSessions["Python"],
        #["ID"] == id &,
        With[{versions = Through[PacletFind["ExternalEvaluate"]["Version"]], required = "32.2"},
            If[ AllTrue[versions, ResourceFunction["VersionOrder"][#, required] > 0 &], 
                Failure["DependencyFailure", <|
                    "MessageTemplate" ->  "ExternalEvaluate paclet should be at least version `` (availabe in Wolfram Language 14), but only versions {``} are found", 
                    "MessageParameters" -> {required, StringRiffle[versions, ", "]}
                |>],
                StartExternalSession[{{"Python", "StandardErrorFunction" -> Null},
                    "ID" -> id,
                    "Evaluator" -> <|"Dependencies" -> Join[$BasePythonPackages, $PythonEnvironmentPackages[env]], "PythonRuntime" -> "3.11"|>,
                    "SessionProlog" -> "import os; os.environ['PATH'] = f\"/opt/homebrew/bin:{os.environ['PATH']}\"; import qiskit"
                }]
            ]
        ]
    ]
]


PythonEvaluate[ctx_String /; StringContainsQ[ctx, "`"], code_String, env : _String | Automatic : Automatic] :=
Enclose @ With[{session = Confirm @ $PythonSession[Replace[env, Automatic -> "default"]]},
    WithCleanup[
        PrependTo[$ContextPath, ctx],
        Progress`EvaluateWithProgress[
            ExternalEvaluate[session, code],
            <|"Text" -> "Running Python code", "ElapsedTime" -> Automatic|>
        ],
        $ContextPath = DeleteCases[$ContextPath, ctx]
    ]
]

PythonEvaluate[code_String, env : _String | Automatic : Automatic] := PythonEvaluate["Wolfram`QuantumFramework`", code, env]


PythonEvalAttr[{bytes_ByteArray, attr_String, args___, kwargs : OptionsPattern[]}, OptionsPattern[{"ReturnBytes" -> False, "Environment" -> Automatic}]] := Block[{
    pythonBytes = bytes,
    pythonAttr = attr,
    pythonArgs = {args},
    pythonKWargs = Association @ {kwargs},
    returnBytes = OptionValue["ReturnBytes"]
},
    PythonEvaluate[Context[pythonBytes], "
import pickle

attr = getattr(pickle.loads(<* pythonBytes *>), <* pythonAttr *>)
if hasattr(attr, '__call__'):
    result = attr(* <* pythonArgs *>, ** <* pythonKWargs *>)
else:
    result = attr

if <* TrueQ @ returnBytes  *>:
    result = pickle.dumps(result)

result
",
    Replace[OptionValue["Environment"], Automatic -> "default"]
]
]

