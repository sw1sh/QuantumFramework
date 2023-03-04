Begin["IBMQAPI`"] (* Begin Private Context *)


Begin["`Private`"](* Begin Private Context *)

(******************************* IBMQ *************************************)

(* Authentication information *)

IBMQdata[] = {
    "ServiceName" -> "IBMQ",
    "URLFetchFun" :> Function[
        Enclose @ With[{params = Lookup[{##2}, "Parameters", {}]},
            URLExecute @ HTTPRequest[
                #1,
                MapAt[
                    Append["X-Access-Token" -> Confirm @ Lookup[params, "id"]],
                    KeyMap[Replace["BodyData" -> "Body"]] @
                        Association @ FilterRules[{##2}, Except["Parameters"]],
                    "Headers"
                ]
            ]
        ]
    ] ,
    "ClientInfo" :> Enclose @ With[{
        token = Confirm @ Lookup[
            OAuthDialogDump`Private`MultipleKeyDialog[
                "IBMQ",
                {"API token" -> "token"},
                "https://quantum-computing.ibm.com/account",
                "https://quantum-computing.ibm.com/terms/privacy"
            ],
            "token"
        ]
    },
        URLExecute[
            HTTPRequest[
                "https://auth.quantum-computing.ibm.com/api/users/loginWithToken",
                <|Method -> "POST", "Body" -> <|"apiToken" -> token|>|>
            ],
            "JSON"
        ]
    ],
    "RawGets" -> {"RawMe", "RawJobStatus", "RawJobResults"},
    "RawPosts" -> {"RawRun"},
    "Gets" -> {"JobStatus", "JobResults"},
    "Posts" -> {"RunCircuit"},
    "Information" -> "IBMQ connection for WolframLanguage"
}

IBMQdata["icon"] := Entity["Financial", "NYSE:IBM"][EntityProperty["Financial", "Image"]]


IBMQdata["RawMe"] := {
    "URL"				-> "https://auth.quantum-computing.ibm.com/api/users/me",
    "HTTPSMethod"		-> "GET",
    "Headers"			-> {"Content-Type" -> "application/json"},
    "Parameters"		-> {},
    "RequiredParameters"-> {},
    "ResultsFunction"	-> (# &)
}

IBMQdata["RawRun"] := {
    "URL"				-> (URLBuild[{#, "jobs"}] &),
    "BodyData"		    -> {"ParameterlessBodyData" -> "Data"},
    "HTTPSMethod"		-> "POST",
    "Headers"			-> {"Content-Type" -> "application/json"},
    "PathParameters"    -> {"RuntimeUrl"},
    "Parameters"		-> {"Data"},
    "RequiredParameters"-> {"RuntimeUrl", "Data"},
    "ResultsFunction"	-> (# &)
}

getRuntimeUrl[id_] := Query["urls", "services", "runtime"] @ KeyClient`rawkeydata[id, "RawMe"]

IBMQcookeddata["RunCircuit", id_, opts : OptionsPattern[]] := Enclose @ KeyClient`rawkeydata[id,
    "RawRun",
    {
        "RuntimeUrl" -> Confirm @ getRuntimeUrl[id],
        "Data" -> Developer`WriteRawJSONString @ <|
            "program_id" -> "circuit-runner",
            "params" -> <|
                "circuits" -> <|
                    "__type__" -> "QuantumCircuit",
                    "__value__" -> Confirm @ OptionValue[{opts}, "QPY"]
                |>
            |>,
            "backend" -> Confirm @ OptionValue[{opts}, "Backend"],
            "hub" -> "ibm-q",
            "group" -> "open",
            "project" -> "main"
        |>
    }
]

IBMQdata["RawJobStatus"] := {
    "URL"				-> (URLBuild[{#1, "jobs", #2}] &),
    "HTTPSMethod"		-> "GET",
    "Headers"			-> {"Content-Type" -> "application/json"},
    "PathParameters"    -> {"RuntimeUrl", "JobID"},
    "RequiredParameters"-> {"RuntimeUrl", "JobID"},
    "ResultsFunction"	-> (# &)
}

IBMQdata["RawJobResults"] := {
    "URL"				-> (URLBuild[{#1, "jobs", #2, "results"}] &),
    "HTTPSMethod"		-> "GET",
    "Headers"			-> {"Content-Type" -> "application/json"},
    "PathParameters"    -> {"RuntimeUrl", "JobID"},
    "RequiredParameters"-> {"RuntimeUrl", "JobID"},
    "ResultsFunction"	-> (# &)
}

IBMQcookeddata["JobStatus", id_, opts : OptionsPattern[]] := Enclose @ KeyClient`rawkeydata[id,
    "RawJobStatus",
    {
        "RuntimeUrl" -> Confirm @ getRuntimeUrl[id],
        "JobID" -> Confirm @ OptionValue[{opts}, "JobID"]
    }
]


IBMQcookeddata["JobResults", id_, opts : OptionsPattern[]] := Enclose @ KeyClient`rawkeydata[id,
    "RawJobResults",
    {
        "RuntimeUrl" -> Confirm @ getRuntimeUrl[id],
        "JobID" -> Confirm @ OptionValue[{opts}, "JobID"]
    }
]


IBMQcookeddata[___] := $Failed

IBMQsendmessage[___] := $Failed


End[]

End[]

SetAttributes[{}, {ReadProtected, Protected}]

(* Return three functions to define oauthservicedata, oauthcookeddata, oauthsendmessage  *)

{
    IBMQAPI`Private`IBMQdata,
    IBMQAPI`Private`IBMQcookeddata,
    IBMQAPI`Private`IBMQsendmessage
}

