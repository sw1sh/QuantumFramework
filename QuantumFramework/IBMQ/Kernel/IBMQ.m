Begin["Wolfram`IBMQAPI`"] (* Begin Private Context *)


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
        token = Lookup[
            ConfirmMatch[
                OAuthDialogDump`Private`MultipleKeyDialog[
                    "IBMQ",
                    {"API token" -> "token"},
                    "https://quantum-computing.ibm.com/account",
                    "https://quantum-computing.ibm.com/terms/privacy"
                ],
                KeyValuePattern[{"token" -> _String}]
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
    "RawGets" -> {"RawAccount", "RawNetwork", "RawBackends", "RawBackend", "RawJobStatus", "RawJobResults"},
    "RawPosts" -> {"RawRun"},
    "Gets" -> {"Account", "Devices", "Backends", "Backend", "BackendQueue", "JobStatus", "JobResults"},
    "Posts" -> {"RunCircuit"},
    "Information" -> "IBMQ connection for WolframLanguage"
}

IBMQdata["icon"] := Entity["Financial", "NYSE:IBM"][EntityProperty["Financial", "Image"]]


importJson[json_Association] := Dataset @ json

importJson[json : {___Rule}] := importJson[json //. rules : {___Rule} :> RuleCondition[Association[rules]]]

importJson[json_String] := importJson @ ImportString[json, "RawJSON"]


IBMQdata["RawAccount"] := {
    "URL"				-> "https://auth.quantum-computing.ibm.com/api/users/me",
    "HTTPSMethod"		-> "GET",
    "Headers"			-> {"Content-Type" -> "application/json"},
    "Parameters"		-> {},
    "RequiredParameters"-> {},
    "ResultsFunction"	-> (# &)
}

IBMQcookeddata["Account", id_, OptionsPattern[]] := importJson @ KeyClient`rawkeydata[id, "RawAccount"]

IBMQdata["RawNetwork"] := {
    "URL"				-> "https://api.quantum-computing.ibm.com/api/Network",
    "HTTPSMethod"		-> "GET",
    "Headers"			-> {"Content-Type" -> "application/json"},
    "Parameters"		-> {},
    "RequiredParameters"-> {}
}

IBMQcookeddata["Devices", id_, OptionsPattern[]] := AssociationThread @@ Query[{
        Query[All, "name"],
        Query[All, "groups", "open", "projects", "main", "devices", All, 2, "name"]
    }] @ KeyClient`rawkeydata[id, "RawNetwork"]

IBMQdata["RawRun"] := {
    "URL"				-> (URLBuild[{#, "jobs"}] &),
    "BodyData"		    -> {"ParameterlessBodyData" -> "Data"},
    "HTTPSMethod"		-> "POST",
    "Headers"			-> {"Content-Type" -> "application/json"},
    "PathParameters"    -> {"RuntimeUrl"},
    "Parameters"		-> {"Data"},
    "RequiredParameters"-> {"RuntimeUrl", "Data"}
}

getRuntimeUrl[id_] := Query["urls", "services", "runtime"] @ KeyClient`rawkeydata[id, "RawAccount"]

IBMQcookeddata["RunCircuit", id_, opts : OptionsPattern[]] := Enclose @ importJson @ KeyClient`rawkeydata[id,
    "RawRun",
    {
        "RuntimeUrl" -> Confirm @ getRuntimeUrl[id],
        "Data" -> Developer`WriteRawJSONString @ <|
            "program_id" -> "circuit-runner",
            "params" -> <|
                "circuits" -> <|
                    "__type__" -> "QuantumCircuit",
                    "__value__" -> Confirm @ Lookup[Flatten[{opts}], "QPY"]
                |>
            |>,
            "backend" -> Confirm @ Lookup[Flatten[{opts}], "Backend"],
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
    "RequiredParameters"-> {"RuntimeUrl", "JobID"}
}

IBMQdata["RawJobResults"] := {
    "URL"				-> (URLBuild[{#1, "jobs", #2, "results"}] &),
    "HTTPSMethod"		-> "GET",
    "Headers"			-> {"Content-Type" -> "application/json"},
    "PathParameters"    -> {"RuntimeUrl", "JobID"},
    "RequiredParameters"-> {"RuntimeUrl", "JobID"}
}


IBMQcookeddata["JobStatus", id_, opts : OptionsPattern[]] := Enclose @ importJson @ KeyClient`rawkeydata[id,
    "RawJobStatus",
    {
        "RuntimeUrl" -> Confirm @ getRuntimeUrl[id],
        "JobID" -> Confirm @ Lookup[Flatten[{opts}], "JobID"]
    }
]


IBMQcookeddata["JobResults", id_, opts : OptionsPattern[]] := Enclose @ importJson @ KeyClient`rawkeydata[id,
    "RawJobResults",
    {
        "RuntimeUrl" -> Confirm @ getRuntimeUrl[id],
        "JobID" -> Confirm @ Lookup[Flatten[{opts}], "JobID"]
    }
]

IBMQdata["RawBackends"] := {
    "URL"				-> (URLBuild[{#1, "backends"}] &),
    "HTTPSMethod"		-> "GET",
    "Headers"			-> {"Content-Type" -> "application/json"},
    "PathParameters"    -> {"RuntimeUrl"},
    "RequiredParameters"-> {"RuntimeUrl"}
}

IBMQdata["RawBackend"] := {
    "URL"				-> (URLBuild[{#1, "backends", #2, #3}] &),
    "HTTPSMethod"		-> "GET",
    "Headers"			-> {"Content-Type" -> "application/json"},
    "PathParameters"    -> {"RuntimeUrl", "ID", "Property"},
    "RequiredParameters"-> {"RuntimeUrl", "ID", "Property"}
}

IBMQcookeddata["Backends", id_, opts : OptionsPattern[]] := Enclose @ importJson @ KeyClient`rawkeydata[id,
    "RawBackends",
    {
        "RuntimeUrl" -> Confirm @ getRuntimeUrl[id]
    }
]

IBMQcookeddata["Backend", id_, opts : OptionsPattern[]] := Enclose @ importJson @ KeyClient`rawkeydata[id,
    "RawBackend",
    {
        "RuntimeUrl" -> Confirm @ getRuntimeUrl[id],
        "ID" -> Confirm @ Lookup[Flatten[{opts}], "ID"],
        "Property" -> ToLowerCase @ Lookup[Flatten[{opts}], "Property", "properties"]
    }
]

IBMQcookeddata["BackendQueue", id_, opts : OptionsPattern[]] :=
    IBMQcookeddata["Backends", id]["devices"][
        AssociationMap[IBMQcookeddata["Backend", id, "ID" -> #, "Property" -> "Status"]["length_queue"] &]]




IBMQcookeddata[___] := $Failed

IBMQsendmessage[___] := $Failed


End[]

End[]

SetAttributes[{}, {ReadProtected, Protected}]

(* Return three functions to define oauthservicedata, oauthcookeddata, oauthsendmessage  *)

{
    Wolfram`IBMQAPI`Private`IBMQdata,
    Wolfram`IBMQAPI`Private`IBMQcookeddata,
    Wolfram`IBMQAPI`Private`IBMQsendmessage
}

