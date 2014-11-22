(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["ClasslessObjects`Tests`Unit`generalMessages`", {"MUnit`"}]


Get["ClasslessObjects`"]


AppendTo[$ContextPath, "ClasslessObjects`Private`"]


(* ::Section:: *)
(*Tests*)


Test[
	ToString @ StringForm[general::object, HoldForm[2], HoldForm[f[arg1, arg2]]]
	,
	"Object expected at position 2 in f[arg1, arg2]."
	,
	TestID -> "object"
]


Test[
	ToString @ StringForm[general::objects,
		HoldForm[2], HoldForm[f[arg1, {obj, nonObj}]], HoldForm[nonObj]
	]
	,
	"Object or non-empty list of objects expected at position 2 in \
f[arg1, {obj, nonObj}]. Argument contained following non-object(s): nonObj."
	,
	TestID -> "objects"
]


Test[
	ToString @
		StringForm[general::nonObject, HoldForm[2], HoldForm[f[arg1, arg2]]]
	,
	"Non-object expected at position 2 in f[arg1, arg2]."
	,
	TestID -> "nonObject"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
