(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["ClasslessObjects`Tests`Unit`WithOrdinaryObjectSet`", {"MUnit`"}]


Get["ClasslessObjects`"]


AppendTo[$ContextPath, "ClasslessObjects`Private`"]


declareMockObjectWithAlteredSetUpValues[obj_] := (
	ObjectQ[obj] ^= True;
	obj /: Set[obj[_], _] = "altered Set";
	obj /: SetDelayed[obj[_], _] = "altered SetDelayed";
	obj /: Unset[obj[_]] = "altered Unset";
)



(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Wrong argument patterns*)


TestMatch[
	WithOrdinaryObjectSet[]
	,
	HoldPattern @ WithOrdinaryObjectSet[]
	,
	Message[WithOrdinaryObjectSet::argrx, WithOrdinaryObjectSet, 0, 2]
	,
	TestID -> "no args: WithOrdinaryObjectSet evaluation"
]


Module[
	{obj, oldUpValues}
	,
	declareMockObjectWithAlteredSetUpValues[obj];
	oldUpValues = UpValues[obj];
	
	TestMatch[
		WithOrdinaryObjectSet[obj]
		,
		HoldPattern @ WithOrdinaryObjectSet[obj]
		,
		Message[WithOrdinaryObjectSet::argrx, WithOrdinaryObjectSet, 1, 2]
		,
		TestID -> "1 arg: WithOrdinaryObjectSet evaluation"
	];
	
	Test[
		UpValues[obj]
		,
		oldUpValues
		,
		TestID -> "1 arg: object UpValues"
	];
]


TestMatch[
	WithOrdinaryObjectSet[arg1, arg2]
	,
	HoldPattern @ WithOrdinaryObjectSet[arg1, arg2]
	,
	Message[Object::object, 1, WithOrdinaryObjectSet[arg1, arg2]]
	,
	TestID -> "2 args: first non-object: WithOrdinaryObjectSet evaluation"
];


Module[
	{obj, oldUpValues}
	,
	declareMockObjectWithAlteredSetUpValues[obj];
	oldUpValues = UpValues[obj];
	
	TestMatch[
		WithOrdinaryObjectSet[obj1, arg2, arg3]
		,
		HoldPattern @ WithOrdinaryObjectSet[obj1, arg2, arg3]
		,
		Message[WithOrdinaryObjectSet::argrx, WithOrdinaryObjectSet, 3, 2]
		,
		TestID -> "3 args: WithOrdinaryObjectSet evaluation"
	];
	
	Test[
		UpValues[obj]
		,
		oldUpValues
		,
		TestID -> "3 args: object UpValues"
	];
]


(* ::Subsection:: *)
(*Correct arguments*)


Module[
	{obj}
	,
	declareMockObjectWithAlteredSetUpValues[obj];
	
	Test[
		WithOrdinaryObjectSet[obj,
			"evaluation result"
		]
		,
		"evaluation result"
		,
		TestID -> "result of body evaluation is returned"
	]
]


Module[
	{obj}
	,
	declareMockObjectWithAlteredSetUpValues[obj];
	
	Test[
		WithOrdinaryObjectSet[obj,
			UpValues[obj]
		]
		,
		{HoldPattern[ObjectQ[obj]] :> True}
		,
		TestID -> "UpValues inside have no altered set functions"
	]
]


Module[
	{obj, oldUpValues}
	,
	declareMockObjectWithAlteredSetUpValues[obj];
	oldUpValues = UpValues[obj];
	
	WithOrdinaryObjectSet[obj,
		UpValues[obj] = {HoldPattern[something[obj]] :> "some value"}
	];
	
	Test[
		UpValues[obj]
		,
		oldUpValues
		,
		TestID -> "object UpValues are restored"
	];
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
