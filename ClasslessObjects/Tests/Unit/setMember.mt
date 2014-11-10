(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["ClasslessObjects`Tests`Unit`setMember`", {"MUnit`"}]


Get["ClasslessObjects`"]


AppendTo[$ContextPath, "ClasslessObjects`Private`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Set member*)


Module[
	{obj, member, value}
	,
	ObjectQ[obj] ^= True;
	
	Test[
		setMember[obj, member, value]
		,
		value
		,
		TestID -> "Set member: setMember evaluation"
	];
	
	Test[
		DownValues[obj]
		,
		{
			HoldPattern[obj[member]] :> value,
			HoldPattern[obj[member, _]] :> value
		}
		,
		TestID -> "Set member: object down values"
	];
]


(* ::Subsection:: *)
(*Reset member*)


Module[
	{obj, member, oldValue, newValue}
	,
	ObjectQ[obj] ^= True;
	setMember[obj, member, oldValue];
	
	Test[
		setMember[obj, member, newValue]
		,
		newValue
		,
		TestID -> "Reset member: setMember evaluation"
	];
	
	Test[
		DownValues[obj]
		,
		{
			HoldPattern[obj[member]] :> newValue,
			HoldPattern[obj[member, _]] :> newValue
		}
		,
		TestID -> "Reset member: object down values"
	];
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
