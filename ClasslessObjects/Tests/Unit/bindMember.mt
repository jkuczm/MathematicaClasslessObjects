(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["ClasslessObjects`Tests`Unit`bindMember`", {"MUnit`"}]


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
		bindMember[obj, member, value]
		,
		Null
		,
		TestID -> "Set member: bindMember evaluation"
	];
	
	Test[
		DownValues[obj]
		,
		{
			HoldPattern[obj[member]] :>  Block[{$self = obj}, value],
			HoldPattern[obj[member, self_]] :> Block[{$self = self}, value]
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
	bindMember[obj, member, oldValue];
	
	Test[
		bindMember[obj, member, newValue]
		,
		Null
		,
		TestID -> "Reset member: bindMember evaluation"
	];
	
	Test[
		DownValues[obj]
		,
		{
			HoldPattern[obj[member]] :>  Block[{$self = obj}, newValue],
			HoldPattern[obj[member, self_]] :> Block[{$self = self}, newValue]
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
