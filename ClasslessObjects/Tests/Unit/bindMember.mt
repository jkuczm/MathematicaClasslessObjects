(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["ClasslessObjects`Tests`Unit`bindMember`", {"MUnit`"}]


Get["ClasslessObjects`"]


AppendTo[$ContextPath, "ClasslessObjects`Private`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Non-protected object*)


(* ::Subsubsection:: *)
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
			HoldPattern[obj[member]] :> withBoundSelf[obj, value],
			HoldPattern[obj[member, self_]] :> withBoundSelf[self, value]
		}
		,
		TestID -> "Set member: object down values"
	];
]


(* ::Subsubsection:: *)
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
			HoldPattern[obj[member]] :> withBoundSelf[obj, newValue],
			HoldPattern[obj[member, self_]] :> withBoundSelf[self, newValue]
		}
		,
		TestID -> "Reset member: object down values"
	];
]


(* ::Subsection:: *)
(*Protected object*)


(* ::Subsubsection:: *)
(*Set member*)


Module[
	{obj, member, value}
	,
	ObjectQ[obj] ^= True;
	Protect[obj];
	
	Test[
		bindMember[obj, member, value]
		,
		$Failed
		,
		Message[SetDelayed::write, obj, obj@member]
		,
		TestID -> "Set member: bindMember evaluation"
	];
	
	Test[
		DownValues[obj]
		,
		{}
		,
		TestID -> "Set member: object down values"
	];
]


(* ::Subsubsection:: *)
(*Reset member*)


Module[
	{obj, member, oldValue, newValue}
	,
	ObjectQ[obj] ^= True;
	bindMember[obj, member, oldValue];
	Protect[obj];
	
	Test[
		bindMember[obj, member, newValue]
		,
		$Failed
		,
		Message[SetDelayed::write, obj, obj@member]
		,
		TestID -> "Reset member: bindMember evaluation"
	];
	
	Test[
		DownValues[obj]
		,
		{
			HoldPattern[obj[member]] :> withBoundSelf[obj, oldValue],
			HoldPattern[obj[member, self_]] :> withBoundSelf[self, oldValue]
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
