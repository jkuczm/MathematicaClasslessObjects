(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["ClasslessObjects`Tests`Unit`setMember`", {"MUnit`"}]


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
		setMember[obj, member, value]
		,
		value
		,
		TestID -> "Set member: setMember evaluation"
	];
	
	Test[
		DownValues[obj]
		,
		{HoldPattern[obj[member, _:obj]] :> value}
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
		{HoldPattern[obj[member, _:obj]] :> newValue}
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
		setMember[obj, member, value]
		,
		value
		,
		Message[Set::write,
			obj, obj[member, HoldPattern[Optional][HoldPattern[_], obj]]
		]
		,
		TestID -> "Protected: Set member: setMember evaluation"
	];
	
	Test[
		DownValues[obj]
		,
		{}
		,
		TestID -> "Protected: Set member: object down values"
	];
]


(* ::Subsubsection:: *)
(*Reset member*)


Module[
	{obj, member, oldValue, newValue}
	,
	ObjectQ[obj] ^= True;
	setMember[obj, member, oldValue];
	Protect[obj];
	
	Test[
		setMember[obj, member, newValue]
		,
		newValue
		,
		Message[Set::write,
			obj, obj[member, HoldPattern[Optional][HoldPattern[_], obj]]
		]
		,
		TestID -> "Protected: Reset member: setMember evaluation"
	];
	
	Test[
		DownValues[obj]
		,
		{HoldPattern[obj[member, _:obj]] :> oldValue}
		,
		TestID -> "Protected: Reset member: object down values"
	];
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
