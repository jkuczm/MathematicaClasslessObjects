(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["ClasslessObjects`Tests`Unit`unsetMember`", {"MUnit`"}]


Get["ClasslessObjects`"]


AppendTo[$ContextPath, "ClasslessObjects`Private`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Unset non-existing member*)


Module[
	{obj, member}
	,
	ObjectQ[obj] ^= True;
	
	Test[
		unsetMember[obj, member]
		,
		$Failed
		,
		Message[Unset::norep, obj@member, obj]
		,
		TestID -> "non-existing member: unsetMember evaluation"
	];
	
	Test[
		DownValues[obj]
		,
		{}
		,
		TestID -> "non-existing member: object down values"
	];
]


(* ::Subsection:: *)
(*Unset non-inheritable member*)


Module[
	{obj, member, value}
	,
	ObjectQ[obj] ^= True;
	obj@member = value;
	
	Test[
		unsetMember[obj, member]
		,
		Null
		,
		TestID -> "non-inheritable member: unsetMember evaluation"
	];
	
	Test[
		DownValues[obj]
		,
		{}
		,
		TestID -> "non-inheritable member: object down values"
	];
]


(* ::Subsection:: *)
(*Unset non-inheritable delayed member*)


Module[
	{obj, member, value}
	,
	ObjectQ[obj] ^= True;
	obj@member := value;
	
	Test[
		unsetMember[obj, member]
		,
		Null
		,
		TestID -> "non-inheritable delayed member: unsetMember evaluation"
	];
	
	Test[
		DownValues[obj]
		,
		{}
		,
		TestID -> "non-inheritable delayed member: object down values"
	];
]


(* ::Subsection:: *)
(*Unset inheritable-only member*)


Module[
	{obj, member, value}
	,
	ObjectQ[obj] ^= True;
	obj[member, _] = value;
	
	Test[
		unsetMember[obj, member]
		,
		Null
		,
		TestID -> "inheritable-only member: unsetMember evaluation"
	];
	
	Test[
		DownValues[obj]
		,
		{}
		,
		TestID -> "inheritable-only member: object down values"
	];
]


(* ::Subsection:: *)
(*Unset inheritable-only delayed member*)


Module[
	{obj, member, value}
	,
	ObjectQ[obj] ^= True;
	obj[member, self_] := self + value;
	
	Test[
		unsetMember[obj, member]
		,
		Null
		,
		TestID -> "inheritable-only delayed member: unsetMember evaluation"
	];
	
	Test[
		DownValues[obj]
		,
		{}
		,
		TestID -> "inheritable-only delayed member: object down values"
	];
]


(* ::Subsection:: *)
(*Unset inheritable member*)


Module[
	{obj, member, value}
	,
	ObjectQ[obj] ^= True;
	obj[member] = value;
	obj[member, _] = value;
	
	Test[
		unsetMember[obj, member]
		,
		Null
		,
		TestID -> "inheritable-only member: unsetMember evaluation"
	];
	
	Test[
		DownValues[obj]
		,
		{}
		,
		TestID -> "inheritable-only member: object down values"
	];
]


(* ::Subsection:: *)
(*Unset inheritable delayed member*)


Module[
	{obj, member, value}
	,
	ObjectQ[obj] ^= True;
	obj[member] := value;
	obj[member, _] := value;
	
	Test[
		unsetMember[obj, member]
		,
		Null
		,
		TestID -> "inheritable-only member: unsetMember evaluation"
	];
	
	Test[
		DownValues[obj]
		,
		{}
		,
		TestID -> "inheritable-only member: object down values"
	];
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
