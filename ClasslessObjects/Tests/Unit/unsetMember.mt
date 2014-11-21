(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["ClasslessObjects`Tests`Unit`unsetMember`", {"MUnit`"}]


Get["ClasslessObjects`"]


AppendTo[$ContextPath, "ClasslessObjects`Private`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Non-protected object*)


(* ::Subsubsection:: *)
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


(* ::Subsubsection:: *)
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


(* ::Subsubsection:: *)
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


(* ::Subsubsection:: *)
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


(* ::Subsubsection:: *)
(*Unset inheritable-only delayed member*)


Module[
	{obj, member, value}
	,
	ObjectQ[obj] ^= True;
	obj[member, self_] := {value, self};
	
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


(* ::Subsubsection:: *)
(*Unset inheritable member*)


Module[
	{obj, member, value}
	,
	ObjectQ[obj] ^= True;
	obj[member, _:obj] = value;
	
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


(* ::Subsubsection:: *)
(*Unset inheritable delayed member*)


Module[
	{obj, member, value}
	,
	ObjectQ[obj] ^= True;
	obj[member, self_:obj] := {value, self};
	
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
(*Protected object*)


(* ::Subsubsection:: *)
(*Unset non-existing member*)


Module[
	{obj, member, downValues}
	,
	ObjectQ[obj] ^= True;
	Protect[obj];
	downValues = DownValues[obj];
	
	Test[
		unsetMember[obj, member]
		,
		$Failed
		,
		Message[Unset::write, obj, obj@member]
		,
		TestID -> "protected: non-existing member: unsetMember evaluation"
	];
	
	Test[
		DownValues[obj]
		,
		downValues
		,
		TestID -> "protected: non-existing member: object down values"
	];
]


(* ::Subsubsection:: *)
(*Unset non-inheritable member*)


Module[
	{obj, member, value, downValues}
	,
	ObjectQ[obj] ^= True;
	obj@member = value;
	Protect[obj];
	downValues = DownValues[obj];
	
	Test[
		unsetMember[obj, member]
		,
		$Failed
		,
		Message[Unset::write, obj, obj@member]
		,
		TestID -> "protected: non-inheritable member: unsetMember evaluation"
	];
	
	Test[
		DownValues[obj]
		,
		downValues
		,
		TestID -> "protected: non-inheritable member: object down values"
	];
]


(* ::Subsubsection:: *)
(*Unset non-inheritable delayed member*)


Module[
	{obj, member, value, downValues}
	,
	ObjectQ[obj] ^= True;
	obj@member := value;
	Protect[obj];
	downValues = DownValues[obj];
	
	Test[
		unsetMember[obj, member]
		,
		$Failed
		,
		Message[Unset::write, obj, obj@member]
		,
		TestID ->
			"protected: non-inheritable delayed member: unsetMember evaluation"
	];
	
	Test[
		DownValues[obj]
		,
		downValues
		,
		TestID ->
			"protected: non-inheritable delayed member: object down values"
	];
]


(* ::Subsubsection:: *)
(*Unset inheritable-only member*)


Module[
	{obj, member, value, downValues}
	,
	ObjectQ[obj] ^= True;
	obj[member, _] = value;
	Protect[obj];
	downValues = DownValues[obj];
	
	Test[
		unsetMember[obj, member]
		,
		$Failed
		,
		Message[Unset::write, obj, obj@member]
		,
		TestID -> "protected: inheritable-only member: unsetMember evaluation"
	];
	
	Test[
		DownValues[obj]
		,
		downValues
		,
		TestID -> "protected: inheritable-only member: object down values"
	];
]


(* ::Subsubsection:: *)
(*Unset inheritable-only delayed member*)


Module[
	{obj, member, value, downValues}
	,
	ObjectQ[obj] ^= True;
	obj[member, self_] := {value, self};
	Protect[obj];
	downValues = DownValues[obj];
	
	Test[
		unsetMember[obj, member]
		,
		$Failed
		,
		Message[Unset::write, obj, obj@member]
		,
		TestID -> "protected: inheritable-only delayed member: \
unsetMember evaluation"
	];
	
	Test[
		DownValues[obj]
		,
		downValues
		,
		TestID ->
			"protected: inheritable-only delayed member: object down values"
	];
]


(* ::Subsubsection:: *)
(*Unset inheritable member*)


Module[
	{obj, member, value, downValues}
	,
	ObjectQ[obj] ^= True;
	obj[member, _:obj] = value;
	Protect[obj];
	downValues = DownValues[obj];
	
	Test[
		unsetMember[obj, member]
		,
		$Failed
		,
		Message[Unset::write, obj, obj@member]
		,
		TestID -> "protected: inheritable-only member: unsetMember evaluation"
	];
	
	Test[
		DownValues[obj]
		,
		downValues
		,
		TestID -> "protected: inheritable-only member: object down values"
	];
]


(* ::Subsubsection:: *)
(*Unset inheritable delayed member*)


Module[
	{obj, member, value, downValues}
	,
	ObjectQ[obj] ^= True;
	obj[member, self_:obj] := {value, self};
	Protect[obj];
	downValues = DownValues[obj];
	
	Test[
		unsetMember[obj, member]
		,
		$Failed
		,
		Message[Unset::write, obj, obj@member]
		,
		TestID -> "protected: inheritable-only member: unsetMember evaluation"
	];
	
	Test[
		DownValues[obj]
		,
		downValues
		,
		TestID -> "protected: inheritable-only member: object down values"
	];
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
