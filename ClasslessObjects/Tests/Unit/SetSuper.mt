(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["ClasslessObjects`Tests`Unit`SetSuper`", {"MUnit`"}]


Get["ClasslessObjects`"]
Get["ClasslessObjects`Tests`Unit`Utilities`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Wrong argument patterns*)


(* ::Subsection:: *)
(*No arguments*)


TestMatch[
	SetSuper[]
	,
	HoldPattern @ SetSuper[]
	,
	Message[SetSuper::argrx, SetSuper, 0, 2]
	,
	TestID -> "no args: SetSuper evaluation"
]


(* ::Subsection:: *)
(*One argument*)


Module[
	{arg1, oldDefinition}
	,
	declareMockObject[arg1];
	oldDefinition = ToString @ Definition[arg1];
	
	TestMatch[
		SetSuper[arg1]
		,
		HoldPattern @ SetSuper[arg1]
		,
		Message[SetSuper::argrx, SetSuper, 1, 2]
		,
		TestID -> "1 arg: SetSuper evaluation"
	];
	
	Test[
		ToString @ Definition[arg1]
		,
		oldDefinition
		,
		TestID -> "1 arg: arg1 definition"
	];
]


(* ::Subsection:: *)
(*Two arguments*)


Module[
	{arg1, arg2, oldDefinition1, oldDefinition2}
	,
	{oldDefinition1, oldDefinition2} =
		ToString[Definition[#]]& /@ {arg1, arg2};
	
	TestMatch[
		SetSuper[arg1, arg2]
		,
		HoldPattern @ SetSuper[arg1, arg2]
		,
		{
			Message[Object::object, 1, SetSuper[arg1, arg2]],
			Message[Object::object, 2, SetSuper[arg1, arg2]]
		}
		,
		TestID -> "2 args: both non-objects: SetSuper evaluation"
	];
	
	Test[
		ToString @ Definition[arg1]
		,
		oldDefinition1
		,
		TestID -> "2 args: both non-objects: arg1 definition"
	];
	Test[
		ToString @ Definition[arg2]
		,
		oldDefinition2
		,
		TestID -> "2 args: both non-objects: arg2 definition"
	];
]

Module[
	{arg1, arg2, oldDefinition1, oldDefinition2}
	,
	declareMockObject[arg2];
	
	{oldDefinition1, oldDefinition2} =
		ToString[Definition[#]]& /@ {arg1, arg2};
	
	
	TestMatch[
		SetSuper[arg1, arg2]
		,
		HoldPattern @ SetSuper[arg1, arg2]
		,
		Message[Object::object, 1, SetSuper[arg1, arg2]]
		,
		TestID -> "2 args: non-object, object: SetSuper evaluation"
	];
	
	Test[
		ToString @ Definition[arg1]
		,
		oldDefinition1
		,
		TestID -> "2 args: non-object, object: arg1 definition"
	];
	Test[
		ToString @ Definition[arg2]
		,
		oldDefinition2
		,
		TestID -> "2 args: non-object, object: arg2 definition"
	];
]

Module[
	{arg1, arg2, oldDefinition1, oldDefinition2}
	,
	declareMockObject[arg1];
	
	{oldDefinition1, oldDefinition2} =
		ToString[Definition[#]]& /@ {arg1, arg2};
	
	
	TestMatch[
		SetSuper[arg1, arg2]
		,
		HoldPattern @ SetSuper[arg1, arg2]
		,
		Message[Object::object, 2, SetSuper[arg1, arg2]]
		,
		TestID -> "2 args: object, non-object: SetSuper evaluation"
	];
	
	Test[
		ToString @ Definition[arg1]
		,
		oldDefinition1
		,
		TestID -> "2 args: object, non-object: arg1 definition"
	];
	Test[
		ToString @ Definition[arg2]
		,
		oldDefinition2
		,
		TestID -> "2 args: object, non-object: arg2 definition"
	];
]


(* ::Subsection:: *)
(*Three arguments*)


Module[
	{arg1, arg2, arg3, oldDefinition1, oldDefinition2, oldDefinition3}
	,
	Scan[declareMockObject, {arg1, arg2, arg3}];
	{oldDefinition1, oldDefinition2, oldDefinition3} =
		ToString[Definition[#]]& /@ {arg1, arg2, arg3};
	
	TestMatch[
		SetSuper[arg1, arg2, arg3]
		,
		HoldPattern @ SetSuper[arg1, arg2, arg3]
		,
		Message[SetSuper::argrx, SetSuper, 3, 2]
		,
		TestID -> "3 args: SetSuper evaluation"
	];
	
	Test[
		ToString @ Definition[arg1]
		,
		oldDefinition1
		,
		TestID -> "3 args: arg1 definition"
	];
	Test[
		ToString @ Definition[arg2]
		,
		oldDefinition2
		,
		TestID -> "3 args: arg2 definition"
	];
	Test[
		ToString @ Definition[arg3]
		,
		oldDefinition3
		,
		TestID -> "3 args: arg2 definition"
	];
]


(* ::Subsection:: *)
(*Correct arguments*)


(* ::Subsubsection:: *)
(*Cyclic inheritance*)


Module[
	{obj, oldDefinition}
	,
	declareMockObject[obj];
	oldDefinition = ToString @ Definition[obj];
	
	Test[
		SetSuper[obj, obj]
		,
		$Failed
		,
		Message[SetSuper::cyclic, obj, obj, {obj}]
		,
		TestID -> "Cyclic inheritance: cycle lenght 1: SetSuper evaluation"
	];
	
	Test[
		ToString @ Definition[obj]
		,
		oldDefinition
		,
		TestID -> "Cyclic inheritance: cycle lenght 1: obj definition"
	];
]


Module[
	{
		parent, parentOldDefinition,
		child, childOldDefinition
	}
	,
	declareMockObject[parent];
	declareMockObject[child, parent];
	
	{parentOldDefinition, childOldDefinition} =
		ToString[Definition[#]]& /@ {parent, child};
	

	Test[
		SetSuper[parent, child]
		,
		$Failed
		,
		Message[SetSuper::cyclic, parent, child, {child, parent}]
		,
		TestID -> "Cyclic inheritance: cycle lenght 2: SetSuper evaluation"
	];
	
	Test[
		ToString @ Definition[parent]
		,
		parentOldDefinition
		,
		TestID -> "Cyclic inheritance: cycle lenght 2: parent definition"
	];
	Test[
		ToString @ Definition[child]
		,
		childOldDefinition
		,
		TestID -> "Cyclic inheritance: cycle lenght 2: child definition"
	];
]


Module[
	{
		grandParent, grandParentOldDefinition,
		parent, parentOldDefinition,
		child, childOldDefinition
	}
	,
	declareMockObject[grandParent];
	declareMockObject[parent, grandParent];
	declareMockObject[child, parent];
	
	{grandParentOldDefinition, parentOldDefinition, childOldDefinition} =
		ToString[Definition[#]]& /@ {grandParent, parent, child};
	
	
	Test[
		SetSuper[grandParent, child]
		,
		$Failed
		,
		Message[SetSuper::cyclic,
			grandParent, child, {child, parent, grandParent}
		]
		,
		TestID -> "Cyclic inheritance: cycle lenght 3: SetSuper evaluation"
	];
	
	Test[
		ToString @ Definition[grandParent]
		,
		grandParentOldDefinition
		,
		TestID -> "Cyclic inheritance: cycle lenght 3: grandParent definition"
	];
	Test[
		ToString @ Definition[parent]
		,
		parentOldDefinition
		,
		TestID -> "Cyclic inheritance: cycle lenght 3: parent definition"
	];
	Test[
		ToString @ Definition[child]
		,
		childOldDefinition
		,
		TestID -> "Cyclic inheritance: cycle lenght 3: child definition"
	];
]


Module[
	{obj, objOldDefinition, oldDefinitionObject}
	,
	declareMockObject[obj];
	
	{objOldDefinition, oldDefinitionObject} =
		ToString[Definition[#]]& /@ {obj, Object};
	
	Test[
		SetSuper[Object, obj]
		,
		$Failed
		,
		Message[SetSuper::cyclic, Object, obj, {obj, Object}]
		,
		TestID ->
			"Cyclic inheritance: set super of Object: SetSuper evaluation"
	];
	
	Test[
		ToString @ Definition[obj]
		,
		objOldDefinition
		,
		TestID -> "Cyclic inheritance: set super of Object: obj definition"
	];
	Test[
		ToString @ Definition[Object]
		,
		oldDefinitionObject
		,
		TestID -> "Cyclic inheritance: set super of Object: Object definition"
	];
]


Module[
	{oldDefinitionObject}
	,
	oldDefinitionObject = ToString[Definition[Object]];
	
	Test[
		SetSuper[Object, Object]
		,
		$Failed
		,
		Message[SetSuper::cyclic, Object, Object, {Object}]
		,
		TestID -> "Cyclic inheritance: set Object as super of Object: \
SetSuper evaluation"
	];
	
	Test[
		ToString @ Definition[Object]
		,
		oldDefinitionObject
		,
		TestID -> "Cyclic inheritance: set Object as super of Object: \
Object definition"
	];
]


(* ::Subsubsection:: *)
(*Successful*)


Module[
	{child, parent, parentOldDefinition}
	,
	declareMockObject[parent];
	declareMockObject[child];
	
	parentOldDefinition = ToString[Definition[parent]];

	Test[
		SetSuper[child, parent]
		,
		Null
		,
		TestID -> "Successful: SetSuper evaluation"
	];

	Test[
		FilterRules[UpValues[child], HoldPattern[HoldPattern][_Super]]
		,
		{HoldPattern[Super[child]] :> parent}
		,
		TestID -> "Successful: child has proper super up value"
	];
	TestMatch[
		DownValues[child]
		,
		inheritanceDownValues[child, parent]
		,
		TestID -> "Successful: child has proper inheritance down values"
	];
	
	Test[
		ToString[Definition[parent]]
		,
		parentOldDefinition
		,
		TestID -> "Successful: parent definition"
	];
]


Module[
	{child, parent, parentOldDefinition, oldDefinitionObject}
	,
	declareMockObject[parent];
	declareMockObject[child, parent];
	
	{parentOldDefinition, oldDefinitionObject} =
		ToString[Definition[#]]& /@ {parent, Object};

	Test[
		SetSuper[child, Object]
		,
		Null
		,
		TestID -> "Successful: remove parent: SetSuper evaluation"
	];
	
	Test[
		FilterRules[UpValues[child], HoldPattern[HoldPattern][_Super]]
		,
		{HoldPattern[Super[child]] :> Object}
		,
		TestID -> "Successful: remove parent: child has proper super up value"
	];
	TestMatch[
		DownValues[child]
		,
		inheritanceDownValues[child, Object]
		,
		TestID -> "Successful: remove parent: \
child has proper inheritance down values"
	];
	
	Test[
		ToString[Definition[parent]]
		,
		parentOldDefinition
		,
		TestID -> "Successful: remove parent: parent definition"
	];
	Test[
		ToString @ Definition[Object]
		,
		oldDefinitionObject
		,
		TestID -> "Successful: remove parent: Object definition"
	];
]


(* ::Subsection:: *)
(*Messages*)


Test[
	ToString @ StringForm[SetSuper::cyclic,
		HoldForm[a], HoldForm[b], HoldForm[{a, b, c}]
	]
	,
	"Object a can't inherit from object b, since it would lead to inheritance \
cycle {a, b, c}."
	,
	TestID -> "Messages: cyclic"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
