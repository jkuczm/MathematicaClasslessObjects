(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage[
	"ClasslessObjects`Tests`Integration`SettersNoInheritance`",
	{"MUnit`"}
]


Get["ClasslessObjects`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Unbound inheritable*)


Module[
	{setterObj, otherObj, member, setter, arg}
	,
	DeclareObject[setterObj];
	DeclareObject[otherObj];
	setterObj@setter[x_] := ($self@member = {$self, x});
	
	Test[
		setterObj@setter[arg]
		,
		{setterObj, arg}
		,
		TestID -> "Unbound inheritable: setter implicit self: call setter"
	];
	
	Test[
		setterObj@member
		,
		{setterObj, arg}
		,
		TestID -> "Unbound inheritable: setter implicit self: \
call member setterObj"
	];
	Test[
		setterObj[member, otherObj]
		,
		{setterObj, arg}
		,
		TestID -> "Unbound inheritable: setter implicit self: \
call member setterObj: explicit self"
	];
]

Module[
	{setterObj, explSelfObj, otherObj, member, setter, arg}
	,
	DeclareObject[setterObj];
	DeclareObject[explSelfObj];
	DeclareObject[otherObj];
	setterObj@setter[x_] := ($self@member = {$self, x});
	
	Test[
		setterObj[setter[arg], explSelfObj]
		,
		{explSelfObj, arg}
		,
		TestID -> "Unbound inheritable: setter explicit self: call setter"
	];
	
	Test[
		setterObj@member
		,
		$Failed
		,
		Message[Object::objectMember, setterObj, member, {Object}]
		,
		TestID -> "Unbound inheritable: setter explicit self: \
call member setterObj"
	];
	Test[
		setterObj[member, otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member, {Object}]
		,
		TestID -> "Unbound inheritable: setter explicit self: \
call member setterObj: explicit self otherObj"
	];
	Test[
		setterObj[member, explSelfObj]
		,
		$Failed
		,
		Message[Object::objectMember, explSelfObj, member, {Object}]
		,
		TestID -> "Unbound inheritable: setter explicit self: \
call member setterObj: explicit self explSelfObj"
	];
	
	Test[
		explSelfObj@member
		,
		{explSelfObj, arg}
		,
		TestID -> "Unbound inheritable: setter explicit self: \
call member explSelfObj"
	];
	Test[
		explSelfObj[member, otherObj]
		,
		{explSelfObj, arg}
		,
		TestID -> "Unbound inheritable: setter explicit self: \
call member explSelfObj: explicit self otherObj"
	];
	Test[
		explSelfObj[member, setterObj]
		,
		{explSelfObj, arg}
		,
		TestID -> "Unbound inheritable: setter explicit self: \
call member explSelfObj: explicit self setterObj"
	];
]


(* ::Subsection:: *)
(*Bound inheritable*)


Module[
	{setterObj, otherObj, member, setter, arg, arg2}
	,
	DeclareObject[setterObj];
	DeclareObject[otherObj];
	setterObj@setter[x_] := ($self@member[y_] := {$self, x, y});
	
	Test[
		setterObj@setter[arg]
		,
		Null
		,
		TestID -> "Bound inheritable: setter implicit self: call setter"
	];
	
	Test[
		setterObj@member[arg2]
		,
		{setterObj, arg, arg2}
		,
		TestID -> "Bound inheritable: setter implicit self: \
call member setterObj"
	];
	Test[
		setterObj[member[arg2], otherObj]
		,
		{otherObj, arg, arg2}
		,
		TestID -> "Bound inheritable: setter implicit self: \
call member setterObj: explicit self"
	];
]

Module[
	{setterObj, explSelfObj, otherObj, member, setter, arg, arg2}
	,
	DeclareObject[setterObj];
	DeclareObject[explSelfObj];
	DeclareObject[otherObj];
	setterObj@setter[x_] := ($self@member[y_] := {$self, x, y});
	
	Test[
		setterObj[setter[arg], explSelfObj]
		,
		Null
		,
		TestID -> "Bound inheritable: setter explicit self: call setter"
	];
	
	Test[
		setterObj@member[arg2]
		,
		$Failed
		,
		Message[Object::objectMember, setterObj, member[arg2], {Object}]
		,
		TestID -> "Bound inheritable: setter explicit self: \
call member setterObj"
	];
	Test[
		setterObj[member[arg2], otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member[arg2], {Object}]
		,
		TestID -> "Bound inheritable: setter explicit self: \
call member setterObj: explicit self otherObj"
	];
	Test[
		setterObj[member[arg2], explSelfObj]
		,
		$Failed
		,
		Message[Object::objectMember, explSelfObj, member[arg2], {Object}]
		,
		TestID -> "Bound inheritable: setter explicit self: \
call member setterObj: explicit self explSelfObj"
	];
	
	Test[
		explSelfObj@member[arg2]
		,
		{explSelfObj, arg, arg2}
		,
		TestID -> "Bound inheritable: setter explicit self: \
call member explSelfObj"
	];
	Test[
		explSelfObj[member[arg2], otherObj]
		,
		{otherObj, arg, arg2}
		,
		TestID -> "Bound inheritable: setter explicit self: \
call member explSelfObj: explicit self otherObj"
	];
	Test[
		explSelfObj[member[arg2], setterObj]
		,
		{setterObj, arg, arg2}
		,
		TestID -> "Bound inheritable: setter explicit self: \
call member explSelfObj: explicit self setterObj"
	];
]


(* ::Subsection:: *)
(*Non-inheritable*)


Module[
	{setterObj, otherObj, member, setter, arg}
	,
	DeclareObject[setterObj];
	DeclareObject[otherObj];
	setterObj@setter[x_] :=
		WithOrdinaryObjectSet[$self,
			$self@member = {$self, x}
		];
	
	Test[
		setterObj@setter[arg]
		,
		{setterObj, arg}
		,
		TestID -> "Non-inheritable: setter implicit self: call setter"
	];
	
	Test[
		setterObj@member
		,
		{setterObj, arg}
		,
		TestID -> "Non-inheritable: setter implicit self: \
call member setterObj"
	];
	Test[
		setterObj[member, otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member, {Object}]
		,
		TestID -> "Non-inheritable: setter implicit self: \
call member setterObj: explicit self"
	];
]

Module[
	{setterObj, explSelfObj, otherObj, member, setter, arg}
	,
	DeclareObject[setterObj];
	DeclareObject[explSelfObj];
	DeclareObject[otherObj];
	setterObj@setter[x_] :=
		WithOrdinaryObjectSet[$self,
			$self@member = {$self, x}
		];
	
	Test[
		setterObj[setter[arg], explSelfObj]
		,
		{explSelfObj, arg}
		,
		TestID -> "Non-inheritable: setter explicit self: call setter"
	];
	
	Test[
		setterObj@member
		,
		$Failed
		,
		Message[Object::objectMember, setterObj, member, {Object}]
		,
		TestID -> "Non-inheritable: setter explicit self: \
call member setterObj"
	];
	Test[
		setterObj[member, otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member, {Object}]
		,
		TestID -> "Non-inheritable: setter explicit self: \
call member setterObj: explicit self otherObj"
	];
	Test[
		setterObj[member, explSelfObj]
		,
		$Failed
		,
		Message[Object::objectMember, explSelfObj, member, {Object}]
		,
		TestID -> "Non-inheritable: setter explicit self: \
call member setterObj: explicit self explSelfObj"
	];
	
	Test[
		explSelfObj@member
		,
		{explSelfObj, arg}
		,
		TestID -> "Non-inheritable: setter explicit self: \
call member explSelfObj"
	];
	Test[
		explSelfObj[member, otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member, {Object}]
		,
		TestID -> "Non-inheritable: setter explicit self: \
call member explSelfObj: explicit self otherObj"
	];
	Test[
		explSelfObj[member, setterObj]
		,
		$Failed
		,
		Message[Object::objectMember, setterObj, member, {Object}]
		,
		TestID -> "Non-inheritable: setter explicit self: \
call member explSelfObj: explicit self setterObj"
	];
]


(* ::Subsection:: *)
(*Inheritable-only*)


Module[
	{setterObj, otherObj, member, setter, arg, arg2}
	,
	DeclareObject[setterObj];
	DeclareObject[otherObj];
	setterObj@setter[x_] :=
		WithOrdinaryObjectSet[$self,
			$self[member[y_], self_] := {$self, self, x, y}
		];
	
	Test[
		setterObj@setter[arg]
		,
		Null
		,
		TestID -> "Inheritable-only: setter implicit self: call setter"
	];
	
	Test[
		setterObj@member[arg2]
		,
		$Failed
		,
		Message[Object::objectMember, setterObj, member[arg2], {Object}]
		,
		TestID -> "Inheritable-only: setter implicit self: \
call member setterObj"
	];
	Test[
		setterObj[member[arg2], otherObj]
		,
		{$self, otherObj, arg, arg2}
		,
		TestID -> "Inheritable-only: setter implicit self: \
call member setterObj: explicit self"
	];
]

Module[
	{setterObj, explSelfObj, otherObj, member, setter, arg, arg2}
	,
	DeclareObject[setterObj];
	DeclareObject[explSelfObj];
	DeclareObject[otherObj];
	setterObj@setter[x_] :=
		WithOrdinaryObjectSet[$self,
			$self[member[y_], self_] := {$self, self, x, y}
		];
	
	Test[
		setterObj[setter[arg], explSelfObj]
		,
		Null
		,
		TestID -> "Inheritable-only: setter explicit self: call setter"
	];
	
	Test[
		setterObj@member[arg2]
		,
		$Failed
		,
		Message[Object::objectMember, setterObj, member[arg2], {Object}]
		,
		TestID -> "Inheritable-only: setter explicit self: \
call member setterObj"
	];
	Test[
		setterObj[member[arg2], otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member[arg2], {Object}]
		,
		TestID -> "Inheritable-only: setter explicit self: \
call member setterObj: explicit self otherObj"
	];
	Test[
		setterObj[member[arg2], explSelfObj]
		,
		$Failed
		,
		Message[Object::objectMember, explSelfObj, member[arg2], {Object}]
		,
		TestID -> "Inheritable-only: setter explicit self: \
call member setterObj: explicit self explSelfObj"
	];
	
	Test[
		explSelfObj@member[arg2]
		,
		$Failed
		,
		Message[Object::objectMember, explSelfObj, member[arg2], {Object}]
		,
		TestID -> "Inheritable-only: setter explicit self: \
call member explSelfObj"
	];
	Test[
		explSelfObj[member[arg2], otherObj]
		,
		{$self, otherObj, arg, arg2}
		,
		TestID -> "Inheritable-only: setter explicit self: \
call member explSelfObj: explicit self otherObj"
	];
	Test[
		explSelfObj[member[arg2], setterObj]
		,
		{$self, setterObj, arg, arg2}
		,
		TestID -> "Inheritable-only: setter explicit self: \
call member explSelfObj: explicit self setterObj"
	];
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
