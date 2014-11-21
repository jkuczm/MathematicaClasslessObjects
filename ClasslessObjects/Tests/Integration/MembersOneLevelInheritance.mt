(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage[
	"ClasslessObjects`Tests`Integration`MembersOneLevelInheritance`",
	{"MUnit`"}
]


Get["ClasslessObjects`"]


setUpObjects[obj_, parent_] := (
	DeclareObject[parent];
	DeclareObject[obj, parent];
)

setUpObjects[obj_, parent_, otherObj_] := (
	setUpObjects[obj, parent];
	DeclareObject[otherObj];
)


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Non-existing member*)


Module[
	{obj, parent, member, parentValue}
	,
	setUpObjects[obj, parent];
	parent@member = parentValue;
	
	Test[
		obj@member
		,
		parentValue
		,
		TestID -> "Non-existing member: unbound on parent: call"
	]
]
Module[
	{obj, parent, otherObj, member, parentValue}
	,
	setUpObjects[obj, parent, otherObj];
	parent@member = parentValue;
	
	Test[
		obj[member, otherObj]
		,
		parentValue
		,
		TestID -> "Non-existing member: unbound on parent: call: explicit self"
	]
]

Module[
	{obj, parent, member, parentValue}
	,
	setUpObjects[obj, parent];
	parent@member = parentValue;
	
	Test[
		obj@member =.
		,
		$Failed
		,
		Message[Unset::norep, obj@member, obj]
		,
		TestID -> "Non-existing member: unbound on parent: unset"
	]
]


Module[
	{obj, parent, member, parentValue}
	,
	setUpObjects[obj, parent];
	parent@member = {parentValue, $self};
	
	Test[
		obj@member
		,
		{parentValue, $self}
		,
		TestID -> "Non-existing member: unbound with $self on parent: call"
	]
]
Module[
	{obj, parent, otherObj, member, parentValue}
	,
	setUpObjects[obj, parent, otherObj];
	parent@member = {parentValue, $self};
	
	Test[
		obj[member, otherObj]
		,
		{parentValue, $self}
		,
		TestID -> "Non-existing member: unbound with $self on parent: call: \
explicit self"
	]
]

Module[
	{obj, parent, member, parentValue}
	,
	setUpObjects[obj, parent];
	parent@member = {parentValue, $self};
	
	Test[
		obj@member =.
		,
		$Failed
		,
		Message[Unset::norep, obj@member, obj]
		,
		TestID -> "Non-existing member: unbound with $self on parent: unset"
	]
]


Module[
	{obj, parent, member, parentValue, arg}
	,
	setUpObjects[obj, parent];
	parent@member[i_Symbol] := {parentValue, i};
	
	Test[
		obj@member[arg]
		,
		{parentValue, arg}
		,
		TestID -> "Non-existing member: bound on parent: call"
	]
]
Module[
	{obj, parent, otherObj, member, parentValue, arg}
	,
	setUpObjects[obj, parent, otherObj];
	parent@member[i_Symbol] := {parentValue, i};
	
	Test[
		obj[member[arg], otherObj]
		,
		{parentValue, arg}
		,
		TestID -> "Non-existing member: bound on parent: call: explicit self"
	]
]

Module[
	{obj, parent, member, parentValue}
	,
	setUpObjects[obj, parent];
	parent@member[i_Symbol] := {parentValue, i};
	
	Test[
		obj@member[i_Symbol] =.
		,
		$Failed
		,
		Message[Unset::norep,
			obj@member[HoldPattern[Pattern][i, HoldPattern[Blank][Symbol]]],
			obj
		]
		,
		TestID -> "Non-existing member: bound on parent: unset"
	]
]


Module[
	{obj, parent, member, parentValue}
	,
	setUpObjects[obj, parent];
	parent@member[] := {parentValue, $self};
	
	Test[
		obj@member[]
		,
		{parentValue, obj}
		,
		TestID -> "Non-existing member: bound with $self on parent: call"
	]
]
Module[
	{obj, parent, otherObj, member, parentValue}
	,
	setUpObjects[obj, parent, otherObj];
	parent@member[] := {parentValue, $self};
	
	Test[
		obj[member[], otherObj]
		,
		{parentValue, otherObj}
		,
		TestID -> "Non-existing member: bound with $self on parent: call: \
explicit self"
	]
]

Module[
	{obj, parent, member, parentValue}
	,
	setUpObjects[obj, parent];
	parent@member[] := {parentValue, $self};
	
	Test[
		obj@member[] =.
		,
		$Failed
		,
		Message[Unset::norep, obj@member[], obj]
		,
		TestID -> "Non-existing member: bound with $self on parent: unset"
	]
]


Module[
	{obj, parent, member, parentValue}
	,
	setUpObjects[obj, parent];
	WithOrdinaryObjectSet[parent,
		parent@member = parentValue
	];
	
	Test[
		obj@member
		,
		$Failed
		,
		Message[Object::objectMember, obj, member, {parent, Object}]
		,
		TestID -> "Non-existing member: non-inheritable on parent: call"
	]
]
Module[
	{obj, parent, otherObj, member, parentValue}
	,
	setUpObjects[obj, parent, otherObj];
	WithOrdinaryObjectSet[parent,
		parent@member = parentValue
	];
	
	Test[
		obj[member, otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member, {Object}]
		,
		TestID -> "Non-existing member: non-inheritable on parent: call: \
explicit self"
	]
]

Module[
	{obj, parent, member, parentValue}
	,
	setUpObjects[obj, parent];
	WithOrdinaryObjectSet[parent,
		parent@member = parentValue
	];
	
	Test[
		obj@member =.
		,
		$Failed
		,
		Message[Unset::norep, obj@member, obj]
		,
		TestID -> "Non-existing member: non-inheritable on parent: unset"
	]
]


Module[
	{obj, parent, member, parentValue}
	,
	setUpObjects[obj, parent];
	WithOrdinaryObjectSet[parent,
		parent[member[], self_] := {parentValue, self}
	];
	
	Test[
		obj@member[]
		,
		{parentValue, obj}
		,
		TestID -> "Non-existing member: inheritable-only on parent: call"
	]
]
Module[
	{obj, parent, otherObj, member, parentValue}
	,
	setUpObjects[obj, parent, otherObj];
	WithOrdinaryObjectSet[parent,
		parent[member[], self_] := {parentValue, self}
	];
	
	Test[
		obj[member[], otherObj]
		,
		{parentValue, otherObj}
		,
		TestID -> "Non-existing member: inheritable-only on parent: call: \
explicit self"
	]
]

Module[
	{obj, parent, member, parentValue}
	,
	setUpObjects[obj, parent];
	WithOrdinaryObjectSet[parent,
		parent[member[], self_] := {parentValue, self}
	];
	
	Test[
		obj@member[] =.
		,
		$Failed
		,
		Message[Unset::norep, obj@member[], obj]
		,
		TestID -> "Non-existing member: inheritable-only on parent: unset"
	]
]


(* ::Subsection:: *)
(*Unbound member*)


Module[
	{obj, parent, otherObj, member, value, parentValue}
	,
	setUpObjects[obj, parent, otherObj];
	parent@member = parentValue;
	
	
	Test[
		obj@member = value
		,
		value
		,
		TestID -> "Unbound member: set"
	];
	
	Test[
		obj@member
		,
		value
		,
		TestID -> "Unbound member: call existing"
	];
	Test[
		obj[member, otherObj]
		,
		value
		,
		TestID -> "Unbound member: call existing: explicit self"
	];
	
	Test[
		obj@member =.
		,
		Null
		,
		TestID -> "Unbound member: unset"
	];
	
	Test[
		obj@member
		,
		parentValue
		,
		TestID -> "Unbound member: call unset"
	];
	Test[
		obj[member, otherObj]
		,
		parentValue
		,
		TestID -> "Unbound member: call unset: explicit self"
	];
	
	
	parent@member =.;
	
	Test[
		obj@member
		,
		$Failed
		,
		Message[Object::objectMember, obj, member, {parent, Object}]
		,
		TestID -> "Unbound member: call unset from self and parent"
	];
	Test[
		obj[member, otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member, {Object}]
		,
		TestID ->
			"Unbound member: call unset from self and parent: explicit self"
	];
]


(* ::Subsection:: *)
(*Unbound member with $self*)


Module[
	{obj, parent, otherObj, member, value, parentValue}
	,
	setUpObjects[obj, parent, otherObj];
	parent@member = {parentValue, $self};
	
	
	Test[
		obj@member = {value, $self}
		,
		{value, $self}
		,
		TestID -> "Unbound member with $self: set"
	];
	
	TestMatch[
		obj@member
		,
		HoldPattern[{value, $self}]
		,
		TestID -> "Unbound member with $self: call existing"
	];
	TestMatch[
		obj[member, otherObj]
		,
		HoldPattern[{value, $self}]
		,
		TestID -> "Unbound member with $self: call existing: explicit self"
	];
	
	Test[
		obj@member =.
		,
		Null
		,
		TestID -> "Unbound member with $self: unset"
	];
	
	Test[
		obj@member
		,
		{parentValue, $self}
		,
		TestID -> "Unbound member with $self: call unset"
	];
	Test[
		obj[member, otherObj]
		,
		{parentValue, $self}
		,
		TestID -> "Unbound member with $self: call unset: explicit self"
	];
	
	
	parent@member =.;
	
	Test[
		obj@member
		,
		$Failed
		,
		Message[Object::objectMember, obj, member, {parent, Object}]
		,
		TestID -> "Unbound member with $self: call unset from self and parent"
	];
	Test[
		obj[member, otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member, {Object}]
		,
		TestID -> "Unbound member with $self: \
call unset from self and parent: explicit self"
	];
]


(* ::Subsection:: *)
(*Bound member*)


Module[
	{obj, parent, otherObj, member, value, newValue, parentValue, arg}
	,
	setUpObjects[obj, parent, otherObj];
	parent@member[i_Symbol] := {parentValue, i};
	
	
	Test[
		obj@member[i_Symbol] := {value, i}
		,
		Null
		,
		TestID -> "Bound member: set"
	];
	
	Test[
		obj@member[arg]
		,
		{value, arg}
		,
		TestID -> "Bound member: call existing"
	];
	Test[
		obj[member[arg], otherObj]
		,
		{value, arg}
		,
		TestID -> "Bound member: call existing: explicit self"
	];
	
	Test[
		obj@member["test string"]
		,
		$Failed
		,
		Message[Object::objectMember,
			obj, member["test string"], {parent, Object}
		]
		,
		TestID -> "Bound member: call non-existing with head same as existing"
	];
	Test[
		obj[member["test string"], otherObj]
		,
		$Failed
		,
		Message[Object::objectMember,
			otherObj, member["test string"], {Object}
		]
		,
		TestID -> "Bound member: \
call non-existing with head same as existing: explicit self"
	];
	
	Test[
		obj@member[i_Symbol] =.
		,
		Null
		,
		TestID -> "Bound member: unset"
	];
	
	Test[
		obj@member[arg]
		,
		{parentValue, arg}
		,
		TestID -> "Bound member: call unset"
	];
	Test[
		obj[member[arg], otherObj]
		,
		{parentValue, arg}
		,
		TestID -> "Bound member: call unset: explicit self"
	];
	
	Test[
		obj@member[x_Real] := {newValue, x}
		,
		Null
		,
		TestID -> "Bound member: set with head same as existing on parent"
	];
	
	Test[
		obj@member[1.5]
		,
		{newValue, 1.5}
		,
		TestID ->
			"Bound member: call existing with head same as existing on parent"
	];
	Test[
		obj[member[1.5], otherObj]
		,
		{newValue, 1.5}
		,
		TestID -> "Bound member: \
call existing with head same as existing on parent: explicit self"
	];
	
	
	parent@member[i_Symbol] =.;
	
	Test[
		obj@member[arg]
		,
		$Failed
		,
		Message[Object::objectMember, obj, member[arg], {parent, Object}]
		,
		TestID -> "Bound member: call unset from self and parent"
	];
	Test[
		obj[member[arg], otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member[arg], {Object}]
		,
		TestID ->
			"Bound member: call unset from self and parent: explicit self"
	];
]


(* ::Subsection:: *)
(*Bound member with $self*)


Module[
	{obj, parent, otherObj, member, value, parentValue, arg}
	,
	setUpObjects[obj, parent, otherObj];
	parent@member[] := {parentValue, $self};
	
	
	Test[
		obj@member[] := {value, $self}
		,
		Null
		,
		TestID -> "Bound member with $self: set"
	];
	
	Test[
		obj@member[]
		,
		{value, obj}
		,
		TestID -> "Bound member with $self: call existing"
	];
	Test[
		obj[member[], otherObj]
		,
		{value, otherObj}
		,
		TestID -> "Bound member with $self: call existing: explicit self"
	];
	
	Test[
		obj@member[arg]
		,
		$Failed
		,
		Message[Object::objectMember, obj, member[arg], {parent, Object}]
		,
		TestID -> "Bound member with $self: \
call non-existing with head same as existing"
	];
	Test[
		obj[member[arg], otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member[arg], {Object}]
		,
		TestID -> "Bound member with $self: \
call non-existing with head same as existing: explicit self"
	];
	
	Test[
		obj@member[] =.
		,
		Null
		,
		TestID -> "Bound member with $self: unset"
	];
	
	Test[
		obj@member[]
		,
		{parentValue, obj}
		,
		TestID -> "Bound member with $self: call unset"
	];
	Test[
		obj[member[], otherObj]
		,
		{parentValue, otherObj}
		,
		TestID -> "Bound member with $self: call unset: explicit self"
	];
	
	Test[
		obj@member[x_] := {value, $self, x}
		,
		Null
		,
		TestID ->
			"Bound member with $self: set with head same as existing on parent"
	];
	
	Test[
		obj@member[arg]
		,
		{value, obj, arg}
		,
		TestID -> "Bound member with $self: \
call existing with head same as existing on parent"
	];
	Test[
		obj[member[arg], otherObj]
		,
		{value, otherObj, arg}
		,
		TestID -> "Bound member with $self: \
call existing with head same as existing on parent: explicit self"
	];
	
	
	parent@member[] =.;
	
	Test[
		obj@member[]
		,
		$Failed
		,
		Message[Object::objectMember, obj, member[], {parent, Object}]
		,
		TestID -> "Bound member with $self: call unset from self and parent"
	];
	Test[
		obj[member[], otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member[], {Object}]
		,
		TestID -> "Bound member with $self: \
call unset from self and parent: explicit self"
	];
]


(* ::Subsection:: *)
(*Non-inheritable member*)


Module[
	{obj, parent, otherObj, member, value, parentValue}
	,
	setUpObjects[obj, parent, otherObj];
	parent@member = parentValue;
	
	
	Test[
		WithOrdinaryObjectSet[obj,
			obj@member = value
		]
		,
		value
		,
		TestID -> "Non-inheritable member: set"
	];
	
	Test[
		obj@member
		,
		value
		,
		TestID -> "Non-inheritable member: call existing"
	];
	Test[
		obj[member, otherObj]
		,
		parentValue
		,
		TestID -> "Non-inheritable member: call existing: explicit self"
	];
	
	Test[
		obj@member =.
		,
		Null
		,
		TestID -> "Non-inheritable member: unset"
	];
	
	Test[
		obj@member
		,
		parentValue
		,
		TestID -> "Non-inheritable member: call unset"
	];
	Test[
		obj[member, otherObj]
		,
		parentValue
		,
		TestID -> "Non-inheritable member: call unset: explicit self"
	];
	
	
	parent@member =.;
	
	Test[
		obj@member
		,
		$Failed
		,
		Message[Object::objectMember, obj, member, {parent, Object}]
		,
		TestID ->
			"Non-inheritable member: call unset from self and parent"
	];
	Test[
		obj[member, otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member, {Object}]
		,
		TestID -> "Non-inheritable member: call unset from self and parent: \
explicit self"
	];
]


(* ::Subsection:: *)
(*Inheritable-only member*)


Module[
	{obj, parent, otherObj, member, value, parentValue, arg}
	,
	setUpObjects[obj, parent, otherObj];
	parent@member[] := {parentValue, $self};
	
	
	Test[
		WithOrdinaryObjectSet[obj,
			obj[member[], self_] := {value, self}
		]
		,
		Null
		,
		TestID -> "Inheritable-only member: set"
	];
	
	Test[
		obj@member[]
		,
		{parentValue, obj}
		,
		TestID -> "Inheritable-only member: call existing"
	];
	Test[
		obj[member[], otherObj]
		,
		{value, otherObj}
		,
		TestID -> "Inheritable-only member: call existing: explicit self"
	];
	
	Test[
		obj@member[arg]
		,
		$Failed
		,
		Message[Object::objectMember, obj, member[arg], {parent, Object}]
		,
		TestID -> "Inheritable-only member: \
call non-existing with head same as existing"
	];
	Test[
		obj[member[arg], otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member[arg], {Object}]
		,
		TestID -> "Inheritable-only member: \
call non-existing with head same as existing: explicit self"
	];
	
	Test[
		obj@member[] =.
		,
		Null
		,
		TestID -> "Inheritable-only member: unset"
	];
	
	Test[
		obj@member[]
		,
		{parentValue, obj}
		,
		TestID -> "Inheritable-only member: call unset"
	];
	Test[
		obj[member[], otherObj]
		,
		{parentValue, otherObj}
		,
		TestID -> "Inheritable-only member: call unset: explicit self"
	];
	
	Test[
		WithOrdinaryObjectSet[obj,
			obj[member[x_], self_] := {value, self, x}
		]
		,
		Null
		,
		TestID ->
			"Inheritable-only member: set with head same as existing on parent"
	];
	
	Test[
		obj@member[arg]
		,
		$Failed
		,
		Message[Object::objectMember, obj, member[arg], {parent, Object}]
		,
		TestID -> "Inheritable-only member: \
call existing with head same as existing on parent"
	];
	Test[
		obj[member[arg], otherObj]
		,
		{value, otherObj, arg}
		,
		TestID -> "Inheritable-only member: \
call existing with head same as existing on parent: explicit self"
	];
	
	
	parent@member[] =.;
	
	Test[
		obj@member[]
		,
		$Failed
		,
		Message[Object::objectMember, obj, member[], {parent, Object}]
		,
		TestID -> "Inheritable-only member: call unset from self and parent"
	];
	Test[
		obj[member[], otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member[], {Object}]
		,
		TestID -> "Inheritable-only member: call unset from self and parent: \
explicit self"
	];
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
