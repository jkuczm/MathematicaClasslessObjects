(* ::Package:: *)

Get["https://raw.githubusercontent.com/jkuczm/MathematicaBootstrapInstaller/v0.1.1/BootstrapInstaller.m"]


BootstrapInstall[
	"ClasslessObjects",
	"https://github.com/jkuczm/MathematicaClasslessObjects/releases/download/v0.1.1/ClasslessObjects.zip",
	"AdditionalFailureMessage" ->
		Sequence[
			"You can ",
			Hyperlink[
				"install ClasslessObjects package manually",
				"https://github.com/jkuczm/MathematicaClasslessObjects#manual-installation"
			],
			"."
		]
]
