# Classless Objects

[![release v0.1.1](http://img.shields.io/badge/release-v0.1.1-blue.svg)](https://github.com/jkuczm/MathematicaClasslessObjects/releases/latest)
[![Semantic Versioning](http://jkuczm.github.io/media/images/SemVer-2.0.0-brightgreen.svg)](http://semver.org/spec/v2.0.0.html)
[![license MIT](http://jkuczm.github.io/media/images/license-MIT-blue.svg)](https://github.com/jkuczm/MathematicaClasslessObjects/blob/master/LICENSE)
[![Mathematica 8.0 9.0 10.0](http://jkuczm.github.io/media/images/Mathematica-8.0_9.0_10.0-brightgreen.svg)](#compatibility)


Tools for creating classless objects, with prototype based inheritance, in
Mathematica.


* [Usage example](#usage-example)
* [Installation](#installation)
    * [Automatic installation](#automatic-installation)
    * [Manual installation](#manual-installation)
    * [No installation](#no-installation)
* [Compatibility](#compatibility)
* [Bugs and requests](#bugs-and-requests)
* [Contributing](#contributing)
* [License](#license)
* [Versioning](#versioning)



## Usage example

You can find usage examples in
[answer to "Once more on object orientation..." question](http://mathematica.stackexchange.com/questions/16869#65173)
on Mathematica Stack Exchange.



## Installation


### Automatic installation

To install ClasslessObjects package evaluate:
```Mathematica
Get["https://raw.githubusercontent.com/jkuczm/MathematicaClasslessObjects/master/BootstrapInstall.m"]
```

Note that this will also install
[ProjectInstaller](https://github.com/lshifr/ProjectInstaller)
package, if you don't have it already installed.

To load ClasslessObjects package evaluate: ``Needs["ClasslessObjects`"]``.


### Manual installation

1. Download latest released
   [ClasslessObjects.zip](https://github.com/jkuczm/MathematicaClasslessObjects/releases/download/v0.1.1/ClasslessObjects.zip)
   file.

2. Extract downloaded `ClasslessObjects.zip` to any directory which is on
   Mathematica `$Path`, e.g. to one obtained by evaluating
   `FileNameJoin[{$UserBaseDirectory,"Applications"}]`.

3. To load the package evaluate: ``Needs["ClasslessObjects`"]``


### No installation

To use package directly from the Web, without installation, evaluate:
```Mathematica
Get["https://raw.githubusercontent.com/jkuczm/MathematicaClasslessObjects/master/ClasslessObjects/ClasslessObjects.m"]
```



## Compatibility

This package contains extensive
[automatic test suite](https://github.com/jkuczm/MathematicaClasslessObjects/tree/master/ClasslessObjects/Tests).
Package is being tested with Mathematica versions 8.0, 9.0 and 10.0 on Linux.
Since it doesn't contain any OS specific code it should work with above
versions on all operating systems.

There's also no obvious reason for package not to work on earlier (6.0+)
versions of Mathematica.



## Bugs and requests

If you find any bugs or have feature request please create an
[issue on GitHub](https://github.com/jkuczm/MathematicaClasslessObjects/issues).



## Contributing

Feel free to fork and send pull requests.

If you want to use Ant scripts from this repository you will also need to
install [WWBCommon](https://github.com/jkuczm/WWBCommon) project.

All contributions are welcome!



## License

This package is released under
[The MIT License](https://github.com/jkuczm/MathematicaClasslessObjects/blob/master/LICENSE).


### Attribution

Parts of code of this project are inspired by
[OO package](https://gist.github.com/lshifr/4266126#file-oo-m) created by
[Leonid Shifrin](http://www.mathprogramming-intro.org/) used under
[MIT License](https://gist.github.com/lshifr/4266126#file-license).

Parts of code of this project are a derivative of code written by
[Leonid Shifrin](http://mathematica.stackexchange.com/users/81/leonid-shifrin)
in
[Once more on object orientation in Mathematica: does it have to be so hard?](http://mathematica.stackexchange.com/questions/16869)
thread on Mathematica Stack Exchange used under
[Creative Commons Attribution-ShareAlike 3.0 Unported License](http://creativecommons.org/licenses/by-sa/3.0/).



## Versioning

Releases of this package will be numbered using
[Semantic Versioning guidelines](http://semver.org/).
