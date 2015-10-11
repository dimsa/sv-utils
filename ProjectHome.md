### Some useful Delphi classes and utilities for easier programming. ###


Current units:
  * **[SQLite3 4 Delphi](Intro.md)** - cross platform SQLite implementation for Delphi. [Checkout](http://code.google.com/p/sv-utils/source/checkout) to get the latest sources.
  * **svTrees** - data structure for building "virtually" [TVirtualStringTree](http://code.google.com/p/virtual-treeview/).
  * **svGenericTrees** - same as svTrees but implemented using generics.
  * **TColorVisualizer** - debugger visualizer for TColor type.
![http://www.soundvibe.net/wp-content/uploads/2011/01/ColorVisualizer1-150x129.jpg](http://www.soundvibe.net/wp-content/uploads/2011/01/ColorVisualizer1-150x129.jpg)
  * **TSVButtonGroup** - visual component (upgraded native TButtonGroup with better painting, html captions, etc.).
![http://sv-utils.googlecode.com/svn/trunk/img/svButtonGroup.jpg](http://sv-utils.googlecode.com/svn/trunk/img/svButtonGroup.jpg)
  * **TPathBuilder** - record for dynamically building cross platform file paths. Uses fluent interface.
  * **TSvSerializer** - very powerful serializer class which can serialize almost any data type (classes, records, arrays, sets, enumerators, variants, even TDictionary<TKey, TRecordType> or [TDataSet](SerializeDataset.md), etc.). Attributes can be used to specify which properties of a class should be serialized. Currently uses [JSON](http://www.json.org) format, XML could be added in the future. Multiple objects can be serialized at once. _(relocated to [delphi-oop](https://code.google.com/p/delphi-oop/) repository)_
  * **Design Patterns** - Software design patters (supports >= Delphi 2010) which uses new Delphi language features. Currently implemented patterns: Factory, Multiton. _(relocated to [delphi-oop](https://code.google.com/p/delphi-oop/) repository)_
  * **SvBindings** - extensions to automate data binding. Uses [DSharp](http://code.google.com/p/delphisorcery/) data binding library. _(relocated to [delphi-oop](https://code.google.com/p/delphi-oop/) repository)_

---

**Note:** Core library units (including TSvSerializer, design patterns, SvBindings) relocated to [delphi-oop](https://code.google.com/p/delphi-oop/) repository.


**Supported Delphi versions:** Delphi 2009 and later.