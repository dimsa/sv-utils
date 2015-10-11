# SQLite3 4 Delphi #

### History ###

Based on [Tim Anderson's](http://www.itwriting.com/blog/?page_id=659) SQLite wrapper.

### Features ###
  * Support multiple platforms (currently supports Windows, Mac OSX, iOS (_not tested yet_))
  * Full unicode support
  * 64 bit support
  * Takes advantage of the new Delphi language features (Generics, closures, etc.)
  * Fast
  * Simple to use
  * Lightweight
  * Supports adding [custom user functions](UserFunc.md)
  * Can fetch data into your own data structure
  * Unit tested (Unit test coverage is 76% overall)
  * Have TDataset descendant (TSQLiteDataset) for using it in DB aware VCL projects
  * Database encryption support (for encryption to work you must use different sqlite3 library file because default one does not support it. You can get it from  http://system.data.sqlite.org/index.html/doc/trunk/www/index.wiki or http://sourceforge.net/projects/wxcode/files/Components/wxSQLite3/  )
  * Prepared statements and SQLite uni-directional tables can be retrieved from database as interfaces so you don't need to free them.
  * Database authorization support (http://sqlite.org/c3ref/set_authorizer.html)