# SQLite database encryption #

It is very easy to encrypt your database using "SQLite 4 Delphi". Just specify an optional password parameter when creating database object (if password parameter is left empty, encryption won't be used):

```
//create and open encrypted db
   DB := TSQLiteDatabase.Create('encrypted.db', seUTF8, 'password');
//create and open unencrypted db
   DB := TSQLiteDatabase.Create('encrypted.db');
```

Furthermore, you can change your encryption password later:

```
//change database password
  DB.ChangePassword('somenewpassword');
```

Notes:
For encryption to work you must use different sqlite3 library file because default one does not support it. You can get it from http://system.data.sqlite.org/index.html/doc/trunk/www/index.wiki or http://sourceforge.net/projects/wxcode/files/Components/wxSQLite3/. Simply rename it to sqlite3.dll (on Windows OS) and deploy it with your application.