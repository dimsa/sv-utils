<center><b>sv-utils</b></center><br />

<b>Projects:</b>
<ul>
<li>
svTrees – data structure (main unit svCollections.Trees.pas) for building TVirtualStringTree virtually. Just create TsvTree, add   childs and use TVirtualStringTree's OnInitNode and OnInitChildren events. Demo project included. Third party Collections open source library included. Needs installed TVirtualStringTree.
</li>
<li>
TColorVisualizer – debugger visualizer for Delphi 2010 and newer. It allows you to visually see TColor's representation in the debugger. Just compile and install the package. 
</li>
</ul><br />

Project is imported from <a href="https://code.google.com/p/sv-utils/">https://code.google.com/p/sv-utils/</a><br />
Resume by Dimsa: Look question on StackOverflow: 
<a href="http://stackoverflow.com/questions/10559080/nested-json-object-deserializing-using-delphi-2012">
http://stackoverflow.com/questions/10559080/nested-json-object-deserializing-using-delphi-2012
</a>

With SvSerializer you can do like this:<br />
<i>
<b>TDropbox</b> = class<br />
  <b>private</b><br />
    &nbsp;&nbsp;FHash: string;<br />
    &nbsp;&nbsp;Fthumb_exists: Boolean;<br />
    &nbsp;&nbsp;...<br />
  <b>public</b><br />
    &nbsp;&nbsp;[SvSerialize]<br />
    &nbsp;&nbsp;property Hash: string read FHash write FHash;<br />
    &nbsp;&nbsp;[SvSerialize]<br />
    &nbsp;&nbsp;property thumb_exists: Boolean read Fthumb_exists write Fthumb_exists;<br />
    &nbsp;&nbsp;... <br />
	</i>
<br />
Then you can add Object Instance to Serializer<br /><br />
<i>
FSerializer := TSvSerializer.Create();<br />
FDropboxFile := TDropbox.Create;<br />
FSerializer.AddObject('', FDropboxFile);<br /><br />
</i>
And now you can serialize/deserialize this object through SvSerializer:<br /><br />
<i>
FSerializer.DeSerialize(<br />
	&nbsp;&nbsp;mmo1.Lines.Text{your json string, stream or filename}, <br />
	&nbsp;&nbsp;TEncoding.UTF8{if it is string you must specify the encoding});<br />
    &nbsp;&nbsp;//After this line your FDropBoxFile's properties are filled from your json string</i><br /><br />
	
SvSerializer is easy to use and is very similar to DataContract Serializer in C# or Serializer by NewtonSoft.<br /> So you can easily go to SvSerializer in Delphi from C#<br /> Good automatic Serializer in Delphi
