{*******************************************************}
{       uVikButtonGroupUtils                            }
{                                                       }
{                                                       }
{       Copyright (C) 2011 Linas Naginionis             }
{       Programmer: Linas Naginionis                    }
{       lnaginionis@gmail.com                           }
{       http://code.google.com/p/sv-utils/              }
{                                                       }
{*******************************************************}
unit uVikButtonGroupUtils;

interface

uses
  SysUtils,
  Graphics,
  Controls,
  Types;


  function DrawHTML(const ARect: TRect; const ACanvas: TCanvas; const Text: String): Integer;
  procedure DisableRedraw(Control: TWinControl);
  procedure EnableRedraw(Control: TWinControl; InvalidateAfter: Boolean = True);


implementation

uses
  Windows,
  Messages;



function DrawHTML(const ARect: TRect; const ACanvas: TCanvas; const Text: String): Integer;
(*DrawHTML - Draws text on a canvas using tags based on a simple subset of HTML/CSS

  <B> - Bold e.g. <B>This is bold</B>
  <I> - Italic e.g. <I>This is italic</I>
  <U> - Underline e.g. <U>This is underlined</U>
  <S> - Strikeout e.g. <U>This is strikeout</S>
  <font-color=x> Font colour e.g.
                <font-color=clRed>Delphi red</font-color>
                <font-color=#FFFFFF>Web white</font-color>
                <font-color=$000000>Hex black</font-color>
  <font-size=x> Font size e.g. <font-size=30>This is some big text</font-size>
  <font-family> Font family e.g. <font-family=Arial>This is arial</font-family>*)

  function CloseTag(const ATag: String): String;
  begin
    Result := '/' + ATag;
  end;

  function GetTagValue(const ATag: String): String;
  var
    p: Integer;
  begin
    p := pos('=', ATag);

    if p = 0 then
      Result := ''
    else
      Result := copy(ATag, p + 1, MaxInt);
  end;

  function ColorCodeToColor(const Value: String): TColor;
  var
    HexValue: String;
  begin
    Result := 0;

    if Value <> '' then
    begin
      if (length(Value) >= 2) and (copy(Uppercase(Value), 1, 2) = 'CL') then
      begin
        // Delphi colour
        Result := StringToColor(Value);
      end else
      if Value[1] = '#' then
      begin
        // Web colour
        HexValue := copy(Value, 2, 6);

        Result := RGB(StrToInt('$'+Copy(HexValue, 1, 2)),
                      StrToInt('$'+Copy(HexValue, 3, 2)),
                      StrToInt('$'+Copy(HexValue, 5, 2)));
      end
      else
        // Hex or decimal colour
        Result := StrToIntDef(Value, 0);
    end;
  end;

const
  TagBold = 'B';
  TagItalic = 'I';
  TagUnderline = 'U';
  TagStrike = 'S';
  TagBreak = 'BR';
  TagFontSize = 'FONT-SIZE';
  TagFontFamily = 'FONT-FAMILY';
  TagFontColour = 'FONT-COLOR';
  TagColour = 'COLOUR';

var
  x, y, idx, CharWidth, MaxCharHeight: Integer;
  CurrChar: Char;
  Tag, TagValue: String;
  PreviousFontColour: TColor;
  PreviousFontFamily: String;
  PreviousFontSize: Integer;
  PreviousColour: TColor;

begin
{  ACanvas.Font.Size := Canvas.Font.Size;
  ACanvas.Font.Name := Canvas.Font.Name;
  ACanvas.Font.Color := Canvas.Font.Color;
  ACanvas.Font.Style := Canvas.Font.Style;   }

  PreviousFontColour := ACanvas.Font.Color;
  PreviousFontFamily := ACanvas.Font.Name;
  PreviousFontSize := ACanvas.Font.Size;
  PreviousColour := ACanvas.Brush.Color;

 // y := ARect.Top + 1;
  idx := 1;

  MaxCharHeight := ACanvas.TextHeight('Ag');

  x := ARect.Left;
  y := ARect.Top;
 // y := ((ARect.Bottom - ARect.Top) div 2) - (MaxCharHeight div 2);    //center vertically

  While idx <= length(Text) do
  begin
    CurrChar := Text[idx];

    // Is this a tag?
    if CurrChar = '<' then
    begin
      Tag := '';

      inc(idx);

      // Find the end of then tag
      while (Text[idx] <> '>') and (idx <= length(Text)) do
      begin
        Tag := Tag +  UpperCase(Text[idx]);

        inc(idx);
      end;

      ///////////////////////////////////////////////////
      // Simple tags
      ///////////////////////////////////////////////////
      if Tag = TagBold then
        ACanvas.Font.Style := ACanvas.Font.Style + [fsBold] else

      if Tag = TagItalic then
        ACanvas.Font.Style := ACanvas.Font.Style + [fsItalic] else

      if Tag = TagUnderline then
        ACanvas.Font.Style := ACanvas.Font.Style + [fsUnderline] else

      if Tag = TagStrike then
        ACanvas.Font.Style := ACanvas.Font.Style + [fsStrikeOut] else

      if Tag = TagBreak then
      begin
        x := ARect.Left;

        inc(y, MaxCharHeight);
      end else

      ///////////////////////////////////////////////////
      // Closing tags
      ///////////////////////////////////////////////////
      if Tag = CloseTag(TagBold) then
        ACanvas.Font.Style := ACanvas.Font.Style - [fsBold] else

      if Tag = CloseTag(TagItalic) then
        ACanvas.Font.Style := ACanvas.Font.Style - [fsItalic] else

      if Tag = CloseTag(TagUnderline) then
        ACanvas.Font.Style := ACanvas.Font.Style - [fsUnderline] else

      if Tag = CloseTag(TagStrike) then
        ACanvas.Font.Style := ACanvas.Font.Style - [fsStrikeOut] else

      if Tag = CloseTag(TagFontSize) then
        ACanvas.Font.Size := PreviousFontSize else

      if Tag = CloseTag(TagFontFamily) then
        ACanvas.Font.Name := PreviousFontFamily else

      if Tag = CloseTag(TagFontColour) then
        ACanvas.Font.Color := PreviousFontColour else

      if Tag = CloseTag(TagColour) then
        ACanvas.Brush.Color := PreviousColour else

      ///////////////////////////////////////////////////
      // Tags with values
      ///////////////////////////////////////////////////
      begin
        // Get the tag value (everything after '=')
        TagValue := GetTagValue(Tag);

        if TagValue <> '' then
        begin
          // Remove the value from the tag
          Tag := copy(Tag, 1, pos('=', Tag) - 1);

          if Tag = TagFontSize then
          begin
            PreviousFontSize := ACanvas.Font.Size;
            ACanvas.Font.Size := StrToIntDef(TagValue, ACanvas.Font.Size);
          end else

          if Tag = TagFontFamily then
          begin
            PreviousFontFamily := ACanvas.Font.Name;
            ACanvas.Font.Name := TagValue;
          end;

          if Tag = TagFontColour then
          begin
            PreviousFontColour := ACanvas.Font.Color;

            try
              ACanvas.Font.Color := ColorCodeToColor(TagValue);
            except
              //Just in case the canvas colour is invalid
            end;
          end else

          if Tag = TagColour then
          begin
            PreviousColour := ACanvas.Brush.Color;

            try
              ACanvas.Brush.Color := ColorCodeToColor(TagValue);
            except
              //Just in case the canvas colour is invalid
            end;
          end;
        end;
      end;
    end
    else
    // Draw the character if it's not a ctrl char
    if CurrChar >= #32 then
    begin
      CharWidth := ACanvas.TextWidth(CurrChar);

      if x + CharWidth > ARect.Right then
      begin
        x := ARect.Left;

        inc(y, MaxCharHeight);
      end;

      if y + MaxCharHeight < ARect.Bottom then
      begin
        ACanvas.Brush.Style := bsClear;

        ACanvas.TextOut(x, y, CurrChar);
      end;

      x := x + CharWidth;
    end;

    inc(idx);
  end;

  Result := x;
end;

procedure DisableRedraw(Control: TWinControl);
begin
  SendMessage(Control.Handle, WM_SETREDRAW, WPARAM(False), 0);
end;

procedure EnableRedraw(Control: TWinControl; InvalidateAfter: Boolean = True);
begin
  SendMessage(Control.Handle, WM_SETREDRAW, WPARAM(True), 0);
  if InvalidateAfter then
    Control.Invalidate;
end;

end.
