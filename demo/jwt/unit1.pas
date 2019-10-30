unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, strutils, dateutils;

type

  { TForm1 }

  TForm1 = class(TForm)
    gen_token_button: TButton;
    val_token_button1: TButton;
    payload_input: TMemo;
    token_input: TMemo;
    payload_label: TLabel;
    token_label: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure gen_token_buttonClick(Sender: TObject);
    procedure val_token_button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  fpjson,
  rsql_crypto_jwt;

const
  CSECRET_KEY = 'mypassword';

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  J: TJSONObject;
begin
  J := TJSONObject.Create();
  try
    J.Add('name', 'adm');   
    J.Add('age', 21);    
    J.Add('exp', IncDay(Now, 1));

    payload_input.Text := J.FormatJSON();

  finally
    J.Free;
  end;
end;

procedure TForm1.gen_token_buttonClick(Sender: TObject);
var                
  VPayload: string;
  VToken: string;
begin              
  VPayload := payload_input.Text;
  VToken := JWTSign(CSECRET_KEY, VPayload);
  token_input.Text := VToken;
end;

procedure TForm1.val_token_button1Click(Sender: TObject);
var
  VToken: string;
  VMsg: string;
begin
  VToken := token_input.Text;
  if (JWTParse(VToken, CSECRET_KEY, VMsg)) then
  begin
    /// OK
    ShowMessage(VMsg);
  end
  else
  begin
    /// ERROR
    ShowMessage(VMsg);
  end;
end;

end.
