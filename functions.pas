// Модуль описания исследуемых функций
unit Functions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math;

Type
  TFunc = Function(x:real):real;

var ArNamef: array[0..2] of string; // Массив ИМЁН функций
    Arf: array [0..2] of TFunc;     // Массив САМИХ функций

// Прототипы функций
Function F1(x:real):real;
Function F2(x:real):real;
Function F3(x:real):real;

implementation

Function F1(x:real):real;
begin
  F1 := sin(x);
end;

Function F2(x:real):real;
begin
  F2 := exp(-x)*cos(x);
end;

Function F3(x:real):real;
begin
  F3 := x*(x-1)*(x-2);
end;


begin
  ArNamef[0]:= 'sin(x)';
  ArNamef[1]:= 'exp(-x)*cos(x)';
  ArNamef[2]:= 'x*(x-1)*(x-2)';
  Arf[0]:= @F1;
  Arf[1]:= @F2;
  Arf[2]:= @F3;
end.

