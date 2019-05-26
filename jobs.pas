// Модуль обработки функций
unit Jobs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Functions;

Type
  TArfx = array of real;
  TInteg = procedure(f:Tfunc; a,b:real; n:integer; var Arfx:TArfx);

// Заголовки функций, которые будут использоваться в других модулях
procedure Fotx(f:Tfunc; a,b:real; n:integer; var Arfx:TArfx);
procedure Df_dx(f:Tfunc; a,b:real; n:integer; var Arfx:TArfx);
procedure D2f_dx(f:Tfunc; a,b:real; n:integer; var Arfx:TArfx);

procedure Roots(f:Tfunc; a,b:real; n:integer; var Arfx:TArfx; var k:integer);
procedure Extrems(f:Tfunc; a,b:real; n:integer; var Arfx:TArfx; var k:integer);

// Массив ИМЁН ПРОЦЕДУР методов интегрирования
var ArNameInteg: array[0..4] of string;
    // Массив САМИХ ПРОЦЕДУР интегрирования
    ArInteg: array [0..4] of TInteg;

implementation

{Эф от икс F(x)}
// Значения переданной функции 'f' на интервале [a,b] записываются в массив arfx
procedure Fotx(f:Tfunc; a,b:real; n:integer; var Arfx:TArfx);
var i: integer;
    x, dx: real;
begin
  dx:= (b-a)/n;
  for i:= 0 to n do
  begin
    x:= a + i*dx;
    Arfx[i]:=f(x);
  end;
end;

{1-ая производна от функции}
procedure Df_dx(f:Tfunc; a,b:real; n:integer; var Arfx:TArfx);
var i:integer;
    x, dx: real;
begin
  dx:= (b-a)/n; // шаг

  Arfx[0]:= (f(a+dx) - f(a))/dx;
  for i:= 1 to n-1 do
  begin
    x:= a + i*dx;
    Arfx[i]:= (f(x+dx) - f(x-dx))/(2*dx);
  end;
  Arfx[n]:= (f(b) - f(b-dx))/dx;

end;

{2-ая производна от функции}
procedure D2f_dx(f:Tfunc; a,b:real; n:integer; var Arfx:TArfx);
var i:integer;
    dx: real;
    rfx: TArfx;
begin
  dx:= (b-a)/n; // шаг
  SetLength(rfx, n+1); // устанавливаем длину массива для 1-ой производной
  Df_dx(f, a, b, n, rfx); // вычисляем 1 -ую производную и записываем её в 'rfx'

  // Вычисляем 2-ую производную
  Arfx[0]:= (rfx[1] - rfx[0])/dx;
  Arfx[n]:= (rfx[n] - rfx[n-1])/dx;
  for i:= 1 to n-1 do
    Arfx[i]:= (rfx[i+1] - rfx[i-1])/(2*dx);

  // очищаем память, т.к. rfx больше не нужен,
  // а хранить его значения очень затратно по ресурсам
  SetLength(rfx, 0);
end;


{Корни функции}
procedure Roots(f:Tfunc; a,b:real; n:integer; var Arfx:TArfx; var k:integer);
var x, dx: real;
    i, i_last: integer;
    before, after: real;
begin
  dx:= (b-a)/n; // шаг
  k:= 0; // Количество элементов в массиве получаемых корней

  // Прверяем все значения на интервале [а,b]
  i:=0; // индекс перебираемого значения
  i_last:= i; // позиция ПОСЛЕДНЕГО НАЙДЕННОГО корня
  while i<=n do
  begin
    x:= a + i*dx; // Вычисляем наше текущее положение по x
    before:= f(x-dx); // Значение функции ДО предполагаемого корня
    after:= f(x+dx);  // Значение функции ПОСЛЕ возможного корня
    // Если знаки функции до и после x различны или f(x)= 0 то это КОРЕНЬ
    if ( (before*after) < 0 ) or (f(x) = 0) then
      // Проверка на повтор корней
      // То есть если разница между предыдущим корнем и текущим равна один шаг
      if (k<> 0) and(i-i_last=1) then Arfx[k-1]:= x // просто перезаписываем корень
      else // иначе добавляем новый корень
      begin
        i_last:= i; // сохраняем позицию ПОСЛЕДНЕГО НАЙДЕННОГО корня
        k := k+1; // Увеличиваем число элементов в получаемом массиве корней
        setlength(Arfx, k); // Задаём размер массива, т.к. k- это просто счётчик
        Arfx[k-1]:= x; // Записываем в массив корней X - корень
      end;
    i :=i+1;
  end;

  // Если корни есть или вначале или в конце интервала, то они могут неправильно посчитаться
  // Пример: если убрать эти строчки ниже и попытаться найти корни для функции x*(x-1)*(x-2)
  // на отрезке [-10,0] ответ будет неправильным
  // Отдельно проверяем корень в точке 'a' НАЧАЛО отрезка
  if f(a) = 0 then
  begin
    Arfx[0]:= a; // Перезаписываем поверх старого
  end;
  // Отдельно проверяем корень в точке 'b' КОНЕЦ отрезка
  if f(b) = 0 then
  begin
    Arfx[k-1]:= b; // Перезаписываем поверх старого
  end;

end;


{Экстремумы функции}
procedure Extrems(f:Tfunc; a,b:real; n:integer; var Arfx:TArfx; var k:integer);
var x, before, after, dx: real;
    i: integer;
begin
  dx := (b-a)/n; // Шаг
  before:= f(a-dx); // Началье значение ДО предполагаемого экстремума
  k:=0;   // Начальное количество элементов в массиве экстремумов (Минимумов и Максимумов)
  for i:= 0 to n do
  begin
    x:= a + i*dx; // текущий x
    after:= f(x + dx); // Значение функци ПОСЛЕ возможного экстремума
    // Условие вытекает из определения экстремума(Без производной)
    if (f(x) > after) and (f(x) > before) or (f(x) < after) and (f(x) < before) then
    begin
      k := k+1; // Увеличиваем счётчик элементов в массиве экстремумов
      setlength(Arfx, k); // Меняем размер массива
      Arfx[k-1] := x; // Здесь же просто записываем значение x экстремума
    end;
    before:= f(x);

  end;
end;


{Интеграл от функции}
// Здесь интересный факт, интеграл может уходит в ноль и быть отрицательный,
// Т.к. мы здесь ищем не площадь а просто высчитываем определённый интеграл
// Хороший сайт http://www.cleverstudents.ru/integral/method_of_rectangles.html

// Важно помнить
// n - число прямугольников, на которые мы разбиваем площадь под графиком функции
// n+1 - Число точек, которые образуют основания прямоугольников
// Но мы должны записать значение интеграла в каждую точку из данного отрезка [a,b]
// Спасает точка 'а'
// ВСЕГДА интеграл от 'а' до 'а' (integ(f, a, a)) равен 0
// Поэтому всегда в точке 'a' интеграл пусть будет равн нулю

// Метод ЛЕВЫХ ПРЯМОУГОЛЬНИКОВ
procedure IntegLeftRect(f:Tfunc; a,b:real; n:integer; var Arfx:TArfx);
var x, dx, s: real; // s- это площадь маленкой трапеции с высотой dx
    i: integer;
begin
  Arfx[0]:= 0;   // Интеграл от а до а = 0
  dx := (b-a)/n; // вычисляем шаг
  s:= 0;
  for i:=0 to n-1 do
  begin
    x:= a + i*dx;
    s:= s + f(x)*dx;
    Arfx[i+1]:= s;
  end;
end;

// Метод ПРАВЫХ ПРЯМОУГОЛЬНИКОВ
procedure IntegRightRect(f:Tfunc; a,b:real; n:integer; var Arfx:TArfx);
var x, dx, s: real; // s- это площадь маленкой трапеции с высотой dx
    i: integer;
begin
  Arfx[0]:= 0;   // Интеграл от а до а = 0
  dx := (b-a)/n; // вычисляем шаг
  s:= 0;
  for i:=1 to n do
  begin
    x:= a + i*dx;
    s:= s + f(x)*dx;
    Arfx[i]:= s;
  end;

end;

// Метод СРЕДНИХ ПРЯМОУГОЛЬНИКОВ
procedure IntegAverageRect(f:Tfunc; a,b:real; n:integer; var Arfx:TArfx);
var x, dx, s: real; // s- это площадь маленкой трапеции с высотой dx
    i: integer;
begin
  Arfx[0]:= 0;   // Интеграл от а до а = 0
  dx := (b-a)/n; // вычисляем шаг
  s:= 0;
  for i:=1 to n do
  begin
    x:= a + (i-1)*dx;
    s:= s + f(x +dx/2)*dx;
    Arfx[i]:= s;
  end;

end;

// Метод ТРАПЕЦИИ
procedure IntegTrapecia(f:Tfunc; a,b:real; n:integer; var Arfx:TArfx);
var x, dx, s: real; // s- это площадь маленкой трапеции с высотой dx
    i: integer;
begin
  Arfx[0]:= 0;   // Интеграл от а до а = 0
  dx := (b-a)/n; // вычисляем шаг
  s:=0;
  for i:= 1 to n do // Последовательно перебираем x
  begin
    x:= a + i*dx; // Вычисляем текущее положение с нашим сдвигом dx
    s:= s + (f(x-dx) + f(x))*dx/2; // Вычисляем площадь трапеции с высотой dx
    arfx[i]:= s;
  end;
end;

// Метод СИМПСОНА
procedure IntegSimpson(f:Tfunc; a,b:real; n:integer; var Arfx:TArfx);
var x, dx, s: real; // s- это площадь маленкой трапеции с высотой dx
    i: integer;
begin
  Arfx[0]:= 0;   // Интеграл от а до а = 0
  dx := (b-a)/n; // вычисляем шаг
  s:=0;
  for i:= 1 to n do
  begin
    x:= a + i*dx;
    s:= s + (dx/6)*(f(x-dx) + 4*f(((x-dx)+x)/2) + f(x)); // Вычисляем площадь трапеции с высотой dx
    arfx[i]:= s;
  end;
end;

// то что будет выполняться при 'uses jobs' в модуле Unit1.pas
begin
  // Заполняем массив ИМЁН методов интегрирования
  ArNameInteg[0]:= 'Левых Прямоугольников';
  ArNameInteg[1]:= 'Правых Прямоугольников';
  ArNameInteg[2]:= 'Средних Прямоугольников';
  ArNameInteg[3]:= 'Трапеции';
  ArNameInteg[4]:= 'Симпсона';

  // Заполняем массив САМИХ МЕТОДОВ интегрирования
  ArInteg[0]:= @IntegLeftRect;
  ArInteg[1]:= @IntegRightRect;
  ArInteg[2]:= @IntegAverageRect;
  ArInteg[3]:= @IntegTrapecia;
  ArInteg[4]:= @IntegSimpson;
end.

end.

