unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TForm2 }

  TForm2 = class(TForm)
    Image1: TImage;
    procedure FormShow(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
  private

  public

  end;

var
  Form2: TForm2;

implementation
// чтобы не было зацикленности именно после implementation,
// т.к. Unit1 также подключает Unit2
Uses Unit1, Jobs;

var Xn, Yn, Xk, Yk: integer; // Область Image1 выделенная под рисование графика
    Ymin, Ymax: real; // Минимуы максимумы функций
    mx, my: real; // Коэффициенты для отображения графика на ВСЁМ Image1

{$R *.lfm}

{ TForm2 }

// Процедура поиска минимального и максимального ЗНАЧЕНИЯ функции
// Func - появилась из первого модуля Unit1
// Arfx - получен процедурой FotX(Модуль Jobs.pas) - распределение игреков на нужном нам интервале
procedure PosMinMax(Arfx: TArfx; var Ymin: real; var Ymax: real);
var i: integer;
    Y: real; // Чтобы было более понятно что такое Arfx[i]
begin
  Ymin:= Arfx[0];
  Ymax:=  Arfx[0];
  for i:=0 to high(Arfx) do // Перебираем все игреки
    begin
      Y:= Arfx[i];
      if Y < Ymin then Ymin:= Y
      else if Y > Ymax then Ymax:= Y;
    end;
end;


procedure TForm2.FormShow(Sender: TObject);
var MasPoints : array of TPoint;
    i: integer;
    x :real; // Для перебора Иксов
    Ndx, Ndy: integer;  // Для координатной сетки
    Xg, Yg: integer;
    DXg, DYg: integer;
    Xf, Yf: real;
    dx, dy: real;
    sx, sy: string;
begin

  // Cтираем всё что было на холсте
  Image1.Canvas.Brush.Color:= clWhite;
  Image1.Canvas.FillRect(Rect(0, 0, Image1.Width, Image1.Height));

  // 1) Задаёмся размерами области графика
  Xn:= 50;
  Yn:= 20;
  Xk:= Image1.Width - 30;
  Yk:= Image1.Height - 30;

  // 2) Определение Ymin, Ymax
  setlength(Arfx, n + 1);
  // Было
  // FotX(Func, a, b, n, Arfx); // Получаем распределение y - игреков
  // Заменяем на
  // Получаем распределение точек графика интеграла
  Integ(Func, a, b, n, Arfx);
  PosMinMax(Arfx, Ymin, Ymax);

  // 3) Операция масштабирования - вычисление масштабных коэффициентов
  mx:= (Xk - Xn)/(b-a);
  my:= (Yk - Yn)/(Ymax - Ymin);

  // 4) Вычисление значений координат расчётных точек в графической системе координат
  setlength(MasPoints, n+1); // Задаём размер массива точек
  dx:= (b-a)/n;             // Находим Шаг
  // Заполняем этот массив точками графика
  for i:= 0 to high(MasPoints) do
    begin
      x:= a + i*dx; //вычисляем нужную точку
      // Переходим к графическим координатам
      MasPoints[i].x:= Xn + Round(mx*(x-a));
      MasPoints[i].y:= Yn + Round(my*(Ymax-Arfx[i])); // нет смысла использовать Func(x) так как Игреки(Y) уже были посчитаны на 2)-ом шаге
    end;

  // 5) Нанесение координатной сетки оцифровка осей
  // В реальных координатах
  Ndx:= 10; // Число интервалов между линиями по x
  Ndy:= 10; // Число интервалов между линиями по Y

  // Шаги по x и y в ГРАФИЧЕСКИХ координатах
  DXg:= (Xk - Xn) div Ndx;
  DYg:= (Yk - Yn) div Ndy;
  // Шаги по x и y в РЕАЛЬНЫХ координатах
  dx:= (b-a)/Ndx;
  dy:= (Ymax-Ymin)/Ndy;

  {Делаем полосы параллельные OY}

  // Полосы должны быть чёрного цвета
  Image1.Canvas.Pen.Color:= clBlack;
  Image1.canvas.Pen.Width:=1; // задаём жирность линии
  for i:= 0 to Ndx do
  begin
    // Рисуем одну вертикальную линию
    Xg := Xn + i * DXg;
    Image1.Canvas.MoveTo(Xg, Yn);
    Image1.Canvas.LineTo(Xg, Yk);

    // Делаем подпись для неё
    Xf:= a + i*dx;
    str(Xf:5:2,sx);
    Image1.Canvas.TextOut(Xg - Image1.Canvas.TextWidth(sx) div 2, yk + 10, sx);
  end;

  {Делаем полосы параллельные OX}

  // Полосы должны быть чёрного цвета
  Image1.Canvas.Pen.Color:= clBlack;
  Image1.canvas.Pen.Width:=1; // задаём размер линии - жирность
  for i:= 0 to Ndy do
  begin
    // Рисуем одну горизонтальную линию
    Yg := Yk - i * DYg;
    Image1.Canvas.MoveTo(Xn, Yg);
    Image1.Canvas.LineTo(Xk, Yg);

    // Делаем подпись для неё
    Yf:= Ymin +  i*dy;
    str(Yf:5:2,sy);
    Image1.Canvas.TextOut(Xn - (Image1.Canvas.TextWidth(sy) + 10) ,Yg - Image1.Canvas.TextHeight(sy) div 2, sy);

  end;

  // Рисуем График
  Image1.Canvas.Pen.Color:= clBlue; // Рисуем его синим цветом шоб выделялся
  Image1.canvas.Pen.Width:=2; // Сделаем его жирнее сетки
  Image1.Canvas.Polyline(MasPoints);

end;

{Координаты под курсором}
// Вызывается когда мы останавливаем где-нибудь курсор
// Служит для того чтобы показать координаты точки, на которую указывает курсор
procedure TForm2.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var Xf, Yf: real;
    sx, sy: string;
begin
  // Возвращаемся от графических координат к реальным
  Xf:= a + (X - Xn)/mx;
  Yf:= Ymax - (Y - Yn)/my;

  // Переводим всё в строки
  str(Xf:5:2, sx);
  str(Yf:5:2, sy);

  // То что будет отображаться около курсора
  // #13 - переход на другую строку
  Hint:= 'x='+sx + #13 +'y='+ sy;
end;


end.

